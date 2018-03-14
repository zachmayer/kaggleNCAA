# TODO: Add function to give top 10 champtions + probs, given a simulation

#' @title Simulate the NCAA tournament
#'
#' @description Given a parsed NCAA bracket, simulate the tournament
#'
#' @details Runs N simulations of the NCAA tournament
#' @note Can be a little slow
#' @param preds Predicted outcomes for ALL possible matchups
#' @param N number of simulations
#' @param year The year of the tournament.  Used to subset preds, if preds
#' contain multiple seasons
#' @param progress If TRUE, a progress bar will be printed
#' @param upset_bias If you want more upsets in your bracket, a little upset
#' bias will give close games to the underdog.
#' @param w Women's or Men's bracket.  1 for women, 0 for men.  If NULL, will be infered from the first row of data.
#' @param parallel if TRUE, run the simulations in parallel
#' @return a data.table
#' @importFrom data.table :=
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @export
simTourney <- function(
  preds, N=1000, year=2018, progress=TRUE, upset_bias=0, w=NULL, parallel=FALSE){
  utils::data('all_slots', package='kaggleNCAA', envir=environment())

  #Checks
  if(progress & parallel){
    stop("Can't use a progress bar in parallel.  Please set either progress or parallel to FALSE")
  }

  #Subset the data
  preds <- preds[season==year,]

  #Decide men or women
  if(is.null(w)){
    w <- preds[1,women]
    message(paste('assuming women =', w))
  }
  stopifnot(w==0 | w == 1)

  #Join slots to the predictions
  n1 <- nrow(preds)
  preds <- merge(preds, all_slots, by=c('season', 'teamid_1', 'teamid_2', 'women'))
  stopifnot(n1 == nrow(preds))

  #Determine seeds
  preds[, seed_1_int := as.integer(substr(seed_1, 2, 3))]
  preds[, seed_2_int := as.integer(substr(seed_2, 2, 3))]

  #Add some columns for tracking the simulation
  preds[, rand := stats::runif(.N),]
  preds[, winner := ifelse(pred > rand, teamid_1, teamid_2)]
  preds[, keep := 1L]

  #Add upset bias
  if(upset_bias!=0){
    preds[seed_1_int > seed_2_int, rand := rand - upset_bias]
    preds[seed_1_int < seed_2_int, rand := rand + upset_bias]
  }

  #Decide on progress bars
  if(progress){
    apply_fun <- pbapply::pblapply
  } else if(parallel){
    apply_fun <- function(idx, fun, ...){
      foreach(
        i=idx,
        packages='kaggleNCAA'
        ) %dopar% fun(i, ...)
    }
  } else{
    apply_fun <- lapply
  }

  #Run the simulation
  sims_list <- apply_fun(1:N, function(x) {
    preds[, rand := stats::runif(.N),]
    if(upset_bias!=0){
      preds[seed_1_int > seed_2_int, rand := rand - upset_bias]
      preds[seed_1_int < seed_2_int, rand := rand + upset_bias]
    }
    preds[, winner := ifelse(pred > rand, teamid_1, teamid_2)]
    sim_tourney_internal(preds)
    })

  #Aggregate results
  sims <- data.table::rbindlist(sims_list)
  data.table::setkeyv(sims, c('women', 'slot', 'winner'))
  sims <- sims[,list(count=.N), by=c('women', 'slot', 'winner')]
  sims[, count := count + stats::runif(.N)/1e12]

  #Add omitted zeros
  all_possible_slots_teamid_1 <- all_slots[season==year, list(slot, winner=teamid_1)]
  all_possible_slots_teamid_2 <- all_slots[season==year, list(slot, winner=teamid_2)]
  all_possible_slots <- unique(rbind(all_possible_slots_teamid_1, all_possible_slots_teamid_2))
  sims <- merge(all_possible_slots, sims, all.x=TRUE, by=c('slot', 'winner'))
  sims[,never_occured := as.integer(is.na(count))]
  sims[is.na(count), count := 0L] # Never occured in the simulation
  sims[is.na(women), women := w] # Never occured in the simulation
  sims[, prob := count / N]
  sims <- sims[order(count, decreasing=TRUE)]

  #Check "convergence"
  slots_simulated <- sims[,sum(never_occured)/.N]
  if(slots_simulated > .80){
    msg <- paste0(
      round(slots_simulated, 3) * 100,
      '% of possible games were not seen in the simulation.  Consider higher N')
    message(msg)
  }

  #Add year and return
  sims[, season := year]
  return(sims)
}

#' @title "Walk" an NCAA tournament, from round 0 to round 6
#'
#' @description Given a parsed NCAA bracket, walk forward through each round
#' to generate a bracket
#'
#' @details If your probabilities are transative, there's no reason to
#' simulate the tournament.  You can just walk forward to generate a bracket
#' @note Much faster than simulation
#' @param preds Predicted outcomes for ALL possible matchups
#' @param year The year of the tournament.  Used to subset preds, if preds
#' contain multiple seasons
#' @param upset_bias If you want more upsets in your bracket, a little upset
#' bias will give close games to the underdog.
#' @return a data.table
#' @importFrom data.table :=
#' @export
walkTourney <- function(preds, year=2018, upset_bias=0){
  utils::data('all_slots', package='kaggleNCAA', envir=environment())

  #Subset the data
  preds <- preds[season==year,]

  #Join slots to the predictions
  n1 <- nrow(preds)
  preds <- merge(preds, all_slots, by=c('season', 'teamid_1', 'teamid_2', 'women'))
  stopifnot(n1 == nrow(preds))

  #Determine seeds
  preds[, seed_1_int := as.integer(substr(seed_1, 2, 3))]
  preds[, seed_2_int := as.integer(substr(seed_2, 2, 3))]

  #Add upset bias
  preds[, rand := .5]
  if(upset_bias!=0){
    preds[seed_1_int > seed_2_int, rand := rand - upset_bias]
    preds[seed_1_int < seed_2_int, rand := rand + upset_bias]
  }

  #Randomly break ties
  small_num <- 1e-6
  preds[, pred := pred + stats::runif(.N, min = -1 * small_num, max = small_num)]

  #Decide a winner
  preds[, winner := ifelse(pred > rand, teamid_1, teamid_2)]

  #Run the simulation
  preds[, keep := 1L]
  sims <- sim_tourney_internal(preds)

  #Add each game's probs
  sims <- merge(sims, preds[,list(teamid_1, teamid_2, pred)], by=c('teamid_1', 'teamid_2'), all.x=TRUE)
  sims[,pred := ifelse(teamid_1 == winner, pred, 1-pred)]

  #Aggregate results
  data.table::setkeyv(sims, c('slot', 'winner'))
  sims[, count := 1]
  data.table::setkeyv(sims, c('winner', 'round'))
  sims[, prob := cumprod(pred), by=c('winner')]

  #Add year and return
  sims[, season := year]
  data.table::setkeyv(sims, c('slot', 'winner'))
  return(sims)
}

#' @title Function to do a single tournament simulation
#'
#' @description Chooses a random number.  If pred > r, the lower id team wins.
#'
#' @details Internal simulation function
#' @param preds the tournament data to use for the simulation
#' @return a data.table
#' @export
#'
sim_tourney_internal <- function(preds){

  all_rounds <- sort(unique(preds$round))

  #Evaluate the playin rounds
  if(all_rounds[1] == 0){
    r <- 0L
    round_teamid_1 <- preds[round == r, list(slot=next_slot, teamid_1=winner, keep_teamid_1=1L)]
    round_teamid_2 <- preds[round == r ,list(slot=next_slot, teamid_2=winner, keep_teamid_2=1L)]
    preds <- merge(preds, round_teamid_1, by=c('slot', 'teamid_1'), all.x=TRUE)
    preds <- merge(preds, round_teamid_2, by=c('slot', 'teamid_2'), all.x=TRUE)
    preds[is.na(keep_teamid_1) & teamid_1_playedin == (r + 1L) & round == 1L, keep := 0L]
    preds[is.na(keep_teamid_2) & teamid_2_playedin == (r + 1L) & round == 1L, keep := 0L]
    preds <- preds[keep==1L,]
    preds[, c('keep_teamid_1', 'keep_teamid_2') := NULL]
    all_rounds <- all_rounds[2:length(all_rounds)]
  }

  #Evaluate the regular rounds
  for(r in 1:5){
    round_teamid_1 <- preds[round == r, list(slot=next_slot, teamid_1=winner, keep_teamid_1=1L)]
    round_teamid_2 <- preds[round == r ,list(slot=next_slot, teamid_2=winner, keep_teamid_2=1L)]
    preds <- merge(preds, round_teamid_1, by=c('slot', 'teamid_1'), all.x=TRUE)
    preds <- merge(preds, round_teamid_2, by=c('slot', 'teamid_2'), all.x=TRUE)
    preds[is.na(keep_teamid_1) & round == (r + 1L), keep := 0L]
    preds[is.na(keep_teamid_2) & round == (r + 1L), keep := 0L]
    preds <- preds[keep==1L,]
    preds[, c('keep_teamid_1', 'keep_teamid_2') := NULL]
  }
  preds <- preds[,list(slot, round, teamid_1, teamid_2, women, winner)]
  data.table::setkeyv(preds, 'slot')

  return(preds)
}
