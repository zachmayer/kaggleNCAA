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
#' @param seed_diff_upset_multiple How much to multiply the upset bias by, based on the seed difference.
#' @return a data.table
#' @importFrom data.table := rbindlist
#' @importFrom pbapply pblapply
#' @export
simTourney <- function(preds, N=1000, year=2015, progress=TRUE, upset_bias=0, seed_diff_upset_multiple=0){
  data('all_slots', package='kaggleNCAA', envir=environment())

  #Subset the data
  preds <- preds[season==year,]

  #Join slots to the predictions
  n1 <- nrow(preds)
  preds <- merge(preds, all_slots, by=c('season', 'team_1', 'team_2'))
  stopifnot(n1 == nrow(preds))

  #Determine seeds
  preds[, seed_1_int := as.integer(substr(seed_1, 2, 3))]
  preds[, seed_2_int := as.integer(substr(seed_2, 2, 3))]
  preds[, seed_diff := abs(seed_1_int - seed_2_int)]

  #Add some columns for tracking the simulation
  preds[, rand := runif(.N),]
  preds[, winner := ifelse(pred > rand, team_1, team_2)]
  preds[, keep := 1L]

  #Add upset bias
  if(upset_bias!=0){
    preds[seed_1_int > seed_2_int, rand := rand - upset_bias * (1 + seed_diff_upset_multiple * seed_diff)]
    preds[seed_1_int < seed_2_int, rand := rand + upset_bias * (1 + seed_diff_upset_multiple * seed_diff)]
  }

  #Decide on progress bars
  if(progress){
    apply_fun <- pblapply
  } else{
    apply_fun <- lapply
  }

  #Run the simulation
  sims_list <- apply_fun(1:N, function(x) {
    preds[, rand := runif(.N),]
    if(upset_bias!=0){
      preds[seed_1_int > seed_2_int, rand := rand - upset_bias * (1 + seed_diff_upset_multiple * seed_diff)]
      preds[seed_1_int < seed_2_int, rand := rand + upset_bias * (1 + seed_diff_upset_multiple * seed_diff)]
    }
    preds[, winner := ifelse(pred > rand, team_1, team_2)]
    sim_tourney_internal(preds)
    })

  #Aggregate results
  sims <- rbindlist(sims_list)
  setkeyv(sims, c('slot', 'winner'))
  sims <- sims[,list(count=.N), by=c('slot', 'winner')]
  sims[, count := count + runif(.N)/1e12]

  #Add omitted zeros
  all_possible_slots_team_1 <- all_slots[season==year, list(slot, winner=team_1)]
  all_possible_slots_team_2 <- all_slots[season==year, list(slot, winner=team_2)]
  all_possible_slots <- unique(rbind(all_possible_slots_team_1, all_possible_slots_team_2))
  sims <- merge(all_possible_slots, sims, all.x=TRUE, by=c('slot', 'winner'))
  sims[is.na(count), count := 0L]
  sims[, prob := count / N]
  sims <- sims[order(count, decreasing=TRUE)]

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
#' @param seed_diff_upset_multiple How much to multiply the upset bias by, based on the seed difference.
#' @return a data.table
#' @importFrom data.table := setkeyv
#' @export
walkTourney <- function(preds, year=2015, upset_bias=0, seed_diff_upset_multiple=0){
  data('all_slots', package='kaggleNCAA', envir=environment())

  #Subset the data
  preds <- preds[season==year,]

  #Join slots to the predictions
  n1 <- nrow(preds)
  preds <- merge(preds, all_slots, by=c('season', 'team_1', 'team_2'))
  stopifnot(n1 == nrow(preds))

  #Determine seeds
  preds[, seed_1_int := as.integer(substr(seed_1, 2, 3))]
  preds[, seed_2_int := as.integer(substr(seed_2, 2, 3))]
  preds[, seed_diff := abs(seed_1_int - seed_2_int)]

  #Add upset bias
  preds[, rand := .5]
  if(upset_bias!=0){
    preds[seed_1_int > seed_2_int, rand := rand - upset_bias * (1 + seed_diff_upset_multiple * seed_diff)]
    preds[seed_1_int < seed_2_int, rand := rand + upset_bias * (1 + seed_diff_upset_multiple * seed_diff)]
  }

  #Randomly break ties
  small_num <- 1e-6
  preds[, pred := pred + runif(.N, min = -1 * small_num, max = small_num)]

  #Decide a winner
  preds[, winner := ifelse(pred > rand, team_1, team_2)]

  #Run the simulation
  preds[, keep := 1L]
  sims <- sim_tourney_internal(preds)

  #Add each game's probs
  sims <- merge(sims, preds[,list(team_1, team_2, pred)], by=c('team_1', 'team_2'), all.x=TRUE)
  sims[,pred := ifelse(team_1 == winner, pred, 1-pred)]

  #Aggregate results
  setkeyv(sims, c('slot', 'winner'))
  sims[, count := 1]
  setkeyv(sims, c('winner', 'round'))
  sims[, prob := cumprod(pred), by=c('winner')]

  #Add year and return
  sims[, season := year]
  setkeyv(sims, c('slot', 'winner'))
  return(sims)
}

#' @title Function to do a single tournament simulation
#'
#' @description Chooses a random number.  If pred > r, the lower id team wins.
#'
#' @details Internal simulation function
#' @param preds the tournament data to use for the simulation
#' @return a data.table
#' @importFrom data.table data.table
sim_tourney_internal <- function(preds){

  all_rounds <- sort(unique(preds$round))

  #Evaluate the playin rounds
  if(all_rounds[1] == 0){
    r <- 0L
    round_team_1 <- preds[round == r, list(slot=next_slot, team_1=winner, keep_team_1=1L)]
    round_team_2 <- preds[round == r ,list(slot=next_slot, team_2=winner, keep_team_2=1L)]
    preds <- merge(preds, round_team_1, by=c('slot', 'team_1'), all.x=TRUE)
    preds <- merge(preds, round_team_2, by=c('slot', 'team_2'), all.x=TRUE)
    preds[is.na(keep_team_1) & team_1_playedin == (r + 1L) & round == 1L, keep := 0L]
    preds[is.na(keep_team_2) & team_2_playedin == (r + 1L) & round == 1L, keep := 0L]
    preds <- preds[keep==1L,]
    preds[, c('keep_team_1', 'keep_team_2') := NULL]
    all_rounds <- all_rounds[2:length(all_rounds)]
  }

  #Evaluate the regular rounds
  for(r in 1:5){
    round_team_1 <- preds[round == r, list(slot=next_slot, team_1=winner, keep_team_1=1L)]
    round_team_2 <- preds[round == r ,list(slot=next_slot, team_2=winner, keep_team_2=1L)]
    preds <- merge(preds, round_team_1, by=c('slot', 'team_1'), all.x=TRUE)
    preds <- merge(preds, round_team_2, by=c('slot', 'team_2'), all.x=TRUE)
    preds[is.na(keep_team_1) & round == (r + 1L), keep := 0L]
    preds[is.na(keep_team_2) & round == (r + 1L), keep := 0L]
    preds <- preds[keep==1L,]
    preds[, c('keep_team_1', 'keep_team_2') := NULL]
  }
  preds <- preds[,list(slot, round, team_1, team_2, winner)]
  setkeyv(preds, 'slot')

  return(preds)
}
