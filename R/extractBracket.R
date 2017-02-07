#' @title Extract a best bracket from a simulation result
#'
#' @description Given the results of simTourney, this function pulls out a
#' single tournament result.
#'
#' @details This function starts with the most likely winner, and then assumes
#' they won all prior games.  It then picks the most likely other team in the
#' championship game, and assumes that team won all prior games.  As such, it
#' works backwards to determine a single result from a simulation.
#' @param sim The outcome of a simTourney run
#' @param restrict Restrict the brakcet to realistic result: if a team wins a
#' given game, they must also have won all prior games.  Set to FALSE for
#' impossible, but still very interesting brackets
#' @return a data.table
#' @importFrom data.table :=
#' @export
extractBracket <- function(sim, restrict=TRUE){

  #Make a deep copy, so we don't update the original data
  dat <- data.table::copy(sim)
  dat[, slot_int := as.integer(slot)]

  #Walk backwards from the championship and choose a single tournament outcome
  if(restrict){
    all_slots <- dat[,sort(unique(slot_int))]
    for(s in all_slots){

      keep <- dat[slot_int == s, winner[1]]
      prior_slots <- dat[winner == keep & slot_int >= s,]

      dat <- dat[winner == keep | !(slot_int %in% prior_slots$slot_int),]
    }
  } else {
    dat[, prob := prob + stats::runif(.N)/1e12]
    dat[, res := as.integer(prob == max(prob)), by='slot']
    dat <- dat[res == 1,]
    dat[, res := NULL]
  }

  return(dat)
}
