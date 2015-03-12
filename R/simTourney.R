#' @title Simulate the NCAA tournament
#'
#' @description Given a parsed NCAA bracket, simulate the tournament
#'
#' @details Runs N simulations of the NCAA tournament
#' @note Can be a little slow
#' @param preds Predicted outcomes for ALL possible matchups
#' @param year The year of the tournament.  Used to lookup the correct slots.
#' @return a data.table
#' @importFrom data.table data.table
#' @export
simTourney <- function(preds, year=2014){
  stop()
}
