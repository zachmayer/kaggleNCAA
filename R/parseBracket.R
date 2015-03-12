#' @title Parse a Kaggle-formatted bracket
#'
#' @description Inputs a bracket in kaggle file format and outputs a data.table
#'
#' @details Reads the csv using data.table::fread, then uses splitstr to parse.
#' problems
#' @note Will only have 1 row per game, where team_1 is the lower id team
#' @param f Path to the Kaggle-formatted bracket csv
#' @return a data.table
#' @importFrom data.table fread setcolorder setkeyv :=
#' @export
parseBracket <- function(f){
  dat <- fread(f)
  dat[, id := strsplit(dat$id, '_')]
  dat[,season := as.integer(sapply(id, '[', 1))]
  dat[,team_1 := as.integer(sapply(id, '[', 2))]
  dat[,team_2 := as.integer(sapply(id, '[', 3))]
  dat[, id := NULL]
  setcolorder(dat, c('season', 'team_1', 'team_2', 'pred'))
  setkeyv(dat, c('season', 'team_1', 'team_2'))
  return(dat)
}
