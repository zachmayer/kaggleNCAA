#' @title Parse a Kaggle-formatted bracket
#'
#' @description Inputs a bracket in kaggle file format and outputs a data.table
#'
#' @details Reads the csv using data.table::fread, then uses splitstr to parse.
#' problems
#' @note Will only have 1 row per game, where team_1 is the lower id team
#' @param f Path to the Kaggle-formatted bracket csv
#' @param w 0 for women's bracket, 1 for mens.  If NULL will be infered from the file name (e.g. "women" in name = 1, otherwise men).
#' @return a data.table
#' @importFrom data.table :=
#' @export
#' @examples
#' f <- system.file('kaggle_data/seed_benchmark_women.csv', package = "kaggleNCAA", mustWork=TRUE)
#' dat <- parseBracket(f)
#' head(dat)
parseBracket <- function(f, w=NULL){
  if(is.null(w)){
    if(grepl('women', f, fixed=T)){
      w = 1
      message("Assuming women's bracket")
    } else{
      w = 0
      message("Assuming men's bracket")
    }
  }
  stopifnot(w==0 | w == 1)
  dat <- data.table::fread(f)
  dat[,women := w]
  data.table::setnames(dat, tolower(names(dat)))
  dat[, id := strsplit(dat$id, '_')]
  dat[,season := as.integer(sapply(id, '[', 1))]
  dat[,teamid_1 := as.integer(sapply(id, '[', 2))]
  dat[,teamid_2 := as.integer(sapply(id, '[', 3))]
  dat[, id := NULL]
  data.table::setcolorder(dat, c('season', 'teamid_1', 'teamid_2', 'women', 'pred'))
  data.table::setkeyv(dat, c('season', 'teamid_1', 'teamid_2', 'women'))
  return(dat)
}
