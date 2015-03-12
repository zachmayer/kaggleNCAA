#' @title Generate a printable NCAA bracket
#'
#' @description Given the results of simTourney, this function generates
#' a pritable bracket
#'
#' @details Runs N simulations of the NCAA tournament
#' @note No guarantees the format will be correct!
#' @param tourney The outcome of a simTourney run
#' @return NULL
#' @importFrom data.table setnames
#' @export
#' @references
#' \url{http://www.kaggle.com/c/march-machine-learning-mania-2015/forums/t/12775/printable-bracket-for-r}
#' \url{http://www.kaggle.com/c/march-machine-learning-mania-2015/forums/t/12627/simulating-the-tournament}
#' \url{http://www.kaggle.com/c/march-machine-learning-mania/forums/t/7309/printable-bracket-in-r}
#' \url{https://github.com/chmullig/marchmania/blob/master/bracket.R}
printableBracket <- function(tourney){
  data('seed_print_positions', package='kaggleNCAA', envir=environment())
  data('slot_print_positions', package='kaggleNCAA', envir=environment())
  data('tourney_seeds', package='kaggleNCAA', envir=environment())
  data('teams', package='kaggleNCAA', envir=environment())

  year <- sort(unique(tourney$season))
  stopifnot(length(year)==1)

  tourney_seeds <- tourney_seeds[season == year,]

  #Add team names
  setnames(teams, 'team_id', 'team')
  setnames(tourney, 'winner', 'team')
  tourney_seeds <- merge(tourney_seeds, teams, by='team', all.x=TRUE)
  tourney <- merge(tourney, teams, by='team', all.x=TRUE)

  #Add printing positions
  tourney_seeds <- merge(tourney_seeds, seed_print_positions, by=c('seed'), all.x=TRUE)
  tourney <- merge(tourney, slot_print_positions, by=c('slot'), all.x=TRUE)

  #Setup plot
  x <- seq(0,220,(221/67))
  y <- 0:66
  plot(x,y,type="l", col.axis="white", col.lab="white", bty="n",axes=F, col="white")
  segments(0,c(seq(0,30,2),seq(34,64,2)),20,c(seq(0,30,2),seq(34,64,2)))
  segments(20,c(seq(0,28,4),seq(34,62,4)),20,c(seq(2,30,4),seq(36,64,4)))
  segments(20,c(seq(1,29,4),seq(35,63,4)),40,c(seq(1,29,4),seq(35,63,4)))
  segments(40,c(seq(1,25,8),seq(35,59,8)),40,c(seq(5,29,8),seq(39,63,8)))
  segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
  segments(60,c(3,19,37,53),60,c(11,27,45,61))
  segments(60,c(7,23,41,57),80,c(7,23,41,57))
  segments(80,c(7,41),80,c(23,57))
  segments(80,c(15,49),100,c(15,49))
  segments(100,c(27,37),120,c(27,37))
  segments(200,c(seq(0,30,2),seq(34,64,2)),220,c(seq(0,30,2),seq(34,64,2)))
  segments(200,c(seq(0,28,4),seq(34,62,4)),200,c(seq(2,30,4),seq(36,64,4)))
  segments(180,c(seq(1,29,4),seq(35,63,4)),200,c(seq(1,29,4),seq(35,63,4)))
  segments(180,c(seq(1,25,8),seq(35,59,8)),180,c(seq(5,29,8),seq(39,63,8)))
  segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
  segments(160,c(3,19,37,53),160,c(11,27,45,61))
  segments(140,c(7,23,41,57),160,c(7,23,41,57))
  segments(140,c(7,41),140,c(23,57))
  segments(120,c(15,49),140,c(15,49))

  #Print seeds, then bracket
  text(tourney_seeds$x, tourney_seeds$y, tourney_seeds$team_name,cex=.3)
  text(tourney$x, tourney$y, tourney$team_name,cex=.3)

  #Return nothing
  return(invisible())
}
