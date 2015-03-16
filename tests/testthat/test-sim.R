
test_that("We can simulate a tourney", {
  f <- system.file('kaggle_data/sample_submission_2015.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)

  #Test all years without progress bar
  for(year in sort(unique(dat$season))){
    sim <- simTourney(dat, N=5, year=year, progress=FALSE)
    sim <- simTourney(dat, N=5, upset_bias=0.05, year=year, progress=FALSE)
    expect_more_than(nrow(sim[slot=='R6CH',]), 63)
    expect_is(sim, 'data.table')
  }

  #Test one year with progress bar
  sim <- simTourney(dat, N=5, year=year, progress=TRUE)
  expect_more_than(nrow(sim[slot=='R6CH',]), 63)
  expect_is(sim, 'data.table')
})

test_that("We can walk a tourney", {
  f <- system.file('kaggle_data/sample_submission_2015.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)

  #Test all years without progress bar
  for(year in sort(unique(dat$season))){
    sim <- walkTourney(dat, year=year)
    sim <- walkTourney(dat, year=year, upset_bias=0.05)
    expect_more_than(nrow(sim), 63)
    expect_is(sim, 'data.table')
  }

})
