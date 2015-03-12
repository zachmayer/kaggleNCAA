
test_that("We can extract a bracket from a simulation", {
  f <- system.file('kaggle_data/sample_submission.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)
  year = sort(unique(dat$season))[1]
  sim <- simTourney(dat, 1, year, progress=FALSE)
  bracket <- extractBracket(sim)
  expect_is(bracket, 'data.table')
  expect_more_than(nrow(bracket), 63)
})
