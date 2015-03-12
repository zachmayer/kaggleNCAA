
test_that("We can print a tourney bracket", {
  f <- system.file('kaggle_data/sample_submission.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)
  year = sort(unique(dat$season))[1]
  sim <- simTourney(dat, 1, year, progress=FALSE)
  res <- printableBracket(sim)
  expect_null(res)
})
