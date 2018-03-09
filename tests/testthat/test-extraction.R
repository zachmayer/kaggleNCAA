test_that("We can extract a bracket from a simulation - women", {
  f <- system.file('kaggle_data/seed_benchmark_women.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)
  year <- sort(unique(dat$season))[1]
  sim <- simTourney(dat, 1, year, progress=FALSE)
  bracket <- extractBracket(sim, restrict=TRUE)
  expect_is(bracket, 'data.table')
  expect_gt(nrow(bracket), 63)
  bracket <- extractBracket(sim, restrict=FALSE)
  expect_is(bracket, 'data.table')
  expect_gt(nrow(bracket), 63)
})

test_that("We can extract a bracket from a simulation - men", {
  f <- system.file('kaggle_data/seed_benchmark_men.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)
  year = sort(unique(dat$season))[1]
  sim <- simTourney(dat, 1, year, progress=FALSE)
  bracket <- extractBracket(sim, restrict=TRUE)
  expect_is(bracket, 'data.table')
  expect_gt(nrow(bracket), 63)
  bracket <- extractBracket(sim, restrict=FALSE)
  expect_is(bracket, 'data.table')
  expect_gt(nrow(bracket), 63)
})
