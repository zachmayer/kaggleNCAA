test_that("We can simulate a tourney - women", {
  f <- system.file('kaggle_data/seed_benchmark_women.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)

  #Test all years without progress bar
  for(year in sort(unique(dat$season))){
    sim <- simTourney(dat, N=5, year=year, progress=FALSE)
    sim <- simTourney(dat, N=5, upset_bias=0.05, year=year, progress=FALSE)
    expect_gt(nrow(sim[slot=='R6CH',]), 63)
    expect_is(sim, 'data.table')
    expect_true(!anyNA(sim))
  }

  #Test one year with progress bar
  sim <- simTourney(dat, N=5, year=year, progress=TRUE)
  expect_gt(nrow(sim[slot=='R6CH',]), 63)
  expect_is(sim, 'data.table')
  expect_true(!anyNA(sim))
})

test_that("We can simulate a tourney - men", {
  f <- system.file('kaggle_data/seed_benchmark_men.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)

  #Test all years without progress bar
  for(year in sort(unique(dat$season))){
    sim <- simTourney(dat, N=5, year=year, progress=FALSE)
    sim <- simTourney(dat, N=5, upset_bias=0.05, year=year, progress=FALSE)
    expect_gt(nrow(sim[slot=='R6CH',]), 63)
    expect_is(sim, 'data.table')
    expect_true(!anyNA(sim))
  }

  #Test one year with progress bar
  sim <- simTourney(dat, N=5, year=year, progress=TRUE)
  expect_gt(nrow(sim[slot=='R6CH',]), 63)
  expect_is(sim, 'data.table')
  expect_true(!anyNA(sim))
})

test_that("We can walk a tourney - women", {
  f <- system.file('kaggle_data/seed_benchmark_women.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)

  #Test all years without progress bar
  for(year in sort(unique(dat$season))){
    sim <- walkTourney(dat, year=year)
    sim <- walkTourney(dat, year=year, upset_bias=0.05)
    expect_equal(nrow(sim), 63)
    expect_is(sim, 'data.table')
    expect_true(!anyNA(sim))
  }
})

test_that("We can walk a tourney - men", {
  f <- system.file('kaggle_data/seed_benchmark_men.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)

  #Test all years without progress bar
  for(year in sort(unique(dat$season))){
    sim <- walkTourney(dat, year=year)
    sim <- walkTourney(dat, year=year, upset_bias=0.05)
    expect_gt(nrow(sim), 63)
    expect_is(sim, 'data.table')
    expect_true(!anyNA(sim))
  }
})

test_that("We can simulate a tourney in parallel - men", {
  f <- system.file('kaggle_data/seed_benchmark_men.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)

  expect_error({
    sim <- simTourney(dat, N=5, year=2018, progress=TRUE, parallel=TRUE)
  }, "Can't use a progress bar in parallel.  Please set either progress or parallel to FALSE")

  #Test all years in parallel
  foreach::registerDoSEQ()
  for(year in sort(unique(dat$season))){
    sim <- simTourney(dat, N=10, year=year, progress=FALSE, parallel=TRUE)
    expect_gt(nrow(sim[slot=='R6CH',]), 63)
    expect_is(sim, 'data.table')
    expect_true(!anyNA(sim))
  }
})
