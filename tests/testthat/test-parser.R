test_that("We can parse the Kaggle sample bracket - women", {
  f <- system.file('kaggle_data/seed_benchmark_women.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)
  expect_is(dat, "data.table")
  expect_equal(nrow(dat), 2016) # 8064 round 1 (practice), 2016 round 2 (real) (REALLY 2016?)
  expect_true(dat[,all(teamid_1 < teamid_2)])
})

test_that("We can parse the Kaggle sample bracket - men", {
  f <- system.file('kaggle_data/seed_benchmark_men.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)
  expect_is(dat, "data.table")
  expect_equal(nrow(dat), 2278) # 9112 round 1 (practice), 2278 round 2 (real)
  expect_true(dat[,all(teamid_1 < teamid_2)])
})
