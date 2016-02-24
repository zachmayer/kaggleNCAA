
test_that("We can parse the Kaggle sample bracket", {
  f <- system.file('kaggle_data/SampleSubmission.csv', package = "kaggleNCAA", mustWork=TRUE)
  dat <- parseBracket(f)
  dat <- dat[season==2015,]
  expect_is(dat, "data.table")
  expect_equal(nrow(dat), 2278)
  expect_true(dat[,all(team_1 < team_2)])
})
