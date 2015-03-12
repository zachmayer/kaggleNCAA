
test_that("We can parse the Kaggle sample bracket", {
  f <- system.file('inst/kaggle_data/sample_submission.csv', package = "kaggleNCAA", mustWork=TRUE)
  print(f)
  dat <- parseBracket(f)
  expect_is(dat, "data.table")
  expect_equal(nrow(dat), 9112)
  expect_true(dat[,all(team_1 < team_2)])
})
