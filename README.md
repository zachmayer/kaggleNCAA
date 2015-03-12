<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build Status](https://travis-ci.org/zachmayer/kaggleNCAA.png?branch=master)](https://travis-ci.org/zachmayer/kaggleNCAA) [![Coverage Status](https://coveralls.io/repos/zachmayer/kaggleNCAA/badge.svg?branch=master)](https://coveralls.io/r/zachmayer/kaggleNCAA?branch=master)

Kaggle NCAA Bracket Simulator
=============================

Simulate the NCAA tournament based on a kaggle-format bracket (with predictions for every possible matchup)

``` {.r}
library('kaggleNCAA')
f <- system.file('kaggle_data/sample_submission.csv', package = "kaggleNCAA", mustWork=TRUE)
dat <- parseBracket(f)
sim <- simTourney(dat, 10, year=2014, progress=TRUE)
bracket <- extractBracket(sim)
printableBracket(bracket)
```

![](README-unnamed-chunk-2-1.png)
