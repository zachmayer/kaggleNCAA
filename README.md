<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/zachmayer/kaggleNCAA.png?branch=master)](https://travis-ci.org/zachmayer/kaggleNCAA) [![Coverage Status](https://coveralls.io/repos/zachmayer/kaggleNCAA/badge.svg?branch=master)](https://coveralls.io/r/zachmayer/kaggleNCAA?branch=master)

Kaggle NCAA Bracket Simulator
=============================

Simulate the NCAA tournament based on a kaggle-format bracket (with predictions for every possible matchup).

First, re-install the package:
------------------------------

``` r
devtools::install_github('zachmayer/kaggleNCAA')
```

Now load the tournment from a csv
---------------------------------

`seed_benchmark_men.csv` is a simple seed-based benchmark I made. Turn it into a bracket with 4 functions:

-   `parseBracket` to load the data from a .csv
-   `simTourney` to simulate the tourney (you can also use `walkTourney`). Use at least 1000 simulations (more is better). One day I will parallelize this =D
-   `extractBracket` to extract a bracket from the simulation results
-   `printableBracket` to actually print the bracket

(Load my data with `data(sample_submission_men, package='kaggleNCAA')` if you want to just use the seeds)

``` r
set.seed(1)
library('kaggleNCAA')
dat <- parseBracket('seed_benchmark_men.csv', w=0)  # w=0 for men
sim <- simTourney(dat, 10, progress=TRUE, w=0)  # w=0 for men
bracket <- extractBracket(sim)
printableBracket(bracket)
#> assuming women = 0
```

![](README-sim_bracket-1.png) If simulation's not your thing (e.g. your predicted probabilities are transitive), you can also "walk" forward through the tournament, which is much faster:

``` r
bracket <- walkTourney(dat)
printableBracket(bracket)
#> assuming women = 0
```

![](README-walk_bracket-1.png) Note in this case that the probabilities associated with the team making it to a given round will be incorrect
