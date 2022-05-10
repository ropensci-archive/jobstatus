
[![Travis build
status](https://travis-ci.org/ropenscilabs/jobstatus.svg?branch=master)](https://travis-ci.org/ropenscilabs/jobstatus)
[![Coverage
status](https://codecov.io/gh/ropenscilabs/jobstatus/branch/master/graph/badge.svg)](https://codecov.io/github/ropenscilabs/jobstatus?branch=master)
[![Project Status: Abandoned – Initial development has started, but there has not yet been a stable, usable release; the project has been abandoned and the author(s) do not intend on continuing development.](https://www.repostatus.org/badges/latest/abandoned.svg)](https://www.repostatus.org/#abandoned)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# jobstatus

`jobstatus` lets you pass live progress, status, and other information
between functions and processes in R, so that you can keep an eye on how
complex and long-running jobs are progressing. `jobstatus` uses the
[`future` package](https://cran.r-project.org/package=future) so you can
even get live progress information back from jobs running in parallel.

## How it works

`jobstatus` passes status information between functions and processes in
R so you can monitor what’s happening and how much progress the code has
made. There are three main functions in `jobstatus`:

  - `jobstatus` creates a jobstatus object, which lets you record your
    progress on a single task
  - `with_jobstatus` runs a chunk of code that runs other jobs, and
    keeps track of the status information
  - `subjob_future` is a wrapper function for `future::future` that lets
    you write code that can be executed either in sequence or in
    parallel.

`with_jobstatus` can also be used to display the progress information,
for example with a progress bar.

A `jobstatus` set up might look something like this:

``` r
library (jobstatus)
library (progress)

# a function to run a single task
some_big_task <- function (iterations = 100) {
  
  # create a jobstatus object to track progress
  # do the work, incrementing the progress counter each time
  status <- jobstatus$new(iterations)
  
  result <- 0
  for (i in seq_len(iterations)) {
    result <- result + 1
    Sys.sleep(runif(1, 0, 0.2))
    status$tick()
  }
  
  # tidy up when we're done, and return the results
  status$finish()
  
  result
  
} 

# load the future package and set it to run the jobs in parallel
library (future)
plan(multiprocess)

# dispatch some jobs, tracking their status information and displaying multiple progress bars
with_jobstatus({
  
  # create some futures (possibly in parallel)
  f1 <- subjob_future(some_big_task())
  f2 <- subjob_future(some_big_task())
  f3 <- subjob_future(some_big_task())
  f4 <- subjob_future(some_big_task())
  
  # get their values
  v1 <- value(f1)
  v2 <- value(f2)
  v3 <- value(f3)
  v4 <- value(f4)
  
}, display = percentage)
```

![](figs/percentage_progress.gif)

## Extensions

This is very much a work in progress and the current implementation is
quite limited (and probably quite buggy). Ideally, we’d like it to
support various different parallel backends and interfaces, handle other
types of job status information, and provide different types of progress
bars and displays.

This prototype was developed over two days at the [2018 rOpenSci
unconference](http://unconf18.ropensci.org/) and the maintainers won’t
have much time to extend and improve the package. We’d love to have
help, so if you’re keen please let us know in [the issues
tracker](https://github.com/ropenscilabs/jobstatus/issues)\!

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ropenscilabs/jobstatus")
```
