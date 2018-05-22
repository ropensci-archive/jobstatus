
[![Travis build status](https://travis-ci.org/ropenscilabs/jobstatus.svg?branch=master)](https://travis-ci.org/ropenscilabs/jobstatus) [![Coverage status](https://codecov.io/gh/ropenscilabs/jobstatus/branch/master/graph/badge.svg)](https://codecov.io/github/ropenscilabs/jobstatus?branch=master) [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- README.md is generated from README.Rmd. Please edit that file -->
jobstatus
=========

`jobstatus` lets you pass live progress, status, and other information between functions and processes in R, so that you can keep an eye on how complex and long-running jobs are progressing. `jobstatus` uses the [`future` package](https://cran.r-project.org/package=future) so you can even get live progress information back from jobs running in parallel.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ropenscilabs/jobstatus")
```
