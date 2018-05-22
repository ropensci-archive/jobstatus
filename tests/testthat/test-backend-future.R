context("test-backend-future.R")

library(future)
plan(multisession)

describe("subjob_future", {
  it("passes subjob context variable along", {
    .GlobalEnv[[JOBSTATUS_VARNAME]] <- "Hello"

    test <- function(globals) {
      x <- value(subjob_future(get(jobstatus:::JOBSTATUS_VARNAME), globals = globals))
      expect_identical(x, "Hello")
    }
    test(globals = TRUE)
    test(globals = FALSE)
    test(globals = list(a = 1, b = 2))
    test(globals = "foo")
  })
})

describe("regular future", {
  it("doesn't pass subjob context variable along", {
    .GlobalEnv[[JOBSTATUS_VARNAME]] <- "Hello"

    expect_error(
      value(future(get(JOBSTATUS_VARNAME))),
      "not found"
    )
  })
})
