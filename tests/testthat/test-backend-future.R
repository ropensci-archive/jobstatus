context("test-backend-future.R")

library(future)
plan(multisession)

describe("subjob_future", {
  it("passes subjob context variable along", {

    test <- function(globals) {
      x <- value(subjob_future(get(jobstatus:::JOBSTATUS_FILE_NAME, globalenv()), globals = globals))
      expect_true(!is.null(x))
    }
    with_jobstatus({
      test(globals = TRUE)
      test(globals = FALSE)
      test(globals = list(a = 1, b = 2))
      test(globals = "foo")
    }, NULL)
  })
})

describe("regular future", {
  it("doesn't pass subjob context variable along", {

    expect_error(
      value(future(get(jobstatus:::JOBSTATUS_FILE_NAME))),
      "not found"
    )
  })
})
