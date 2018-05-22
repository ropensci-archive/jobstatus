context("test-backend-future.R")

library(future)
plan(multisession)

describe("subjob_future", {
  it("passes subjob context variable along", {

    file <- file.path(tempdir(), "Hello")
    js <- intermediate_jobstatus_node$new()

    .GlobalEnv[[JOBSTATUS_FILE_NAME]] <- file
    .GlobalEnv[[JOBSTATUS_NODE_NAME]] <- js

    test <- function(globals) {
      x <- value(subjob_future(get(jobstatus:::JOBSTATUS_FILE_NAME), globals = globals))
      all_files <- js$.__enclos_env__$private$read_files
      last_file <- all_files[[length(all_files)]]
      expect_identical(x, last_file)
    }

    test(globals = TRUE)
    test(globals = FALSE)
    test(globals = list(a = 1, b = 2))
    test(globals = "foo")

  })
})

describe("regular future", {
  it("doesn't pass subjob context variable along", {

    file <- file.path(tempdir(), "Hello")
    .GlobalEnv[[JOBSTATUS_FILE_NAME]] <- file

    expect_error(
      value(future(get(JOBSTATUS_FILE_NAME))),
      "not found"
    )
  })
})
