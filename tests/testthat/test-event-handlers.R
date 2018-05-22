context("test-event-handlers.R")

describe("jobstatus", {
  it("event handlers work", {

    file <- file.path(tempdir(), "Hello")
    .GlobalEnv[[JOBSTATUS_FILE_NAME]] <- file

    fake_status <- function (...) {
      status <- list(...)
      attr(status, "jobstatus_filename") <- file
      attr(status, "job_terminated") <- FALSE
      status
    }

    with_jobstatus({
      job <- terminal_jobstatus_node$new()

      value <- job$status
      times_called <- 0L

      reg_handle <- job$on_status_changed(function(status) {
        value <<- status
        times_called <<- times_called + 1L
      })

      expect_identical(value, list(progress = 0))

      job$set_status(10)

      expected <- fake_status(progress = 10)

      expect_equal(value, expected)
      expect_identical(times_called, 1L)

      other_times_called <- 0L
      job$on_status_changed(function(status) {
        other_times_called <<- other_times_called + 1L
      })

      job$set_status(20)
      expected <- fake_status(progress = 20)

      expect_equal(value, expected)
      expect_identical(times_called, 2L)
      expect_identical(other_times_called, 1L)

      # Now unregister the first event handler
      reg_handle()
      job$set_status(30)
      # value and times_called shouldn't have changed

      expect_equal(value, expected)
      expect_identical(times_called, 2L)
      # but this one should have
      expect_identical(other_times_called, 2L)
    }, NULL)
  })
})
