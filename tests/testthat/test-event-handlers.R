context("test-event-handlers.R")

describe("jobstatus", {
  it("event handlers work", {

    copy_filename <- function(from, to) {
      to$filename <- from$filename
      to
    }

    fake_status <- function (object, progress = 0, max = 100L) {
      list(filename = object$.__enclos_env__$private$write_file,
           terminated = FALSE,
           progress = progress,
           max = max)
    }

    with_jobstatus({
      job <- jobstatus$new()

      value <- job$status

      times_called <- 0L

      reg_handle <- job$on_status_changed(
        function (status) {
          value <<- status
          times_called <<- times_called + 1L
        })

      expected <- fake_status(job)
      expect_identical(value, expected)
#
#       job$set_status(progress = 10)
#
#       expected <- fake_status(job, progress = 10)
#       expected <- copy_filename(value, expected)
#
#       expect_equal(value, expected)
#       expect_identical(times_called, 1L)
#
#       other_times_called <- 0L
#       job$on_status_changed(function(status) {
#         other_times_called <<- other_times_called + 1L
#       })
#
#       job$set_status(20)
#       expected <- fake_status(pjob, progress = 20)
#       expected <- copy_filename(value, expected)
#
#       expect_equal(value, expected)
#       expect_identical(times_called, 2L)
#       expect_identical(other_times_called, 1L)
#
#       # Now unregister the first event handler
#       reg_handle()
#       job$set_status(30)
#       # value and times_called shouldn't have changed
#
#       expect_equal(value, expected)
#       expect_identical(times_called, 2L)
#       # but this one should have
#       expect_identical(other_times_called, 2L)
    }, display = NULL)
  })
})
