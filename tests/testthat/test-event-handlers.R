describe("jobstatus", {
  it("event handlers work", {
    job <- jobstatus$new()

    value <- job$status
    times_called <- 0L

    reg_handle <- job$on_status_changed(function(status) {
      value <<- status
      times_called <<- times_called + 1L
    })

    expect_identical(value, list())

    job$set_status(10)
    expect_identical(value, list(value = 10))
    expect_identical(times_called, 1L)

    other_times_called <- 0L
    job$on_status_changed(function(status) {
      other_times_called <<- other_times_called + 1L
    })

    job$set_status(20)
    expect_identical(value, list(value = 20))
    expect_identical(times_called, 2L)
    expect_identical(other_times_called, 1L)

    # Now unregister the first event handler
    reg_handle()
    job$set_status(30)
    # value and times_called shouldn't have changed
    expect_identical(value, list(value = 20))
    expect_identical(times_called, 2L)
    # but this one should have
    expect_identical(other_times_called, 2L)
  })
})
