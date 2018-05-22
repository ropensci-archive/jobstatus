
# handle accumulation and passing of jobstatus information whilst executing code
# in an intermediate or top-level (ie. non-terminal) job. Apply a display
# callback to enable visualisation of progress and status information.


#' with_jobstatus
#'
#' run an expression with some jobs, transmitting and possibly displaying status
#' information from jobstatus objects in the jobs
#'
#' @param expr an expression to execute
#' @param display a function to read status information and display the progress
#'   information
#'
#' @export
with_jobstatus <- function (expr, display = jobstatus_bar()) {

  # create an intermediate jobstatus object, using the current filename,
  # if there is one
  js <- intermediate_jobstatus_node$new()

  # temporarily put the jobstatus in the global environment, to be
  # used by subjob future
  old_js <- .GlobalEnv[[JOBSTATUS_NODE_NAME]]
  .GlobalEnv[[JOBSTATUS_NODE_NAME]] <- js
  on.exit(.GlobalEnv[[JOBSTATUS_NODE_NAME]] <- old_js, add = TRUE)

  if (!is.null(display) && is.null(old_js)) {
    on.exit(clear_progress_display(display))
    # register the display callback with it

    # possibly suppress this when deeper in the stack, by only executing if the
    # node has no parents (in ons_status_changed)
    js$on_status_changed(function(status) {
      update_progress_display(display, status)
    })
  }

  # execute the code and return the value of the expression
  with_current_job(js, expr)
}
