
# handle accumulation and passing of jobstatus information whilst executing code
# in an intermediate or top-level (ie. non-terminal) job. Apply a display
# callback to enable visualisation of progress and status information.


#' with_jobstatus
#'
#' run an expression with some jobs, transmitting and possibly displaying status information
#' from jobstatus objects in the jobs
#'
#' @export
with_jobstatus <- function (expr, display = jobstatus_bar()) {

  if (exists(JOBSTATUS_NODE_NAME)) {
    stop ("with_jobstatus can only be called once per job",
          call. = FALSE)
  }

  # create an intermediate jobstatus object, using the current filename
  js <- intermediate_jobstatus_node$new()

  # register the display callback with it
  js$on_status_changed(display)

  # put the jobstatus in the global environment, to be used by subjob future
  assign(JOBSTATUS_NODE_NAME, js, envir = .GlobalEnv)

  # execute the code and return the value of the expression
  eval(expr, parent.frame())

}
