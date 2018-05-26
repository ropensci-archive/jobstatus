# Retrieve the current jobstatus object from the global environment.
# If there's no current jobstatus object but there is a jobstatus file
# name (i.e. sent from the parent), create a new jobstatus object and
# make it current.
get_current_job <- function () {
  if (exists(JOBSTATUS_NODE_NAME, .GlobalEnv)) {
    current_job <- .GlobalEnv[[JOBSTATUS_NODE_NAME]]
  } else {
    current_job <- NULL
  }
  current_job
}

with_current_job <- function(jobstatus, expr) {
  # old <- .GlobalEnv[[JOBSTATUS_NODE_NAME]]
  .GlobalEnv[[JOBSTATUS_NODE_NAME]] <- jobstatus
  on.exit(.GlobalEnv[[JOBSTATUS_NODE_NAME]] <- NULL)
  force(expr)
}
