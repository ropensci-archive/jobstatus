# Retrieve the current jobstatus object from the global environment.
# If there's no current jobstatus object but there is a jobstatus file
# name (i.e. sent from the parent), create a new jobstatus object and
# make it current.
get_current_job <- function () {
  if (exists(JOBSTATUS_NODE_NAME, .GlobalEnv)) {
    return(.GlobalEnv[[JOBSTATUS_NODE_NAME]])
  } else if (exists(JOBSTATUS_FILE_NAME, .GlobalEnv)){
    filename <- .GlobalEnv[[JOBSTATUS_FILE_NAME]]
    rm(list = JOBSTATUS_FILE_NAME, .GlobalEnv)

    node <- intermediate_jobstatus_node$new(super_job = filename)
    .GlobalEnv[[JOBSTATUS_NODE_NAME]] <- node
    return(node)
  } else {
    return(NULL)
  }
}

with_current_job <- function(jobstatus, expr) {
  old <- .GlobalEnv[[JOBSTATUS_NODE_NAME]]
  .GlobalEnv[[JOBSTATUS_NODE_NAME]] <- jobstatus
  on.exit(.GlobalEnv[[JOBSTATUS_NODE_NAME]] <- old)
  force(expr)
}
