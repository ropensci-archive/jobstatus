# attempt to detect the filename for a current jobstatus object in the calling
# environment
get_current_job <- function () {

  if (exists(JOBSTATUS_VARNAME)){
    current_job <- get(JOBSTATUS_VARNAME, envir = .GlobalEnv)
  } else {
    current_job <- NULL
  }

  current_job
}
