# attempt to detect the filename for a current jobstatus object in the calling
# environment
get_current_job <- function () {

  if (exists(JOBSTATUS_FILE_NAME)){
    current_job <- get(JOBSTATUS_FILE_NAME, envir = .GlobalEnv)
  } else {
    current_job <- NULL
  }

  current_job
}
