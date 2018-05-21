# attempt to detect the filename for a current jobstatus object in the calling
# environment
get_current_job <- function () {

  if (exists(".current_job_file")){
    current_job <- .current_job_file
  } else {
    current_job <- NULL
  }

  current_job
}
