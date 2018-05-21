# R6 class for storing and transferring job status information

#' @importFrom R6 R6Class
jobstatus <- R6::R6Class(

  classname = "jobstatus",

  private = list(

    # the file to write progress information to when sending status information
    # up the tree
    write_file = NULL,

    # a list of files containing progress information from down the tree
    read_files = list(),

    # create a child jobstatus object and register it
    create_sub_jobstatus = function () {

      filename <- private$generate_filename()
      private$read_files <- c(private$read_files, filename)
      filename

    },

    generate_filename = function (...) {
      tempfile(...)
    }

  ),

  public = list(

    # the status of this job
    status = list(),

    # the initialisation function (called with jobstatus$new()) which takes the
    # maximum number of iterations fo the job, and optionally a parent jobstatus
    # object. By default, get_current_job() tries to automagically detect a
    # parent jobstatus object.
    initialize = function (maximum_progress = NULL,
                           super_job = get_current_job()) {


      private$write_file <- super_job


    },

    set_status = function () {

    },

    fetch_status = function () {

    },

    send_status = function () {

    }

  )
)
