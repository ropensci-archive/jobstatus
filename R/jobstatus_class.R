# R6 class for storing and transferring job status information

#' @importFrom R6 R6Class
jobstatus <- R6::R6Class(

  classname = "jobstatus",

  private = list(

    callbacks_status_changed = new.env(parent = emptyenv()),
    nextCallbackId = 1L,

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
    },

    fire_status_changed = function() {
      # TODO: Fire event handlers in the order they were added
      for (name in ls(private$callbacks_status_changed)) {
        # TODO: Error handling, maybe
        private$callbacks_status_changed[[name]](self$status)
      }
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

    set_status = function (value, ...) {
      # TODO: Handle ... arguments

      # TODO: Merge with existing keys
      self$status <- list(value = value)

      private$fire_status_changed()
    },

    # Retrieve status from children
    fetch_status = function () {
      # TODO: implement

      # TODO: Only fire status changed if actually changed
      private$fire_status_changed()
    },

    send_status = function () {

    },

    on_status_changed = function(callback) {
      id <- as.character(private$nextCallbackId)
      private$nextCallbackId <- private$nextCallbackId + 1L
      private$callbacks_status_changed[[id]] <- callback
      # Return a no-arg function that can be called to unregister the callback
      invisible(function() {
        rm(list = id, pos = private$callbacks_status_changed)
      })
    }

  )
)
