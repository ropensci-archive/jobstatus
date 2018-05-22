# R6 class for storing and transferring job status information

#' @importFrom R6 R6Class
jobstatus_node <- R6::R6Class(

  classname = "jobstatus_node",

  private = list(

    callbacks_status_changed = new.env(parent = emptyenv()),

    nextCallbackId = 1L,

    terminated = FALSE,

    # the file to write progress information to when sending status information
    # up the tree
    write_file = NULL,

    # a list of files containing progress information from down the tree
    read_files = list(),

    # generate a file for passing status information
    generate_filename = function (...) {
      if (!is.null(.GlobalEnv[[JOBSTATUS_FILE_NAME]])) {
        file.path(dirname(.GlobalEnv[[JOBSTATUS_FILE_NAME]]), rhash())
      } else {
        file.path(getOption("jobstatus.basedir", tempdir()), rhash())
      }
    },

    status_changed = FALSE,

    fire_status_changed = function() {
      # TODO: Fire event handlers in the order they were added
      for (name in ls(private$callbacks_status_changed)) {
        # TODO: Error handling, maybe
        private$callbacks_status_changed[[name]](self$status)
      }
    },

    has_children = function () {
      !identical(private$read_files, list())
    },

    has_parent = function () {
      !is.null(private$write_file)
    },

    default_status = function () {
      status <- list(progress = list(0),
                     max = 10)
      attr (status, "jobstatus_filename") <- private$write_file
      attr (status, "job_terminated") <- FALSE
      status
    },

    # write the status to file IFF there is a parent jobstatus object
    write_status = function () {

      if (private$has_parent()) {
        # <to do>
        attr (self$status, "jobstatus_filename") <- private$write_file
        attr (self$status, "job_terminated") <- private$terminated

        f <- file(private$write_file, open = "w")
        x <- serialize(self, f)
        close (f)

      }

    },

    # read the status information from children
    read_status = function () {

      vals <- lapply(private$read_files,
        function (filename) {
          sleep_times <- c(0.01, 0.1, 0.2)
          # Try this up to 3 times
          for (i in seq_len(length(sleep_times) + 1L)) {
            tryCatch({
              if (!file.exists(filename))
                return (private$default_status())
              f <- file (filename, open = "r")
              ret <- unserialize (f)$status$progress
              close (f)
              return (ret)
            }, error = function(e) {
              if (i <= length(sleep_times)) {
                Sys.sleep(sleep_times[[i]])
              } else {
                # Give up
                stop(e)
              }
            })
          }
        })

      prog <- list(progress = vals)

      private$status_changed = !identical (vals, self$status$progress)
      self$status$progress <- prog

      if (private$has_parent()) {
        f <- file(private$write_file, open = "w")
        x <- serialize(private, f)
        close (f)
      }

      invisible()
    },

    # validation of status inputs.
    check_status = function (status, terminal = FALSE) {

      # if this is a terminal node (e.g. for set_status), there should be no
      # nested list structure
      if (terminal) {

      } else {
        lapply(status, private$check_status, terminal = FALSE)
      }

    }

  ),

  public = list(

    # the status of this job
    status = structure(list(),
                       job_terminated = FALSE,
                       jobstatus_filename = ""),

    # the initialisation function (called with jobstatus$new()) which takes at
    # minimum the maximum number of iterations of the job (if a terminal
    # jobstatus object), and optionally a filename to write status information
    # to a parent jobstatus object. By default, get_current_job() tries to
    # automagically detect a parent jobstatus object, so that isn't necessary.
    initialize = function (super_job = get_current_job()) {

      # record where to write the status information
      if (is.character(super_job)) {
        # This is a special case for when with_jobstatus is called inside
        # a new subjob_future; we know exactly what file we want to write
        # to.
        private$write_file <- super_job
      } else if (is.null(super_job)) {
        private$write_file <- NULL
      } else {
        private$write_file <- super_job$create_sub_jobstatus()
      }
    },

    on_status_changed = function(callback) {
      id <- as.character(private$nextCallbackId)
      private$nextCallbackId <- private$nextCallbackId + 1L
      private$callbacks_status_changed[[id]] <- callback
      # Return a no-arg function that can be called to unregister the callback
      invisible(function() {
        rm(list = id, pos = private$callbacks_status_changed)
      })
    },

    # a print method for this object
    print = function () {
      cat("a jobstatus object with current status:\n")
      status <- self$status
      attributes(status) <- NULL
      print(status)
    }

  )
)

# jobstatus nodes that aren't terminal just accumulate status information from
# their children
intermediate_jobstatus_node <- R6::R6Class(

  classname = "intermediate_jobstatus_node",

  inherit = jobstatus_node,

  private = list(
    # if all the children have terminated, label this as terminated and return
    check_termination = function () {
      children_terminated <- vapply(self$status,
                                    function(x) {
                                      jt <- attr(x, "job_terminated", exact = TRUE)
                                      !is.null(jt) && jt
                                    },
                                    FUN.VALUE = FALSE)
      if (all(children_terminated)) {
        private$terminated <- TRUE
      }

      invisible()
    }

  ),

  public = list(
    initialize = function(super_job = get_current_job()) {
      super$initialize(super_job = super_job)
    },

    # create a child jobstatus object and register it
    create_sub_jobstatus = function () {

      filename <- private$generate_filename()
      private$read_files <- c(private$read_files, filename)
      filename

    },

    # fetch status information from the children
    fetch_status = function () {

      if (!private$has_children()) {
        stop ("cannot fetch the status as there are no sub-jobs",
              call. = FALSE)
      }

      private$read_status()
      # private$check_status(new_status)
      private$check_termination()
      private$write_status()

      if (private$status_changed)
        private$fire_status_changed()

    }

  )
)

terminal_jobstatus_node <- R6::R6Class(

  classname = "terminal_jobstatus_node",

  inherit = jobstatus_node,

  public = list(

    maximum_progress = NULL,

    # the initialisation function (called with jobstatus$new()) which takes at
    # minimum the maximum number of iterations of the job (if a terminal
    # jobstatus object), and optionally a filename to write status information
    # to a parent jobstatus object. By default, get_current_job() tries to
    # automagically detect a parent jobstatus object, so that isn't necessary.
    initialize = function (maximum_progress = 100L,
                           ...,
                           super_job = get_current_job()) {

      super$initialize(super_job)
      self$maximum_progress <- maximum_progress
      status <- private$default_status()
      status$max <- maximum_progress

      other_args <- list(...)
      names <- names(other_args)
      for (arg_num in seq_along(other_args)) {
        status[[names[arg_num]]] <- other_args[[arg_num]]
      }

      self$status <- status

    },

    # set the current progress and any other information and write
    set_status = function (progress, ...) {

      if (private$has_children()) {
        stop ("cannot set the status as there are sub-jobs",
              call. = FALSE)
      }

      # Merge old status and new args
      new_status <- self$status
      if (!missing(progress))
        new_status$progress <- list(progress)
      other_args <- list(...)

      names <- names(other_args)
      for (arg_num in seq_along(other_args)) {
        new_status[[names[arg_num]]] <- other_args[[arg_num]]
      }

      # <update the status info>
      private$check_status(new_status, terminal = TRUE)
      self$status <- new_status
      private$write_status()

      private$fire_status_changed()

    },

    # utility function to increment the progress only
    tick = function () {
      progress <- self$status$progress[[1]]
      progress <- progress + 1
      self$set_status(progress = progress)
    },

    finish = function () {
      private$terminated = TRUE
    }

  )
)

#' jobstatus
#'
#' create a job status node to record progress on a task
#'
#' @export
#'
jobstatus <- terminal_jobstatus_node
