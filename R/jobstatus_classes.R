# R6 class for storing and transferring job status information

#' @importFrom R6 R6Class
jobstatus_node <- R6::R6Class(

  classname = "jobstatus_node",

  private = list(

    callbacks_status_changed = new.env(parent = emptyenv()),

    nextCallbackId = 1L,

    # the file to write progress information to when sending status information
    # up the tree
    write_file = NULL,

    # a list of files containing progress information from down the tree
    read_files = list(),

    # generate a file for passing status information
    generate_filename = function (...) {
        file.path (dirname (get (JOBSTATUS_FILE_NAME, envir = .GlobalEnv)),
                   rhash ())
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

    # write the status to file IFF there is a parent jobstatus object
    write_status = function () {

      if (private$has_parent()) {
        # <to do>

        attr (self$progress, "jobstatus_filename") <- get (JOBSTATUS_FILE_NAME)

        f <- file(private$write_file, open = "w")
        x <- serialize(self, f)
        close (f)

      }

    },

    # read the status information from children
    read_status = function () {

      vals <- lapply(private$read_files,
        function (i) {
          sleep_times <- c(0.01, 0.1, 0.2)
          # Try this up to 3 times
          for (i in seq_len(length(sleep_times) + 1L)) {
            tryCatch({
              f <- file (i, open = "r")
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
      private$status$progress <- prog

      f <- file(private$write_file, open = "w")
      x <- serialize(private, f)
      close (f)
      x

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
    status = NULL,

    # the initialisation function (called with jobstatus$new()) which takes at
    # minimum the maximum number of iterations of the job (if a terminal
    # jobstatus object), and optionally a filename to write status information
    # to a parent jobstatus object. By default, get_current_job() tries to
    # automagically detect a parent jobstatus object, so that isn't necessary.
    initialize = function (super_job = get_current_job()) {

      # do something with super_job

      # record where to write the status information
      private$write_file <- super_job

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
      print("a jobstatus object with current status:")
      print(self$status)
    }

  )
)

# jobstatus nodes that aren't terminal just accumulate status information from
# their children
intermediate_jobstatus_node <- R6::R6Class(

  classname = "intermediate_jobstatus_node",

  inherit = jobstatus_node,

  public = list(

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

      new_status <- private$read_status()
      private$check_status(new_status)
      self$status <- new_status
      private$write_status()

      if (private$status_changed)
        private$fire_status_changed()

    }

  )
)

#' @export
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
      self$status <- list(progress = 0, ...)

      # how to make public member (self$status) immutable?

      # record where to write the status information
      private$write_file <- super_job

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
        new_status$progress <- progress
      other_args <- list(...)
      for (arg_name in other_args) {
        new_status[[arg_name]] <- other_args
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
      self$status$progress[[1]] <- progress
    }

  )
)
