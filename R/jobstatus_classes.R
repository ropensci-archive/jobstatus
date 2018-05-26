# R6 class for storing and transferring job status information

#' @importFrom R6 R6Class
jobstatus_node <- R6::R6Class(

  classname = "jobstatus_node",

  private = list(

    callbacks_status_changed = new.env(parent = emptyenv()),

    nextCallbackId = 1L,

    parent = NULL,

    terminated = FALSE,

    # the file to write progress information to when sending status information
    # up the tree
    write_file = NULL,

    # a list of files containing progress information from down the tree
    read_files = list(),

    # generate a file for passing status information
    generate_filename = function (...) {
      file.path(getOption("jobstatus.basedir", tempdir()), rhash())
    },

    status_changed = FALSE,

    fire_status_changed = function() {

      # look for a parent
      parent <- private$parent

      # if we're at the top of the stack, it's up to us to display
      if (is.null(parent)) {

        # TODO: Fire event handlers in the order they were added
        for (name in ls(private$callbacks_status_changed)) {
          # TODO: Error handling, maybe
          private$callbacks_status_changed[[name]](self$status)
        }

      } else {

        # if we have a parent and they told us we are running in sequence, we should trigger the
        # parent to fire from higher up the stack
        if (parent$sequential) {
          parent$fetch_status()
        }

        # if we're not running in sequence, we can't display anything and we're
        # not holding anyone up, so move on

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
        write(private$write_file,
              list(filename = private$write_file,
                   terminated = private$terminated,
                   progress = self$status$progress,
                   max = self$status$max)
              )

      }

    },

    # read the status information from children, with delays in case of blocking
    read_status = function () {

      read_filename <- function (filename) {

        sleep_times <- c(0.01, 0.1, 0.2)

        # Try this up to 3 times
        for (i in seq_len(length(sleep_times) + 1L)) {
          tryCatch({

            # if (!file.exists(filename)) {
            #   return (private$default_status())
            # }

            status <- read(filename)
            return (status)

          },

          error = function(e) {
            # on error, try waiting
            if (i <= length(sleep_times)) {
              Sys.sleep(sleep_times[[i]])
            } else {
              # Give up
              stop(e)
            }
          })
        }
      }

      status_list <- lapply(private$read_files, read_filename)
      new_status <- invert(status_list)
      private$status_changed <- !identical (new_status, self$status)
      self$status <- new_status
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
    status = NULL,

    # the initialisation function (called with jobstatus$new()) which takes at
    # minimum the maximum number of iterations of the job (if a terminal
    # jobstatus object), and optionally a filename to write status information
    # to a parent jobstatus object. By default, get_current_job() tries to
    # automagically detect a parent jobstatus object, so that isn't necessary.
    initialize = function (super_job = get_current_job()) {

      private$parent <- super_job
      if (!is.null(super_job)) {
        private$write_file <- super_job$latest_read_file()
      }

      self$status <- empty_status()

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

    sequential = NULL,

    initialize = function(super_job = get_current_job()) {
      super$initialize(super_job = super_job)
    },

    # create a child jobstatus object and register it
    create_sub_jobstatus = function () {

      filename <- private$generate_filename()
      private$read_files <- c(private$read_files, filename)

      # just create it now, so we don't have to worry about waiting for it to be created
      # the empty list means progress bars can identify this as having 0 progress so far
      write(filename, empty_status(filename))
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

    },

    latest_read_file = function () {
      n_files <- length(private$read_files)
      if (n_files > 0) {
        ans <- private$read_files[[n_files]]
      } else {
        ans <- NULL
      }
      ans
    }

  )
)

terminal_jobstatus_node <- R6::R6Class(

  classname = "terminal_jobstatus_node",

  inherit = jobstatus_node,

  private = list(
    sequential = NULL
  ),

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

      # if jobstatus hasn't created a file, we can ignore it
      if (is.null(super_job$latest_read_file))
        super_job <- NULL

      super$initialize(super_job)

      # record whether we were running in sequence at the time of construction
      private$sequential <- private$parent$sequential

      self$maximum_progress <- maximum_progress
      status <- empty_status(private$write_file)
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
        new_status$progress <- progress
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
