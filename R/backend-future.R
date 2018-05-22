# a jobstatus_node object that might be assigned to the global environment, and
# rperesents the current job
JOBSTATUS_NODE_NAME <- ".current_jobstatus_node"

# filepath that might be assigned to the global environment saying where the
# current jobstatus node should write status information to
JOBSTATUS_FILE_NAME <- ".current_jobstatus_file"

# make subjobfuture pass the new filename instead


#' subjob_future
#'
#' a drop-in replacement for \code{\link[future:future]{future::future}} that passes on job status
#' information
#'
#' @export
subjob_future <- function(expr, envir = parent.frame(), substitute = TRUE,
  globals = TRUE, packages = NULL, lazy = FALSE, seed = NULL,
  evaluator = plan("next"), ...) {

  if (!exists(JOBSTATUS_NODE_NAME, .GlobalEnv)) {
    .GlobalEnv[[JOBSTATUS_NODE_NAME]] <- NULL
  }

  expr <- substitute(expr)

  # on.exit with global environment
  js <- get_current_job()
  if (is.null(js)) {
    stop("Can't call subjob_future from outside of a with_jobstatus block")
  }
  subjob_file_name <- js$create_sub_jobstatus()

  # to pass the file name on to the subjob, temporarily set the current file to
  # be this subjob's one
  assign(JOBSTATUS_FILE_NAME, subjob_file_name, envir = .GlobalEnv)
  on.exit({
    if (exists(JOBSTATUS_FILE_NAME, .GlobalEnv))
      rm(list = JOBSTATUS_FILE_NAME, pos = .GlobalEnv)
  }, add = TRUE)

  globals <- enhance_globals(expr, envir, globals, packages, JOBSTATUS_FILE_NAME)
  packages <- unique(c(packages, "jobstatus"))

  f <- future::future(expr, envir = envir, substitute = FALSE, globals = globals,
    packages = packages, lazy = lazy, seed = seed, evaluator = evaluator, ...)

  class(f) <- c("Subjob.Future", class(f))
  f
}

pump_events <- function() {
  node <- get_current_job()
  node$fetch_status()
}

#' @export
#' @rdname subjob_future
resolved.Subjob.Future <- function(x, timeout = 0.2, ...) {
  pump_events()
  NextMethod()
}

#' @export
#' @rdname subjob_future
result.Subjob.Future <- function(future, ...) {
  while (!resolved(future, timeout = 0)) {
    Sys.sleep(0.1)
  }
  NextMethod()
}

enhance_globals <- function(expr, envir, globals, packages, extra_globals) {
  debug <- getOption("future.debug", FALSE)
  mdebug <- future:::mdebug

  if (is.logical(globals)) {
    ## Gather all globals?
    if (globals) {
      if (debug) mdebug("Finding globals ...")

      #      expr <- do.call(call, args = c(list("FUN"), list(...)))
      gp <- future::getGlobalsAndPackages(expr, envir = envir, tweak = future:::tweakExpression, globals = TRUE)
      globals <- gp$globals
      packages <- unique(c(packages, gp$packages))
      gp <- NULL

      if (debug) {
        mdebug(" - globals found: [%d] %s", length(globals), hpaste(sQuote(names(globals))))
        mdebug(" - needed namespaces: [%d] %s", length(packages), hpaste(sQuote(packages)))
        mdebug("Finding globals ... DONE")
      }

      globals <- unique(c(globals, mget(extra_globals, globalenv())))
    } else {
      globals <- extra_globals
    }
  } else if (is.character(globals)) {
    globals <- unique(c(globals, "FUN", "args"))

    globals <- unique(c(globals, extra_globals))
  } else if (is.list(globals)) {
    globals <- c(globals, mget(extra_globals, globalenv()))
  } else {
    stop(FutureError("Invalid argument 'globals': ", mode(globals)))
  }
  globals
}
