# a jobstatus_node object that might be assigned to the global environment, and
# represents the current job
JOBSTATUS_NODE_NAME <- ".current_jobstatus_node"

#' subjob_future
#'
#' a drop-in replacement for \code{\link[future:future]{future::future}} that passes on job status
#' information.
#'
#' @details Unlike \code{future}, \code{subjob_future} uses \code{lazy = TRUE}
#'   by default when a sequential plan is in operation, since this is required
#'   to correctly display multiple parallale progress bars with a sequential
#'   plan.
#'
#' @export
subjob_future <- function(expr, envir = parent.frame(), substitute = TRUE,
  globals = TRUE, packages = NULL, lazy = inherits(future::plan(), "sequential"), seed = NULL,
  evaluator = plan("next"), ...) {

  if (!exists(JOBSTATUS_NODE_NAME, .GlobalEnv)) {
    .GlobalEnv[[JOBSTATUS_NODE_NAME]] <- NULL
  }

  expr <- substitute(expr)

  js <- get_current_job()
  if (is.null(js)) {
    stop ("Can't call subjob_future from outside of a with_jobstatus block")
  }

  # set whether we're runnning in sequence at the moment, to tell our shildren
  js$sequential <- inherits(future::plan(), "sequential")

  # set up a reporting file for that child
  js$create_sub_jobstatus()

  globals <- enhance_globals(expr, envir, globals, packages, JOBSTATUS_NODE_NAME)
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
value.Subjob.Future <- function(future, ...) {
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
