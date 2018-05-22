JOBSTATUS_VARNAME = ".current_jobstatus"

#' @export
subjob_future <- function(expr, envir = parent.frame(), substitute = TRUE,
  globals = TRUE, packages = NULL, lazy = FALSE, seed = NULL,
  evaluator = plan("next"), ...) {

  if (!exists(JOBSTATUS_VARNAME, .GlobalEnv)) {
    .GlobalEnv[[JOBSTATUS_VARNAME]] <- NULL
  }

  expr <- substitute(expr)

  globals <- enhance_globals(expr, envir, globals, packages, JOBSTATUS_VARNAME)
  packages <- unique(c(packages, "jobstatus"))

  future::future(expr, envir = envir, substitute = FALSE, globals = globals,
    packages = packages, lazy = lazy, seed = seed, evaluator = evaluator, ...)
}

enhance_globals <- function(expr, envir, globals, packages, extra_globals) {
  debug <- getOption("future.debug", FALSE)
  mdebug <- future:::mdebug

  if (is.logical(globals)) {
    ## Gather all globals?
    if (globals) {
      if (debug) mdebug("Finding globals ...")

      #      expr <- do.call(call, args = c(list("FUN"), list(...)))
      gp <- getGlobalsAndPackages(expr, envir = envir, tweak = future:::tweakExpression, globals = TRUE)
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
