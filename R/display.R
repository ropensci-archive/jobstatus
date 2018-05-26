list(
  progress = list(1, 2, 3),
  max = list(10, 10, 10)
)

calculate_ratio <- function(status) {
  num <- sum(unlist(status$progress))
  denom <- sum(unlist(status$max))
  num / denom
}

#' @export
update_progress_display <- function(x, status) {
  UseMethod("update_progress_display")
}

#' @export
clear_progress_display <- function(x) {
  UseMethod("clear_progress_display")
}

#' @export
percentage <- list()
class(percentage) <- "percentage"

#' @export
update_progress_display.percentage <- function(x, status) {
  progress <- unlist(status$progress)
  max <- unlist(status$max)
  percentages <- round(100 * progress / max)
  overall <- calculate_ratio(status)

  job_progress <- sprintf("job %i: %s%%",
                          seq_along(percentages),
                          percentages)
  msg <- sprintf("\r  total: %s%%\t%s",
                 round(100 * overall),
                 paste(job_progress, collapse = "   "))
  cat(msg)
}

#' @export
clear_progress_display.percentage <- function(x) {
  flush.console()
}

# progress_bar implementation =======================

#' @export
update_progress_display.progress_bar <- function(x, status) {
  x$update(calculate_ratio(status))
}

#' @export
clear_progress_display.progress_bar <- function(x) {
}

#' @export
rstudio_progress <- R6Class(
  "rstudio_progress",
  private = list(
    # keys: jobstatus job ID, values: RStudio job ID
    job_id_map = NULL
  ),
  public = list(
    initialize = function() {
      private$job_id_map <- list()
    },
    update = function(status) {
      stopifnot(is.data.frame(status))
      stopifnot(identical(names(status), c("id", "progress", "max")))
      # Status should be a data frame with columns id, progress, and max

      mapply(as.character(status$id), status$progress / status$max,
        FUN = function(id, progress) {
          if (is.null(private$job_id_map[[id]])) {
            private$job_id_map[[id]] <- .rs.api.addJob(
              name = "",
              status = "",
              progressUnits = 100L
            )
          }
          job <- private$job_id_map[[id]]
          .rs.api.setJobProgress(job, as.integer(progress * 100))
          NULL
        }
      )

      invisible()
    },
    close = function() {
      for (rs_id in private$job_id_map) {
        .rs.api.removeJob(rs_id)
      }
      private$job_id_map <- list()

      invisible()
    }
  )
)

#' @export
update_progress_display.rstudio_progress <- function(x, status) {
  rollup <- function(x) { sum(unlist(x)) }

  ids <- names(status$progress)
  prog <- vapply(status$progress, rollup, numeric(1))
  max <- vapply(status$max, rollup, numeric(1))
  df <- data.frame(
    id = ids,
    progress = prog,
    max = max,
    stringsAsFactors = FALSE
  )

  x$update(df)
}

#' @export
clear_progress_display.rstudio_progress <- function(x) {
  x$close()
}
