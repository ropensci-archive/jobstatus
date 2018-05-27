list(
  progress = list(1, 2, 3),
  max = list(10, 10, 10)
)

calculate_ratio <- function(status) {
  num <- unlist(status$progress)
  denom <- unlist(status$max)
  mean(num / denom)
}

#' @export
update_progress_display <- function(x, status) {
  UseMethod("update_progress_display")
}

#' @export
clear_progress_display <- function(x) {
  UseMethod("clear_progress_display")
}

# percentage implementation =======================

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
  msg <- sprintf("\r  total: %s%%\t\t%s",
                 round(100 * overall),
                 paste(job_progress, collapse = "   "))
  cat(msg)
}

#' @export
clear_progress_display.percentage <- function(x) {
  flush.console()
}

# progress_bar implementation =======================

# no total for now, just the component jobs

# just use the progress bar from the progress package; but ignore total
progress_bar <- progress::progress_bar

#' @export
update_progress_display.progress_bar <- function(x, status) {

  # given a tempalte progress bar x, and a status object, create a smaller progress bar for each job

  x_private <- x$.__enclos_env__$private

  # get progress statistics
  progress <- unlist(status$progress)
  max <- unlist(status$max)
  fractions <- progress / max

  # work out the individual bar width from the overall width in x
  total_width <-   terminal_width <- options()$width
  n_bars <- length(status$progress)
  # total_width <- total_width - (n_bars - 1)
  bar_width <- total_width %/% n_bars - 2
  x_private$width <- bar_width

  # fix some things to stabilise the plotting
  x_private$total <- 100
  x_private$clear = FALSE
  x_private$show_after = 0

  # output text
  f <- tempfile()
  file.create(f)
  con <- file(f, "r+")
  x_private$stream <- con
  on.exit(close(con))

  # loop through the bars, getting a string for the status

  capture_bar <- function (fraction) {
    # never let it close
    x_private$complete <- FALSE
    # update the ratio to render
    x$update(ratio = fraction)
    line <- suppressWarnings(readLines(con))[2]

    # x$update(0)

    # flush the connection
    flush.connection(con)
    if (is.na(line)) {
      line <- ""
    }
    line
  }

  bar_text <- vapply(fractions, capture_bar, "")

  text <- paste(bar_text, collapse = "    ")
  cat("\r", text)

  invisible()

}

#' @export
clear_progress_display.progress_bar <- function(x) {

  flush.console()

}

# rstudio_progress implementation =======================


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

  ids <- seq_along(status$progress)
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
