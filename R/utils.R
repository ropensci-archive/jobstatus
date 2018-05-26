# Cheap hash name generator
rhash <- function(n = 20) {
  paste (c ("jst", sample(c (LETTERS, letters, 0:9), n, TRUE)), collapse = "")
}

write <- function (filename, ...) {
  f <- file(filename, open = "w")
  x <- serialize(..., f)
  close(f)
  invisible()
}

read <- function (filename, ...) {
  f <- file(filename, open = "r")
  ans <- unserialize(f)
  close(f)
  ans
}

empty_status = function (filename = "") {
  status <- list(
    filename = filename,
    terminated = FALSE,
    progress = 0,
    max = 10
  )
  status
}

# invert one layer of the tree (run at each read step)
invert <- function (x) {
  names <- names(x[[1]])
  result <- list()
  for (name in names) {
    elems <- lapply(x, "[[", name)
    result[[name]] <- elems
  }
  result
}
