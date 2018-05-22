x <- list(
  list(
    list(
      progress = 0,
      max = 1,
      filename = "hi",
      terminated = FALSE
    )
  ),
  list(
    list(
      progress = 1,
      max = 2,
      filename = "hi",
      terminated = FALSE
    )
  )
)

walk_tree <- function(tree, func) {
  lapply(tree, function(x) {
    if (is.list(x) && is.null(names(x))) {
      walk_tree(x, func)
    } else {
      func(x)
    }
  })
}

invert <- function(tree) {
  result <- list()
  uniqueNames <- unique(names(unlist(x)))
  for (name in uniqueNames) {
    result[[name]] <- walk_tree(tree, function(x) {
      list(x[[name]])
    })
  }
  result
}
