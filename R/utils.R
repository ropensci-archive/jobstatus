# Cheap hash name generator
rhash <- function(n = 20) {
  paste (c ("jst", sample(c (LETTERS, letters, 0:9), n, TRUE)), collapse = "")
}
