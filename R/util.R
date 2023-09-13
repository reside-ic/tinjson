`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


vcapply <- function(...) {
  vapply(..., FUN.VALUE = "")
}


vlapply <- function(...) {
  vapply(..., FUN.VALUE = logical(1))
}


list_to_logical <- function(x) {
  vlapply(x, identity)
}


last <- function(x) {
  x[[length(x)]]
}
