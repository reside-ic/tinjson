json_pointer <- function(json, path) {
  if (path == "") {
    return(new_pointer(character(), json$type, integer()))
  }
  if (!grepl("/", path)) {
    stop(sprintf("Pointer must start with '/', but was given '%s'", path))
  }
  parts <- strsplit(path, "/")[[1]][-1]
  parts <- gsub("~0", "~", gsub("~1", "/", parts, fixed = TRUE), fixed = TRUE)
  if (grepl("/$", path)) {
    parts <- c(parts, "")
  }
  res <- json_find(json, parts, character())
  path_value <- unlist(lapply(res, "[[", "ipath"))
  type <- res[[length(res)]]$type
  new_pointer(parts, type, path_value)
}


json_find <- function(json, path, parent) {
  ## We need to make the distinction between the element and the value
  ## here; these are the same for arrays, but different for arrays.
  ret <- integer(0)
  if (length(path) == 0) {
    return(NULL)
  }
  p <- path[[1]]
  if (json$type == "object") {
    i <- which(p == vcapply(json$value, function(el) el$key))
    if (length(i) != 1) {
      if (length(path) > 1) {
        stop(sprintf("Did not find key '%s' in object '%s'",
                     p, paste0("/", parent, collapse = "")))
      }
      i <- length(json$value) + 1L
    }
    ipath <- c(2L, i, 2L)
  } else if (json$type == "array") {
    n <- length(json$value)
    if (p == "-") {
      i <- n
    } else {
      is_number <- grepl("^(0|[1-9]+[0-9]*)$", p) # no leading zeros
      if (!is_number) {
        stop(sprintf(
          "Expected a numeric index as '%s' is an array, but was given '%s'",
          paste0("/", parent, collapse = ""), p))
      }
      i <- as.integer(p)
      if (i < 0 || i > n || (length(path) > 1 && i == n)) {
        stop(sprintf("Out of bounds array access at '%s' - must be in [0, %d]",
                     paste0("/", c(parent, p), collapse = ""), n))
      }
    }
    ipath <- c(2L, i + 1L)
  } else {
    stop(sprintf("Trying to index into atomic type '%s' at '%s'",
                 json$type, paste0("/", c(parent, p), collapse = "")))
  }
  c(list(list(type = json$type, ipath = ipath)),
    json_find(json[[ipath]], path[-1], c(parent, p)))
}


new_pointer <- function(parts, type, value) {
  path <- paste0(rep_len("/", length(parts)), parts, collapse = "")
  element <- container <- value
  n <- length(value)
  if (n > 0) {
    if (type == "array") {
      element <- value
      container <- value[-n]
    } else {
      element <- value[-n]
      container <- value[-c(n - 1, n)]
    }
  }
  ret <- list(path = path, parts = parts, type = type,
              value = value, element = element, container = container)
  class(ret) <- "jsonpointer"
  ret
}
