read_tests <- function() {
  d <- from_json(paste(readLines("tests.json"), collapse = "\n"))

  ## Error strings:
  err <- from_json(paste(readLines("tests-errors.json"), collapse = "\n"))
  errors <- vector("list", length(d$value))
  errors[as.integer(vcapply(err$value, function(x) x$key))] <-
    vcapply(err$value, function(x) {
      gsub('\\"', '"', x$value$value, fixed = TRUE)
    })

  read_one <- function(i) {
    el <- d$value[[i]]
    json <- to_json(el)
    key <- vcapply(el$value, "[[", "key")
    doc <- to_json(el$value[[which(key == "doc")]]$value)
    patch <- el$value[[which(key == "patch")]]$value

    comment <- NULL
    expected <- NULL
    error <- NULL
    disabled <- FALSE

    if ("comment" %in% key) {
      comment <- el$value[[which(key == "comment")]]$value
    }
    if ("expected" %in% key) {
      expected <- to_json(el$value[[which(key == "expected")]]$value)
    }
    if ("error" %in% key) {
      error <- el$value[[which(key == "error")]]$value
    }
    if ("disabled" %in% key) {
      disabled <- el$value[[which(key == "disabled")]]$value$value == "true"
    }
    error_r <- errors[[i]]

    list(comment = comment, doc = doc, patch = patch, expected = expected,
         error = error, error_r = error_r, json = json)
  }

  lapply(seq_along(d$value), read_one)
}


apply_patch <- function(x) {
  doc <- from_json(x$doc)
  patch <- x$patch
  for (p in patch$value) {
    el <- object_to_list(p$value)
    path <- el$path$value
    doc <- switch(
      el$op$value,
      add = json_patch_add(doc, path, to_json(el$value)),
      remove = json_patch_remove(doc, path),
      replace = json_patch_replace(doc, path, to_json(el$value)),
      copy = json_patch_copy(doc, el$from$value, path),
      move = json_patch_move(doc, el$from$value, path),
      test = json_patch_test(doc, path, to_json(el$value)),
      stop("Unknown op"))
  }
  doc
}


apply_test <- function(x) {
  if (!is.null(x$expected)) {
    expect_true(is_same(apply_patch(x), from_json(x$expected)))
  } else {
    if (is.null(x$error_r)) {
      browser()
    }
    expect_error(apply_patch(x), x$error_r, fixed = TRUE)
  }
}


object_to_list <- function(x) {
  value <- lapply(x, "[[", "value")
  names(value) <- vcapply(x, "[[", "key")
  value
}
