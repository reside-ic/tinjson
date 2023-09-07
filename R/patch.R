json_patch_remove <- function(json, path) {
  patch_remove(json, json_pointer(json, path))
}


json_patch_replace <- function(json, path, value) {
  patch_replace(json, json_pointer(json, path), unclass(from_json(value)))
}


json_patch_add <- function(json, path, value) {
  patch_add(json, json_pointer(json, path), unclass(from_json(value)))
}


json_patch_copy <- function(json, from, path) {
  ptr_from <- json_pointer(json, from)
  ptr_path <- json_pointer(json, path)
  patch_copy(json, ptr_from, ptr_path)
}


json_patch_move <- function(json, from, path) {
  ptr_from <- json_pointer(json, from)
  if (from == path) {
    return(json)
  }
  ptr_path <- json_pointer(json, path)
  patch_move(json, ptr_from, ptr_path)
}


json_patch_test <- function(json, path, value) {
  patch_test(json, json_pointer(json, path), from_json(value))
}


patch_remove <- function(json, ptr) {
  if (last(ptr$element) > length(json[[ptr$container]])) {
    stop(sprintf("Trying to remove non-existant entry '%s'", ptr$path))
  }
  json[[ptr$element]] <- NULL
  json
}


patch_replace <- function(json, ptr, value) {
  if (length(ptr$value) == 0) {
    json <- value
  } else {
    json[[ptr$value]] <- value
  }
  json
}


patch_add <- function(json, ptr, value) {
  if (length(ptr$value) == 0) {
    json <- value
  } else if (ptr$type == "array") {
    i <- last(ptr$element) - 1L
    arr <- json[[ptr$container]]
    n <- length(arr)
    if (i == n) {
      json[[ptr$container]] <- c(arr, list(value))
    } else if (i == 0) {
      json[[ptr$container]] <- c(list(value), arr)
    } else {
      json[[ptr$container]] <- c(arr[seq_len(i)], list(value), arr[-seq_len(i)])
    }
  } else {
    i <- last(ptr$element)
    n <- length(json[[ptr$container]])
    if (i > n) {
      json[[c(ptr$container, i)]] <- list(key = last(ptr$parts), value = value)
    } else {
      json[[ptr$value]] <- value
    }
  }
  json
}


patch_copy <- function(json, ptr_from, ptr_path) {
  patch_add(json, ptr_path, json[[ptr_from$value]])
}


patch_move <- function(json, ptr_from, ptr_path) {
  is_same_array <- ptr_from$type == "array" && ptr_path$type == "array" &&
    identical(ptr_from$container, ptr_path$container)
  if (is_same_array) {
    ## This case is a bit weird, I've verified with
    ## https://jsonpatch.me/ but the actual spec tests don't include
    ## an example.
    value <- json[[ptr_from$element]]
    json <- patch_remove(json, ptr_from)
    patch_add(json, ptr_path, value)
  } else {
    json <- patch_copy(json, ptr_from, ptr_path)
    patch_remove(json, ptr_from)
  }
}

patch_test <- function(json, ptr, value) {
  found <- if (length(ptr$value) == 0) json else json[[ptr$value]]
  if (!is_same(found, value)) {
    stop(sprintf("Patch test failed: expected '%s' but found '%s'",
                 to_json(value), to_json(found)))
  }
  json
}


is_same <- function(a, b) {
  if (!identical(a$type, b$type)) {
    return(FALSE)
  }
  if (a$type == "array") {
    length(a$value) == length(b$value) &&
      all(list_to_logical(Map(is_same, a$value, b$value)))
  } else if (a$type == "object") {
    if (identical(a$value, b$value)) {
      return(TRUE)
    }
    key_a <- vcapply(a$value, "[[", "key")
    key_b <- vcapply(b$value, "[[", "key")
    if (!setequal(key_a, key_b)) {
      return(FALSE)
    }
    all(list_to_logical(Map(
      is_same,
      lapply(a$value, "[[", "value"),
      lapply(b$value, "[[", "value")[match(key_a, key_b)])))
  } else {
    identical(a$value, b$value)
  }
}
