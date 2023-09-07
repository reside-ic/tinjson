##' Patch json documents. This set of functions presents an R-ish
##' interface to all the json patch verbs (add, remove, copy, replace,
##' move, and test). It does not implement the json-format patch
##' (i.e., a patch represented itself as a json document). The primary
##' use case here is to allow expressing modifications to a json
##' document from R without having to deserialise the document into
##' R's data structures, so that a "json -> R -> json" roundtrip can
##' be done losslessly, with modification in R.
##'
##' This interface is designed to play nicely with R's piping
##' operator, see examples.
##'
##' The verbs are:
##'
##' ## Add (`json_patch_add`)
##'
##' Adds a value to an object or inserts it into an array. In the case
##' of an array, the value is inserted before the given index. The `-`
##' character can be used instead of an index to insert at the end of
##' an array.  Note that the first element has index `0` (not `1`).
##'
##' ## Remove (`json_patch_remove`)
##'
##' Removes a value from an object or array.
##'
##' ## Replace (`json_patch_replace`)
##'
##' Replaces a value. Equivalent to a `remove` followed by an `add`.
##'
##' ## Copy (`json_patch_copy`)
##'
##' Copies a value from one location to another within the JSON
##' document.
##'
##' ## Move (`json_patch_move`)
##'
##' Moves a value from one location to the other.
##'
##' ## Test (`json_patch_test`)
##'
##' Tests that the specified value is set in the document. If the test
##' fails, then an error is thrown, so can be used within a pipeline
##' to prevent a patch applying.
##'
##' @title JSON Patch
##'
##' @param json The json document to work with
##'
##' @param path The patch to modify (or for `json_patch_test`, to test)
##'
##' @param value The value to add (`json_patch_add`), replace an
##'   existing value with (`json_patch_replace`) or test
##'   (`json_patch_test`)
##'
##' @param from The source value for `json_patch_move` and
##'   `json_patch_copy`
##'
##' @rdname json_patch
##'
##' @seealso The json patch specification for more details, at
##'   https://jsonpatch.com/
##'
##' @return The modified json document (in parsed form) or throws if
##'   impossible
##'
##' @export
##' @examples
##'
##' # The example doc used in https://jsonpatch.com/
##' doc <- tinyjson::from_json('{
##'   "biscuits": [
##'     { "name": "Digestive" },
##'     { "name": "Choco Leibniz" }
##'    ]
##' }')
##'
##' # add
##' tinyjson::json_patch_add(doc, "/biscuits/1", '{"name": "Ginger Nut"}') |>
##'   tinyjson::to_json()
##'
##' # remove
##' tinyjson::json_patch_remove(doc, "/biscuits") |> tinyjson::to_json()
##' tinyjson::json_patch_remove(doc, "/biscuits/0") |> tinyjson::to_json()
##'
##' # replace (note the extra quotes here, the 'value' gets parsed as
##' # json and is not an R string)
##' tinyjson::json_patch_replace(
##'   doc, "/biscuits/0/name", '"Chocolate Digestive"') |> tinyjson::to_json()
##'
##' # copy
##' tinyjson::json_patch_copy(doc, "/biscuits/0", "/best_biscuit") |>
##'   tinyjson::to_json()
##'
##' # move
##' tinyjson::json_patch_copy(doc, "/biscuits", "/cookies") |>
##'   tinyjson::to_json()
##'
##' # test
##' tinyjson::json_patch_test(doc, "/biscuits/1/name", '"Choco Leibniz"') |>
##'   tinyjson::to_json()
##'
json_patch_remove <- function(json, path) {
  patch_remove(json, json_pointer(json, path))
}


##' @export
##' @rdname json_patch
json_patch_replace <- function(json, path, value) {
  patch_replace(json, json_pointer(json, path), unclass(from_json(value)))
}


##' @export
##' @rdname json_patch
json_patch_add <- function(json, path, value) {
  patch_add(json, json_pointer(json, path), unclass(from_json(value)))
}


##' @export
##' @rdname json_patch
json_patch_copy <- function(json, from, path) {
  ptr_from <- json_pointer(json, from)
  ptr_path <- json_pointer(json, path)
  patch_copy(json, ptr_from, ptr_path)
}


##' @export
##' @rdname json_patch
json_patch_move <- function(json, from, path) {
  ptr_from <- json_pointer(json, from)
  if (from == path) {
    return(json)
  }
  ptr_path <- json_pointer(json, path)
  patch_move(json, ptr_from, ptr_path)
}


##' @export
##' @rdname json_patch
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
