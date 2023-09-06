##' Parse a json string into a lossless R representation. Unlike
##' `jsonlite::fromJSON` this does not try and deserialise the JSON
##' into conventional R objects, but instead converts the data into a
##' representation that can be inspected, modified and rewritten with
##' [tinyjson::to_json] without any corruption caused by the ambiguity
##' of R's lack of a true scalar type.
##'
##' @title Parse json into lossless R representation
##'
##' @param string A string to parse as json
##'
##' @return An object of class `tinyjson`, which is our lossless
##'   representation of json data.
##'
##' @export
##'
##' @seealso [tinyjson::from_json], which does the inverse
##'   transformation
##'
##' @examples
##' tinyjson::from_json('[1, 2, 3, 4]')
##' tinyjson::from_json('{"a": "value"}')
from_json <- function(string) {
  structure(json_parse_tokens(lex_json(string)), class = "tinyjson")
}


json_parse_tokens <- function(tokens) {
  tokens <- json_tokens(tokens)
  if (tokens$is_complete()) {
    stop("Trying to parse empty json")
  }
  res <- json_parse_any(tokens)
  if (!tokens$is_complete()) {
    stop("Did not consume all tokens")
  }
  res
}


json_parse_any <- function(tokens) {
  t <- tokens$pop("json")
  if (t$type == "syntax" && t$value == "{") {
    json_parse_object(tokens)
  } else if (t$type == "syntax" && t$value == "[") {
    json_parse_array(tokens)
  } else {
    t
  }
}


json_tokens <- function(tokens) {
  tokens <- tokens[vapply(tokens, "[[", "", "type") != "whitespace"]
  pos <- 0L
  len <- length(tokens)
  list(
    is_complete = function() {
      pos == len
    },
    peek = function(current) {
      if (pos == len) {
        stop(sprintf("Ran out of tokens while parsing %s", current))
      }
      tokens[[pos + 1L]]
    },
    pop = function(current) {
      if (pos == len) {
        stop(sprintf("Ran out of tokens while parsing %s", current))
      }
      tokens[[pos <<- pos + 1L]]
    }
  )
}


json_parse_object <- function(tokens) {
  ret <- list(type = "object",
              value = list())

  t <- tokens$peek("object")
  if (t$type == "syntax" && t$value == "}") {
    tokens$pop("object")
    return(ret)
  }

  repeat {
    key <- tokens$pop("object")
    if (key$type != "string") {
      stop("Expected string key")
    }
    t <- tokens$pop("object")
    if (!(t$type == "syntax" && t$value == ":")) {
      stop("Expected ':' after an object key")
    }
    value <- json_parse_any(tokens)
    ret$value[[length(ret$value) + 1]] <- list(key = key$value, value = value)
    t <- tokens$pop("object")
    if (t$type == "syntax" && t$value == "}") {
      return(ret)
    }
    if (!(t$type == "syntax" && t$value == ",")) {
      stop("Expected a comma after object element")
    }
  }
}


json_parse_array <- function(tokens) {
  ret <- list(type = "array",
              value = list())

  t <- tokens$peek("array")
  if (t$type == "syntax" && t$value == "]") {
    tokens$pop("array")
    return(ret)
  }

  repeat {
    ret$value[[length(ret$value) + 1L]] <- json_parse_any(tokens)
    t <- tokens$pop("array")
    if (t$type == "syntax" && t$value == "]") {
      return(ret)
    }
    if (!(t$type == "syntax" && t$value == ",")) {
      stop("Expected a comma after array element")
    }
  }
}
