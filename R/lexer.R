lex_json <- function(string) {
  chomp <- json_reader(string)
  tokens <- list()
  while (!is.null(t <- chomp())) {
    tokens[[length(tokens) + 1L]] <- t
  }
  tokens
}

JSON_NUMBER_START <- c(as.character(0:9), ".", "-")
JSON_NUMBER <- c(JSON_NUMBER_START, "e")
JSON_WHITESPACE = c(" ", "\t", "\b", "\n", "\r")
JSON_SYNTAX = c(",", ":", "[", "]", "{", "}")

json_element <- function(type, value) {
  list(type = type, value = value)
}

json_reader <- function(string) {
  idx_quote <- c(gregexec('(?<!\\\\)"', string, perl = TRUE)[[1]])
  chars <- strsplit(string, NULL)[[1]]
  pos <- 1L
  len <- length(chars)

  chomp_chars <- function(type, include) {
    i <- pos
    while (i <= len && any(chars[[i]] == include)) {
      i <- i + 1L
    }
    value <- substr(string, pos, i - 1)
    pos <<- i
    json_element(type, value)
  }

  chomp_string <- function() {
    to <- idx_quote[idx_quote > pos]
    if (length(to) == 0) {
      stop("Expected end of string")
    }
    to <- to[[1L]]
    value <- substr(string, pos + 1L, to - 1L)
    pos <<- to + 1L
    json_element("string", value)
  }

  chomp_literal_or_fail <- function() {
    value <- substr(string, pos, pos + 3L)
    if (value != "null" && value != "true") {
      value <- substr(string, pos, pos + 4L)
      if (value != "false") {
        stop(sprintf("Unexpected token at pos %d", pos))
      }
    }
    pos <<- pos + nchar(value)
    json_element("literal", value)
  }

  function() {
    if (pos > length(chars)) {
      return(NULL)
    }
    cur <- chars[[pos]]
    if (any(cur == JSON_SYNTAX)) {
      pos <<- pos + 1L
      json_element("syntax", cur)
    } else if (cur == '"') {
      chomp_string()
    } else if (any(cur == JSON_WHITESPACE)) {
      chomp_chars("whitespace", JSON_WHITESPACE)
    } else if (cur %in% JSON_NUMBER_START) {
      chomp_chars("number", JSON_NUMBER)
    } else {
      chomp_literal_or_fail()
    }
  }
}
