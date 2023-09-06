lex_json <- function(string) {
  chomp <- json_reader(string)
  tokens <- list()
  while (!is.null(t <- chomp())) {
    tokens[[length(tokens) + 1L]] <- t
  }
  tokens
}

json_number_start <- c(as.character(0:9), ".", "-")
json_number <- c(json_number_start, "e")
json_whitespace <- c(" ", "\t", "\b", "\n", "\r")
json_syntax <- c(",", ":", "[", "]", "{", "}")

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
    if (any(cur == json_syntax)) {
      pos <<- pos + 1L
      json_element("syntax", cur)
    } else if (cur == '"') {
      chomp_string()
    } else if (any(cur == json_whitespace)) {
      chomp_chars("whitespace", json_whitespace)
    } else if (cur %in% json_number_start) {
      chomp_chars("number", json_number)
    } else {
      chomp_literal_or_fail()
    }
  }
}
