to_json <- function(x) {
  if (x$type == "array") {
    contents <- vcapply(x$value, to_json)
    sprintf("[%s]", paste(contents, collapse = ","))
  } else if (x$type == "object") {
    contents <- vcapply(x$value, function(el) {
      sprintf('"%s":%s', el$key, to_json(el$value))
    })
    sprintf("{%s}", paste(contents, collapse = ","))
  } else if (x$type == "string") {
    sprintf('"%s"', x$value)
  } else if (x$type %in% c("literal", "number")) {
    x$value
  }
}
