##' Format a json object read in via [tinyjson::from_json] into a
##' string. Importantly, and unlike `jsonlite::toJSON` this does not
##' work for arbitrary R objects.
##'
##' @title Convert json object to string
##'
##' @param x A json object to convert
##'
##' @return A string
##'
##' @export
##'
##' @seealso [tinyjson::from_json], which does the inverse
##'   transformation
##'
##' @examples
##' # Lossless json -> R -> json roundtrip of arrays:
##' tinyjson::to_json(tinyjson::from_json('1'))
##' tinyjson::to_json(tinyjson::from_json('[1]'))
##' tinyjson::to_json(tinyjson::from_json('[1, 2, 3, 4]'))
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
