test_that("can convert simple things from json", {
  expect_equal(from_json("1"),
               structure(list(type = "number", value = "1"),
                         class = "tinyjson"))
  expect_equal(from_json("true"),
               structure(list(type = "literal", value = "true"),
                         class = "tinyjson"))
  expect_equal(from_json("false"),
               structure(list(type = "literal", value = "false"),
                         class = "tinyjson"))
  expect_equal(from_json("null"),
               structure(list(type = "literal", value = "null"),
                         class = "tinyjson"))
  expect_equal(from_json('"something"'),
               structure(list(type = "string", value = "something"),
                         class = "tinyjson"))
  expect_equal(from_json('"some \\"thing\\" else"'),
               structure(list(type = "string",
                              value = "some \\\"thing\\\" else"),
                         class = "tinyjson"))
  expect_equal(from_json("[]"),
               structure(list(type = "array", value = list()),
                         class = "tinyjson"))
  expect_equal(from_json("{}"),
               structure(list(type = "object", value = list()),
                         class = "tinyjson"))
  expect_equal(
    from_json("[1, 2]"),
    structure(list(
      type = "array",
      value = list(
        list(type = "number", value = "1"),
        list(type = "number", value = "2"))),
      class = "tinyjson"))
  expect_equal(
    from_json('{"a": 1, "b": 2}'),
    structure(list(
      type = "object",
      value = list(
        list(key = "a", value = list(type = "number", value = "1")),
        list(key = "b", value = list(type = "number", value = "2")))),
      class = "tinyjson"))
})


test_that("error if empty", {
  expect_error(from_json(""),
               "Trying to parse empty json")
})


test_that("error if unconsumed tokens", {
  expect_error(from_json("[1, 2]3"), "Did not consume all tokens")
  ## but whitespace is fine
  expect_equal(from_json("[1, 2]  "), from_json("[1, 2]"))
})


test_that("objects throw parse errors when invalid", {
  expect_error(from_json('{"a": 1, 10: 2}'),
               "Expected string key")
  expect_error(from_json('{"a": 1, "b" 2}'),
               "Expected ':' after an object key")
  expect_error(from_json('{"a": 1 "b": 2}'),
               "Expected a comma after object element")
  expect_error(from_json('{"a": 1, "b": 2'),
               "Ran out of tokens while parsing object")
  expect_error(from_json("{"),
               "Ran out of tokens while parsing object")
})


test_that("arrays throw parse errors when invalid", {
  expect_error(from_json("[1, 2 3]"),
               "Expected a comma after array element")
  expect_error(from_json("[1, 2, 3"),
               "Ran out of tokens while parsing array")
  expect_error(from_json("["),
               "Ran out of tokens while parsing array")
})
