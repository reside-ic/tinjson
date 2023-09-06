test_that("simple cases", {
  expect_equal(
    lex_json(""),
    list())
  expect_equal(
    lex_json("1"),
    list(json_element("number", "1")))
  expect_equal(
    lex_json("true"),
    list(json_element("literal", "true")))
  expect_equal(
    lex_json("false"),
    list(json_element("literal", "false")))
  expect_equal(
    lex_json("null"),
    list(json_element("literal", "null")))
  expect_equal(
    lex_json('"something"'),
    list(json_element("string", "something")))
  expect_equal(
    lex_json("[]"),
    list(json_element("syntax", "["),
         json_element("syntax", "]")))
  expect_equal(
    lex_json("{}"),
    list(json_element("syntax", "{"),
         json_element("syntax", "}")))
  expect_equal(
    lex_json("[1, 2]"),
    list(json_element("syntax", "["),
         json_element("number", "1"),
         json_element("syntax", ","),
         json_element("whitespace", " "),
         json_element("number", "2"),
         json_element("syntax", "]")))
  expect_equal(
    lex_json('["first", "second"]'),
    list(json_element("syntax", "["),
         json_element("string", "first"),
         json_element("syntax", ","),
         json_element("whitespace", " "),
         json_element("string", "second"),
         json_element("syntax", "]")))

  expect_equal(
    lex_json("[null, true, false]"),
    list(json_element("syntax", "["),
         json_element("literal", "null"),
         json_element("syntax", ","),
         json_element("whitespace", " "),
         json_element("literal", "true"),
         json_element("syntax", ","),
         json_element("whitespace", " "),
         json_element("literal", "false"),
         json_element("syntax", "]")))
})


## We never actually expect to fail; these would need improvement.
test_that("handle error cases with some grace", {
  expect_error(lex_json('"unterminated'), "Expected end of string")
  expect_error(lex_json("falser"), "Unexpected token at pos 6")
})
