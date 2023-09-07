test_that("spec tests pass", {
  ignore <- c(57,            # spec does not include expectation
              75, 76, 78:87) # patch json unmarshalling
  tests <- read_tests()
  for (i in setdiff(seq_along(tests), ignore)) {
    apply_test(tests[[i]])
  }
})


test_that("can move an element within the same array", {
  json <- from_json('{"a": [1, 2, 3, 4]}')
  expect_true(is_same(
    json_patch_move(json, "/a/0", "/a/2"),
    from_json('{"a": [2, 3, 1, 4]}')))
})


test_that("can test if two json documents are the same", {
  is_same_json <- function(a, b) {
    is_same(from_json(a), from_json(b))
  }

  expect_true(is_same_json("true", "true"))
  expect_true(is_same_json("false", "false"))
  expect_true(is_same_json("null", "null"))
  expect_false(is_same_json("true", "false"))
  expect_false(is_same_json("false", "null"))

  expect_true(is_same_json("1", "1"))
  expect_false(is_same_json("1", "2"))

  expect_true(is_same_json('"a"', '"a"'))
  expect_false(is_same_json('"a"', '"b"'))

  expect_true(is_same_json("[1,2,3]", "[1,2,3]"))
  expect_true(is_same_json("[1,2,3]", "[1, 2, 3]"))
  expect_false(is_same_json("[1,2,3]", "[1,2,3,4]"))
  expect_false(is_same_json("[1,2,3]", "[1,2,4]"))
  expect_false(is_same_json("[1,2,3]", "[3,2,1]"))

  expect_true(is_same_json('{"a": 1, "b": 2}', '{"a": 1, "b": 2}'))
  expect_true(is_same_json('{"a": 1, "b": 2}', '{"b": 2, "a": 1}'))
  expect_false(is_same_json('{"a": 1, "b": 2}', '{"b": 1, "a": 2}'))
  expect_false(is_same_json('{"a": 1, "b": 2}', '{"a": 1, "c": 2}'))
  expect_false(is_same_json('{"a": 1, "b": 2}', '{"a": 1, "b": 2, "c": 3}'))
})
