test_that("RFC 6901 tests", {
  ## https://datatracker.ietf.org/doc/html/rfc6901 - page 5
  str <- '{
      "foo": ["bar", "baz"],
      "": 0,
      "a/b": 1,
      "c%d": 2,
      "e^f": 3,
      "g|h": 4,
      "i\\\\j": 5,
      "k\\"l": 6,
      " ": 7,
      "m~n": 8
   }'
  json <- from_json(str)

  expect_equal(
    json_pointer(json, ""),
    new_pointer(character(), "object", integer()))

  expect_equal(
    json_pointer(json, "/foo"),
    new_pointer("foo", "object", c(2, 1, 2)))
  expect_equal(json[[json_pointer(json, "/foo")$value]],
               unclass(from_json('["bar", "baz"]')))

  expect_equal(
    json_pointer(json, "/foo/0"),
    new_pointer(c("foo", "0"), "array", c(2, 1, 2, 2, 1)))
  expect_equal(json[[json_pointer(json, "/foo/0")$value]],
               unclass(from_json('"bar"')))

  expect_equal(
    json_pointer(json, "/"),
    new_pointer("", "object", c(2, 2, 2)))
  expect_equal(json[[json_pointer(json, "/")$value]],
               unclass(from_json("0")))

  expect_equal(
    json_pointer(json, "/a~1b"),
    new_pointer("a/b", "object", c(2, 3, 2)))
  expect_equal(json[[json_pointer(json, "/a~1b")$value]],
               unclass(from_json("1")))

  expect_equal(
    json_pointer(json, "/c%d"),
    new_pointer("c%d", "object", c(2, 4, 2)))
  expect_equal(json[[json_pointer(json, "/c%d")$value]],
               unclass(from_json("2")))

  expect_equal(
    json_pointer(json, "/e^f"),
    new_pointer("e^f", "object", c(2, 5, 2)))
  expect_equal(json[[json_pointer(json, "/e^f")$value]],
               unclass(from_json("3")))

  expect_equal(
    json_pointer(json, "/g|h"),
    new_pointer("g|h", "object", c(2, 6, 2)))
  expect_equal(json[[json_pointer(json, "/g|h")$value]],
               unclass(from_json("4")))

  expect_equal(
    json_pointer(json, "/i\\\\j"),
    new_pointer("i\\\\j", "object", c(2, 7, 2)))
  expect_equal(json[[json_pointer(json, "/i\\\\j")$value]],
               unclass(from_json("5")))

  expect_equal(
    json_pointer(json, '/k\\"l'),
    new_pointer('k\\"l', "object", c(2, 8, 2)))
  expect_equal(json[[json_pointer(json, '/k\\"l')$value]],
               unclass(from_json("6")))

  expect_equal(
    json_pointer(json, "/ "),
    new_pointer(" ", "object", c(2, 9, 2)))
  expect_equal(json[[json_pointer(json, "/ ")$value]],
               unclass(from_json("7")))

  expect_equal(
    json_pointer(json, "/m~0n"),
    new_pointer("m~n", "object", c(2, 10, 2)))
  expect_equal(json[[json_pointer(json, "/m~0n")$value]],
               unclass(from_json("8")))
})


test_that("pointers must be absolute paths", {
  expect_error(json_pointer(from_json("[1,2,3,4]"), "2"),
               "Pointer must start with '/', but was given '2'")
})


test_that("meaningful errors where find fails", {
  json <- from_json('{"a": [1, 2, 3], "b": {"x": 1, "y": true}}')
  expect_equal(
    json_pointer(json, "/x"),
    new_pointer("x", "object", c(2, 3, 2)))
  expect_error(json_pointer(json, "/x/y"),
               "Did not find key 'x' in object '/'")
  expect_error(json_pointer(json, "/b/z/y"),
               "Did not find key 'z' in object '/b'")
  expect_error(json_pointer(json, "/b/x/y"),
               "Trying to index into atomic type 'number' at '/b/x/y'")
  expect_equal(
    json_pointer(json, "/a/3"),
    new_pointer(c("a", "3"), "array", c(2, 1, 2, 2, 4)))
  expect_equal(
    json_pointer(json, "/a/-"),
    new_pointer(c("a", "-"), "array", c(2, 1, 2, 2, 4)))
  expect_error(json_pointer(json, "/a/10"),
               "Out of bounds array access at '/a/10' - must be in [0, 3]",
               fixed = TRUE)
  expect_error(
    json_pointer(json, "/a/-5"),
    "Expected a numeric index as '/a' is an array, but was given '-5'")
  expect_error(
    json_pointer(json, "/a/00"),
    "Expected a numeric index as '/a' is an array, but was given '00'")
  expect_error(
    json_pointer(json, "/a/1e0"),
    "Expected a numeric index as '/a' is an array, but was given '1e0'")
  expect_error(
    json_pointer(json, "/a/01"),
    "Expected a numeric index as '/a' is an array, but was given '01'")
})
