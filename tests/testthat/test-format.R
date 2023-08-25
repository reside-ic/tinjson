expect_can_roundtrip <- function(x) {
  expect_equal(to_json(from_json(x)), x,
               label = sprintf("Roundtrip of '%s'", x))
}

test_that("can roundtrip simple json", {
  # expect_can_roundtrip("")
  expect_can_roundtrip("[]")
  expect_can_roundtrip("{}")
  expect_can_roundtrip("true")
  expect_can_roundtrip("false")
  expect_can_roundtrip("null")
  expect_can_roundtrip("1")
  expect_can_roundtrip('"something"')
  expect_can_roundtrip("[1,2,3]")
  expect_can_roundtrip('{"a":1,"b":2}')
})
