test_that("parse_class works", {
  state <- new_state()
  expect_s3_class(parse_class(tokenize("class Main {}"), state), "class_node")
  expect_equal(state$i, 5)
})

test_that("is_type works", {
  expect_true(is_type(keyword_token("int")))
  expect_true(is_type(identifier_token("Array")))
  expect_false(is_type(keyword_token("null")))
})
