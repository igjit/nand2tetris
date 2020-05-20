test_that("parse_class works", {
  state <- new_state()
  expect_s3_class(parse_class(tokenize("class Main {}"), state), "class_node")
  expect_equal(state$i, 5)
})
