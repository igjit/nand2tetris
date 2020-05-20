test_that("parse_class works", {
  state <- new_state()
  expect_s3_class(parse_class(tokenize("class Main {}"), state), "class_node")
  expect_equal(state$i, 5)
})

test_that("parse_class_var_dec works", {
  state <- new_state()
  tokens <- tokenize("static char x;")
  expect_equal(parse_class_var_dec(tokens, state), class_var_dec_node(tokens))
  expect_equal(state$i, 5)

  state <- new_state()
  tokens <- tokenize("field int x, y;")
  expect_equal(parse_class_var_dec(tokens, state), class_var_dec_node(tokens))
  expect_equal(state$i, 7)
})

test_that("is_type works", {
  expect_true(is_type(keyword_token("int")))
  expect_true(is_type(identifier_token("Array")))
  expect_false(is_type(keyword_token("null")))
})
