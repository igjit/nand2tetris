test_that("parse_class works", {
  state <- new_state()
  expect_s3_class(parse_class(tokenize("class Main {}"), state), "class_node")
  expect_equal(state$i, 5)

  node <- parse_class(tokenize("class Main { static char a; field int x; }"),
                      new_state())
  class_var_dec_nodes <- keep(node$elements, ~ is(., "class_var_dec_node"))
  expect_equal(length(class_var_dec_nodes), 2)
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

test_that("parse_statements works", {
  state <- new_state()
  tokens <- tokenize("let a = x; return;")
  node <- parse_statements(tokens, state)
  expect_s3_class(node, "statements_node")
  expect_s3_class(node$elements[[1]], "let_statement_node")
  expect_s3_class(node$elements[[2]], "return_statement_node")
  expect_equal(state$i, 8)
})

test_that("parse_let_statement works", {
  state <- new_state()
  tokens <- tokenize("let a = x;")
  expect_s3_class(parse_let_statement(tokens, state), "let_statement_node")
  expect_equal(state$i, 6)

  state <- new_state()
  tokens <- tokenize("let b[i] = x;")
  expect_s3_class(parse_let_statement(tokens, state), "let_statement_node")
  expect_equal(state$i, 9)
})

test_that("parse_return_statement works", {
  state <- new_state()
  tokens <- tokenize("return;")
  expect_s3_class(parse_return_statement(tokens, state), "return_statement_node")
  expect_equal(state$i, 3)

  state <- new_state()
  tokens <- tokenize("return x;")
  expect_s3_class(parse_return_statement(tokens, state), "return_statement_node")
  expect_equal(state$i, 4)
})

test_that("parse_expression works", {
  state <- new_state()
  token <- identifier_token("x")
  node <- parse_expression(list(token), state)
  expect_s3_class(node, "expression_node")
  expect_equal(node$elements[[1]]$elements[[1]], token)
  expect_equal(state$i, 2)
})

test_that("is_type works", {
  expect_true(is_type(keyword_token("int")))
  expect_true(is_type(identifier_token("Array")))
  expect_false(is_type(keyword_token("null")))
})
