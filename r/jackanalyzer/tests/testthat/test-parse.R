test_that("parse_class works", {
  state <- new_state()
  expect_s3_class(parse_class(tokenize("class Main {}"), state), "class_node")
  expect_equal(state$i, 5)

  node <- parse_class(tokenize("class Main { static char a; field int x; }"),
                      new_state())
  class_var_dec_nodes <- keep(node$elements, ~ is(., "class_var_dec_node"))
  expect_equal(length(class_var_dec_nodes), 2)

  tokens <- tokenize("class Main { function int foo(int x) {  return x; } }")
  expect_s3_class(parse_class(tokens, new_state())$elements[[4]], "subroutine_dec_node")
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

test_that("parse_subroutine_dec works", {
  state <- new_state()
  tokens <- tokenize("function int foo(int x) {  return x; }")
  expect_s3_class(parse_subroutine_dec(tokens, state), "subroutine_dec_node")
  expect_equal(state$i, 13)
})

test_that("parse_subroutine_body works", {
  state <- new_state()
  tokens <- tokenize("{ return x; }")
  expect_s3_class(parse_subroutine_body(tokens, state), "subroutine_body_node")
  expect_equal(state$i, 6)

  state <- new_state()
  tokens <- tokenize("{ var int x; return x; }")
  expect_s3_class(parse_subroutine_body(tokens, state), "subroutine_body_node")
  expect_equal(state$i, 10)

  state <- new_state()
  tokens <- tokenize("{ var int x; var char c; return x; }")
  expect_s3_class(parse_subroutine_body(tokens, state), "subroutine_body_node")
  expect_equal(state$i, 14)
})

test_that("parse_var_dec works", {
  state <- new_state()
  tokens <- tokenize("var int x;")
  expect_s3_class(parse_var_dec(tokens, state), "var_dec_node")
  expect_equal(state$i, 5)

  state <- new_state()
  tokens <- tokenize("var int x, y;")
  expect_s3_class(parse_var_dec(tokens, state), "var_dec_node")
  expect_equal(state$i, 7)
})

test_that("parse_parameter_list works", {
  state <- new_state(i = 2)
  tokens <- tokenize("()")
  expect_s3_class(parse_parameter_list(tokens, state), "parameter_list_node")
  expect_equal(state$i, 2)

  state <- new_state(i = 2)
  tokens <- tokenize("(int a)")
  expect_s3_class(parse_parameter_list(tokens, state), "parameter_list_node")
  expect_equal(state$i, 4)

  state <- new_state(i = 2)
  tokens <- tokenize("(int a, int b)")
  expect_s3_class(parse_parameter_list(tokens, state), "parameter_list_node")
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

test_that("parse_if_statement works", {
  state <- new_state()
  tokens <- tokenize("if (x) { return a; }")
  expect_s3_class(parse_if_statement(tokens, state), "if_statement_node")
  expect_equal(state$i, 10)

  state <- new_state()
  tokens <- tokenize("if (x) { return a; } else { return b; }")
  expect_s3_class(parse_if_statement(tokens, state), "if_statement_node")
  expect_equal(state$i, 16)
})

test_that("parse_while_statement works", {
  state <- new_state()
  tokens <- tokenize("while (x) { do foo(); }")
  expect_s3_class(parse_while_statement(tokens, state), "while_statement_node")
  expect_equal(state$i, 12)
})

test_that("parse_do_statement works", {
  state <- new_state()
  tokens <- tokenize("do foo();")
  expect_s3_class(parse_do_statement(tokens, state), "do_statement_node")
  expect_equal(state$i, 6)
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

  state <- new_state()
  tokens <- tokenize("x + y")
  node <- parse_expression(tokens, state)
  expect_s3_class(node, "expression_node")
  expect_equal(node$elements[[1]]$elements[[1]], identifier_token("x"))
  expect_equal(node$elements[[2]], symbol_token("+"))
  expect_equal(node$elements[[3]]$elements[[1]], identifier_token("y"))
  expect_equal(state$i, 4)
})

expect_parse_term <- function(token) {
  state <- new_state()
  node <- parse_term(list(token), state)
  expect_s3_class(node, "term_node")
  expect_equal(node$elements[[1]], token)
  expect_equal(state$i, 2)
}

test_that("parse_term works", {
  expect_parse_term(int_const_token(12))
  expect_parse_term(string_const_token("hello"))
  expect_parse_term(keyword_token("true"))
  expect_parse_term(identifier_token("x"))

  state <- new_state()
  node <- parse_term(tokenize("x[1]"), state)
  expect_s3_class(node, "term_node")
  expect_equal(state$i, 5)

  state <- new_state()
  tokens <- tokenize("foo()")
  expect_s3_class(parse_term(tokens, state), "term_node")
  expect_equal(state$i, 4)
})

test_that("parse_subroutine_call works", {
  state <- new_state()
  tokens <- tokenize("bar()")
  expect_type(parse_subroutine_call(tokens, state), "list")
  expect_equal(state$i, 4)

  state <- new_state()
  tokens <- tokenize("foo.bar()")
  expect_type(parse_subroutine_call(tokens, state), "list")
  expect_equal(state$i, 6)
})

test_that("parse_expression_list works", {
  state <- new_state(i = 2)
  token <- tokenize("()")
  expect_s3_class(parse_expression_list(token, state), "expression_list_node")
  expect_equal(state$i, 2)

  state <- new_state(i = 2)
  token <- tokenize("(a)")
  expect_s3_class(parse_expression_list(token, state), "expression_list_node")
  expect_equal(state$i, 3)

  state <- new_state(i = 2)
  token <- tokenize("(a, b)")
  expect_s3_class(parse_expression_list(token, state), "expression_list_node")
  expect_equal(state$i, 5)
})

test_that("is_type works", {
  expect_true(is_type(keyword_token("int")))
  expect_true(is_type(identifier_token("Array")))
  expect_false(is_type(keyword_token("null")))
})
