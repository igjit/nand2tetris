test_that("tokenize works", {
  expect_equal(tokenize("class Main"),
               list(keyword_token("class"),
                    identifier_token("Main")))
  expect_equal(tokenize("Array.new(length)"),
               list(identifier_token("Array"),
                    symbol_token("."),
                    identifier_token("new"),
                    symbol_token("("),
                    identifier_token("length"),
                    symbol_token(")")))
  expect_equal(tokenize('a=1+2'),
               list(identifier_token("a"),
                    symbol_token("="),
                    int_const_token(1),
                    symbol_token("+"),
                    int_const_token(2)))
  expect_equal(tokenize('"hello world"'),
               list(string_const_token("hello world")))
})
