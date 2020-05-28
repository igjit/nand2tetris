test_that("class_symbol_table works", {
  code <- "class Foo { static int a; static String b; field int c, d; }"
  node <- parse(tokenize(code))
  expected <- tibble(name = c("a", "b", "c", "d"),
                     type = c("int", "String", "int", "int"),
                     kind = c("static", "static", "field", "field"),
                     index = c(0, 1, 0, 1))
  expect_equal(class_symbol_table(node), expected)

  code <- "class Foo {}"
  node <- parse(tokenize(code))
  expect_equal(nrow(class_symbol_table(node)), 0)
})

test_that("class_symbol_table works", {
  code <- "method void foo(int a, String b) { var int c, d; }"
  class_name <- "Foo"
  node <- parse_subroutine_dec(tokenize(code), new_state())
  expected <- tibble(name = c("this", "a", "b", "c", "d"),
                     type = c("Foo", "int", "String", "int", "int"),
                     kind = c("argument", "argument", "argument", "var", "var"),
                     index = c(0, 1, 2, 0, 1))
  expect_equal(method_symbol_table(node, class_name), expected)
})
