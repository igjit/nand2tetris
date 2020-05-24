test_that("class_symbol_table works", {
  code <- "class Foo { static int a; static String b; field int c, d; }"
  node <- parse(tokenize(code))
  expected <- tibble(name = c("a", "b", "c", "d"),
                     type = c("int", "String", "int", "int"),
                     kind = c("static", "static", "field", "field"),
                     index = c(0, 1, 0, 1))
  expect_equal(class_symbol_table(node), expected)
})
