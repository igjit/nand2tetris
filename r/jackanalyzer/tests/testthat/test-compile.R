expect_compiled <- function(jack) {
  vm <- str_replace(jack, ".jack$", ".vm")
  compiled <- read_jack(jack) %>% tokenize %>% parse %>% compile
  expect_equal(compiled, readLines(vm))
}

test_that("compile works", {
  expect_compiled(file.path("data", "IntConstant", "Main.jack"))
  expect_compiled(file.path("data", "Arithmetic", "Main.jack"))
  expect_compiled(file.path("data", "Variable", "Main.jack"))
  expect_compiled(file.path("data", "Function", "Main.jack"))
  expect_compiled(file.path("data", "Boolean", "Main.jack"))
  expect_compiled(file.path("data", "While", "Main.jack"))
  expect_compiled(file.path("data", "If", "Main.jack"))
  expect_compiled(file.path("data", "String", "Main.jack"))
  expect_compiled(file.path("data", "Array", "Main.jack"))
  expect_compiled(file.path("data", "Method", "Main.jack"))
  expect_compiled(file.path("data", "Method", "Complex.jack"))
})
