expect_compiled <- function(jack) {
  vm <- str_replace(jack, ".jack$", ".vm")
  compiled <- read_jack(jack) %>% tokenize %>% parse %>% compile
  expect_equal(compiled, readLines(vm))
}

test_that("compile works", {
  expect_compiled(file.path("data", "IntConstant", "Main.jack"))
  expect_compiled(file.path("data", "Arithmetic", "Main.jack"))
  expect_compiled(file.path("data", "Variable", "Main.jack"))
})
