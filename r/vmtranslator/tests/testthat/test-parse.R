test_that("extract_commands works", {
  lines <- c("// comment",
             "",
             "push constant 1 // another comment",
             "push constant 2",
             " add ")
  expected <- c("push constant 1",
                "push constant 2",
                "add")
  expect_equal(extract_commands(lines), expected)
})

test_that("parse_command works", {
  expect_equal(parse_command("push local 1"), command("push", "local", 1))
  expect_equal(parse_command("goto LOOP"), command("goto", "LOOP"))
  expect_equal(parse_command("add"), command("add"))
})
