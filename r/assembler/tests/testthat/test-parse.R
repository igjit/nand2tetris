test_that("extract_commands works", {
  lines <- c("// comment",
             "",
             "@2 // another comment",
             " D=A ")
  expected <- c("@2",
                "D=A")
  expect_equal(extract_commands(lines), expected)
})

test_that("parse_a_command works", {
  expect_equal(parse_a_command("@12"), a_command(12))
})

test_that("parse_c_command works", {
  expect_equal(parse_c_command("D=M"), c_command(dest = "D", comp = "M"))
  expect_equal(parse_c_command("0;JMP"), c_command(comp = "0", jump = "JMP"))
})
