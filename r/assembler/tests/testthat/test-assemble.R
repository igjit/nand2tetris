test_that("assemble.a_command works", {
  expect_equal(assemble(a_command(2)), "0000000000000010")
})

test_that("assemble.c_command works", {
  expect_equal(assemble(parse_c_command("D=D+A")), "1110000010010000")
  expect_equal(assemble(parse_c_command("0;JMP")), "1110101010000111")
})

test_that("assemble_c_comp works", {
  expect_equal(assemble_c_comp("D+1"), "0011111")
})

test_that("assemble_c_dest works", {
  expect_equal(assemble_c_dest(NULL), "000")
  expect_equal(assemble_c_dest("D"), "010")
  expect_equal(assemble_c_dest("AM"), "101")
})

test_that("assemble_c_jump works", {
  expect_equal(assemble_c_jump(NULL), "000")
  expect_equal(assemble_c_jump("JGT"), "001")
  expect_equal(assemble_c_jump("JLE"), "110")
})

test_that("generate_symbol_table works", {
  commands <- map(c("(LOOP)", "@LOOP", "0;JMP"), parse_command)
  expect_equal(generate_symbol_table(commands), c(LOOP = 0))
})

