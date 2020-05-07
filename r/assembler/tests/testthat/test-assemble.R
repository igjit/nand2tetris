test_that("assemble.a_command works", {
  expect_equal(assemble(a_command(2)), "0000000000000010")
})
