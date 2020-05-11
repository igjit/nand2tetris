test_that("translate_command works", {
  expect_equal(translate_command(command("push", "constant", 12))[1], "@12")
})
