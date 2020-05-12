test_that("translate_command works", {
  expect_equal(translate_command(command("push", "constant", 12))[1], "@12")
})

test_that("label_generator works", {
  gen_label <- label_generator()
  expect_equal(gen_label(), "LABEL0")
  expect_equal(gen_label(), "LABEL1")
})
