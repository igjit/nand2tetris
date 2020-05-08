test_that("address_generator works", {
  gen_address <- address_generator()
  expect_equal(gen_address("a"), 16)
  expect_equal(gen_address("b"), 17)
  expect_equal(gen_address("a"), 16)
})
