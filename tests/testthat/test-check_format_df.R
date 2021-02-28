# test it on the example

test_that("Iris check", {
  expect_equal(check_format_df(iris), "Check completed")
})
