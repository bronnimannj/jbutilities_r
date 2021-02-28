test_that("Error when input empty 1", {
  expect_error(find_series(), "argument \"Name\" is missing, with no default")
})

test_that("Error when input empty 2", {
  expect_error(find_series(Name = "test"), "argument \"Type\" is missing, with no default")
})


test_that("Error when input empty 3", {
  expect_error(find_series(Type = "csv"), "argument \"Name\" is missing, with no default")
})

test_that("test file doesnt exist, test returns NULL", {
  expect_equal(find_series("test", "csv"), NULL)
})
