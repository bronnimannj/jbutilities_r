test_that("Error if empty 1", {
  expect_error(name_series(), "argument \"Name\" is missing, with no default")
})

test_that("Error if empty 2", {
  expect_error(name_series(Name = "sjdh"),
               "argument \"Type\" is missing, with no default")
})


test_that("Error if empty 3", {
  expect_error(name_series(Type = "sjdh"),
               "argument \"Name\" is missing, with no default")
})


test_that("test file didnt exist, thus we create without a series", {
  expect_equal(name_series("test", "csv"), "test.csv")
})
