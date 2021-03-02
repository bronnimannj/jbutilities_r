test_that("test 1", {
  expect_equal(pavg( data.frame(c(1,1,1),
                                c(1,1,1)),
                     na_rm = TRUE),
               c(1,1,1))
})

test_that("test 2", {
  expect_equal(pavg( data.frame(c(1,1,1),
                                c(2,3,4),
                                c(5,6,7)),
                     na_rm = TRUE),
               c( 2.666667, 3.333333, 4.000000),
               tolerance = 10^{-6})
})

test_that("test NA1", {
  expect_equal(pavg( data.frame(c(NA,5,6),
                                c(7,10,9)),
                     na_rm = TRUE),
               c(7,7.5,7.5))
})


test_that("test NA2", {
  expect_equal(pavg( data.frame(c(NA,5,6),
                                c(7,10,9)),
                     na_rm = FALSE),
               c(7,7.5,7.5))
})
