test_that("test example", {
  expect_equal(rankdf(data.frame(x = c(3,4),
                                 y = c(5,6))),
               data.frame(TOP1 = c(3,4),
                          TOP2 = c(5,6)))
})
