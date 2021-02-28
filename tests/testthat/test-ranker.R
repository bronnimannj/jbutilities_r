test_that("test example", {
  expect_equal(ranker(10,
                      data.frame(x = 10 ,
                                 y = 10,
                                 z = 11,
                                 w = 12,
                                 u = 13),
                      "min"),
               1)
})
