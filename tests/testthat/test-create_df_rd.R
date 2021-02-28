#No idea how to test it for now

test_that("multiplication works", {
  expect_equal(create_df_rd(size = 9,
                            seed = 1,
                            Numerics = c("num1","num2"),
                            Booleans = c(0.2,0.3),
                            Categories = list(a = c(1,2,3),b = c("F","M"))),
               data.frame(ID = 1:9,
                          num1 = c(-0.89691455,  0.18484918 , 1.58784533,
                                   -1.13037567, -0.08025176,  0.13242028 ,
                                   0.70795473, -0.23969802,  1.98447394),
                          num2 = c(-0.96193342, -0.29252572 , 0.25878822 ,
                                   -1.15213189 , 0.19578283 , 0.03012394,
                                   0.08541773 , 1.11661021, -1.21885742 ),
                          Bool_1 = c(FALSE, FALSE ,FALSE, FALSE,  TRUE ,
                                     TRUE, FALSE,  TRUE, FALSE ),
                          Bool_2 = c(FALSE,  TRUE ,FALSE ,FALSE, FALSE,
                                     FALSE, FALSE ,FALSE, FALSE ),
                          a = factor(c( 1, 3, 2, 2, 1, 1, 1, 1, 1),
                                     levels = c( 1, 2, 3)),
                          b = factor(c("F", "M", "M", "F", "M",
                                       "M", "M", "F" ,"F"),
                                     levels = c("F", "M"))),
               tolerance = 0.0000001 )
})
