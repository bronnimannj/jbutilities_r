
# different types of error
test_that("Expect error if no input", {
  expect_error(cut_fast(),"We need at least 2 values for breaks")
})

test_that("Expect error if not enough breaks", {
  expect_error(cut_fast(x = 3, breaks = c(3)),"We need at least 2 values for breaks")
})

test_that("Expect error if not enough values to break", {
  expect_error(cut_fast(breaks = c(3,4)),"We need at least 1 value for x")
})

test_that("Expect error x not numeric", {
  expect_error(cut_fast(x = "xx", breaks = c(3,4)),"We need x to be numeric")
})

test_that("Expect error if not enough values to break", {
  expect_error(cut_fast(x = 3, breaks = c("n","m")),"We need breaks to be numeric")
})


# real values

test_that("One value to cut, before the intervalle", {
  expect_equal(cut_fast(x = 3, breaks = c(4,5)),
               factor("[-Inf,4)",levels = c("[-Inf,4)")))
})

test_that("One value to cut, inside the intervalle", {
  expect_equal(cut_fast(x = 4, breaks = c(3,5)),
               factor("[3,5)",levels = c("[3,5)")))
})

test_that("One value to cut, after the intervalle", {
  expect_equal(cut_fast(x = 5, breaks = c(3,4)),
               factor("[4,Inf)",levels = c("[4,Inf)")))
})

test_that("Multiple values to cut within multiple intervalles", {
  expect_equal(cut_fast(x = c(0,3,5,3.4,7.8,10), breaks = c(1,3,6,9)),
               factor(c("[-Inf,1)","[3,6)","[3,6)","[3,6)","[6,9)","[9,Inf)") ,
                      levels = c("[-Inf,1)","[3,6)","[6,9)","[9,Inf)")))
})
