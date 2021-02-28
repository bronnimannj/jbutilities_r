test_that("step at 0", {
  expect_equal(step_time(0), paste0("(0) - [ ",  base::format(Sys.time(), "%d/%m/%Y %H:%M:%S ]")))
})


test_that("step at 2.3", {
  expect_equal(step_time(2.3), paste0("(2.3) - [ ",  base::format(Sys.time(), "%d/%m/%Y %H:%M:%S ]")))
})


test_that("step at xxx", {
  expect_equal(step_time("xxx"), paste0("(xxx) - [ ",  base::format(Sys.time(), "%d/%m/%Y %H:%M:%S ]")))
})

test_that("step empty", {
  expect_equal(step_time(), paste0("(0) - [ ",  base::format(Sys.time(), "%d/%m/%Y %H:%M:%S ]")))
})
