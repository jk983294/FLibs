library(data.table)
test_that("stats", {
  dt <- gen_test_data()
  expect_equal(dim(dt), c(2000L, 10L))
})
