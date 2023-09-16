test_that("stats", {
  df <- as.data.frame(FM::GBM_ohlc())
  expect_equal(dim(df), c(1000, 8))
  result <- QA::autopcor(df, c(5, 6))
  expect_equal(length(result), 8)
  expect_equal(length(result[[1]]), 2)
})
