test_that("sim", {
  df <- as.data.frame(FM::GBM_ohlc())
  expect_equal(dim(df), c(1000, 8))
  result <- FM::autopcor(df, c(5, 6))
  expect_equal(length(result), 8)
  expect_equal(length(result[[1]]), 2)

  result <- FM::pcor(df)
  expect_equal(length(result), 3)
  expect_equal(length(result[[1]]), 28)
  expect_equal(length(result[[2]]), 28)
  expect_equal(length(result[[3]]), 28)

  result <- FM::pcor(df, df)
  expect_equal(length(result), 3)
  expect_equal(length(result[[1]]), 28)
  expect_equal(length(result[[2]]), 28)
  expect_equal(length(result[[3]]), 28)

  result <- FM::pcor(df, df$close)
  expect_equal(length(result), 8)

  result <- FM::pcor(df$close, df)
  expect_equal(length(result), 8)

  result <- FM::pcor(df$open, df$close)
  expect_equal(length(result), 1)
})
