library(data.table)
test_that("stats", {
  df <- data.frame(x = c(1:6), y = c(2:7), z = runif(6))
  st1 <- stats(df$x, w = df$w)
  st2 <- stats(df)
  expect_equal(dim(st2), c(3L, 23L))
})
