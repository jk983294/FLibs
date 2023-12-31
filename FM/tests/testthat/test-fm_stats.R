test_that("stats", {
  expect_equal(fm_nan_ratio(c(1., 2, NA_real_, 4)), 0.25)
  expect_equal(fm_nan_ratio(c(1., 2, 3., 4)), 0.)
  expect_equal(fm_nan_ratio(c(1., 2, Inf, 4)), 0.)
  expect_equal(fm_inf_ratio(c(1., 2, NA_real_, 4)), 0.)
  expect_equal(fm_inf_ratio(c(1., 2, 3., 4)), 0.)
  expect_equal(fm_inf_ratio(c(1., 2, Inf, 4)), 0.25)
  expect_equal(fm_zero_ratio(c(1., 2, Inf, 4)), 0.)
  expect_equal(fm_zero_ratio(c(0., 2, Inf, 4)), 0.25)
  expect_equal(fm_zero_ratio(c(0., 0.001, Inf, 4)), 0.25)
  expect_equal(fm_zero_ratio(c(0., 1e-10, Inf, 4)), 0.5)
  expect_equal(fcap(c(0., -Inf, Inf, 4), -2, 2), c(0., -2, 2, 2))
  expect_equal(fcap(6, -2, 2), 2)
  expect_equal(log_trim(c(0., -0.5, 0.5, 10.)), c(0., -0.5, 0.5, log(10.) + 1.))
  expect_equal(fm_all_equal(c(1., 1, NA_real_, 1), 1e-6, TRUE), TRUE)
  expect_equal(fm_all_equal(c(1., 1, NA_real_, 1)), FALSE)
  expect_equal(fm_all_equal(c(1., 1, 2)), FALSE)
  expect_equal(fm_all_equal(c(1., 1, 1)), TRUE)

  data <- list(x = 1, y = 2)
  expect_equal(list_get(data, "x"), 1)
  expect_equal(list_get(data, ~ x + y), 3)
  data <- data.frame(x = 1:10, y = rnorm(10))
  expect_equal(list_get(data, "x"), data$x)
  expect_equal(length(list_get(data, ~ ifelse(x >= 5, y - 0.1, y + 0.1))), 10L)

  fs <- fractile(rnorm(100), 10)
  expect_equal(length(fs), 100L)
})
