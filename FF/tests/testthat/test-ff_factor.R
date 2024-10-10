test_that("ff works", {
  v <- c(1., 2., NA)
  v_int <- c(1L, 2L, -1L)
  expect_equal(FF::as_int_group(v), v_int)

  qtl_int <- c(1L, 2L, 1L)
  expect_equal(FF::qcut(v, 2L), qtl_int)

  expect_equal(FF::fill_zero(v), c(1., 2., 0.))
  expect_equal(FF::to_weights(v, demean = TRUE, equal_weight = FALSE), c(0.0, 0.5, -0.5))
  expect_equal(FF::to_weights(v, demean = TRUE, equal_weight = TRUE), c(0.0, 0.5, -0.5))
})
