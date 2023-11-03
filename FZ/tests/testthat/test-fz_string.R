test_that("fz_string", {
  expect_equal(FZ::fz_basename("/tmp/a.txt"), "a.txt")
  expect_equal(FZ::fz_dirname("/tmp/a.txt"), "/tmp")
  expect_equal(FZ::fz_split("i j k", " "), c("i", "j", "k"))
  expect_equal(FZ::fz_HumanReadableMillisecond("60ms"), 60L)
  expect_equal(FZ::str_expand("f%d,tspcor({x1},{x2},{n1})", x1 = "ret", x2 = "ret2", n1 = 3L, expand = F), "f%d,tspcor(ret,ret2,3)")
  expect_equal(FZ::str_expand("f%d,tspcor({x1},{x2},{n1})", x1 = "ret", x2 = c("ret2", "ret3"), n1 = 1L:3L, expand = T),
    c("f%d,tspcor(ret,ret2,1)", "f%d,tspcor(ret,ret3,1)", "f%d,tspcor(ret,ret2,2)", "f%d,tspcor(ret,ret3,2)",
      "f%d,tspcor(ret,ret2,3)", "f%d,tspcor(ret,ret3,3)"))

  x1 <- as.character((seq(1L, 5L, 2L)))
  x2 <- as.character((seq(2L, 5L, 2L)))
  expect_equal(FZ::str_expand2("f%d,tspcor({x1},{x2},{n1})", c("x1", "x2"), list(x1, x2)),
    c("f%d,tspcor(1,2,{n1})", "f%d,tspcor(1,4,{n1})", "f%d,tspcor(3,2,{n1})",
      "f%d,tspcor(3,4,{n1})", "f%d,tspcor(5,2,{n1})", "f%d,tspcor(5,4,{n1})"))
  
  exprs <- c("f0,tsmean(a, b)", "f1,tssum(a, b)")
  dt <- FZ::expr_split_to_dt(exprs, ",")
  expect_equal(dt$f0, c("f0", "f1"))
  expect_equal(dt$f1, c("tsmean(a, b)", "tssum(a, b)"))
})
