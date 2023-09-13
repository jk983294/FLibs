test_that("fz_string", {
  expect_equal(FZ::fz_basename("/tmp/a.txt"), "a.txt")
  expect_equal(FZ::fz_dirname("/tmp/a.txt"), "/tmp")
  expect_equal(FZ::fz_split("i j k", " "), c("i", "j", "k"))
  expect_equal(FZ::fz_HumanReadableMillisecond("60ms"), 60L)
  expect_equal(FZ::str_expand("f%d,tspcor({x1},{x2},{n1})", x1 = "ret", x2 = "ret2", n1 = 3L, expand = F), "f%d,tspcor(ret,ret2,3)")
  expect_equal(FZ::str_expand("f%d,tspcor({x1},{x2},{n1})", x1 = "ret", x2 = c("ret2", "ret3"), n1 = 1L:3L, expand = T),
    c("f%d,tspcor(ret,ret2,1)", "f%d,tspcor(ret,ret3,1)", "f%d,tspcor(ret,ret2,2)", "f%d,tspcor(ret,ret3,2)",
      "f%d,tspcor(ret,ret2,3)", "f%d,tspcor(ret,ret3,3)"))
})
