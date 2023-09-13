# FLibs
FLibs

## pkg
```R
# in FLIBs folder
> library("Rcpp")
> Rcpp.package.skeleton("FZ")

# go inside FZ pkg folder
> pkgbuild::compile_dll()
> devtools::document()

# test, in FZ pkg folder
> usethis::use_testthat(3)
> usethis::use_test("fz_string")
> testthat::test_file("tests/testthat/test-fz_string.R")
> devtools::test()
> devtools::check()
```
