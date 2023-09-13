library(Rcpp)
pkgbuild::compile_dll()
devtools::document()

# Rscript -e 'devtools::build()'