library(data.table)
library(FQA)

dt <- as.data.table(FM::GBM_ohlc())
QA::autopcor(dt, c(5, 6))
x <- dt[, x]
w <- dt[, z]
stats(x, w = w)
stats(dt)
