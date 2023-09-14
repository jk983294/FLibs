library(data.table)
library(FQA)

dt <- data.table(x = c(1:6), y = c(2:7), z=runif(6))
x <- dt[, x]
w <- dt[, z]
stats(x, w = w)
stats(dt)
