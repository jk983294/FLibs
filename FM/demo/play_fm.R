df <- as.data.frame(FM::GBM_ohlc())
dim(df)

result <- FM::autopcor(df, c(5L, 10L))
result[[1]]
