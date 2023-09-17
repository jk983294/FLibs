df <- as.data.frame(FM::GBM_ohlc())
dim(df)

(result <- FM::autopcor(df, c(5L, 10L)))
result[[1]]

FM::pcor(df)
FM::pcor(df, df$close)
FM::rcor(df)
FM::rcor(df, df$close)
