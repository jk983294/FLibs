df <- as.data.frame(FM::GBM_ohlc())
dim(df)

(result <- FM::autopcor(df, c(5L, 10L)))
result[[1]]

FM::pcor(df)
FM::pcor(df, df)
FM::pcor(df, df$close)
FM::pcor(df$close, df)
FM::pcor(df$ukey, df$close)
FM::pcor(df$high, df$close)

FM::rcor(df)
FM::rcor(df, df)
FM::rcor(df, df$close)
FM::rcor(df$close, df)
FM::rcor(df$ukey, df$close)
FM::rcor(df$high, df$close)

FM::get_all_drawdown(df$close)

data <- data.table(x = c(1, 2, NA, 4, Inf), y = c(2, NA, 3, 4, Inf))
FM::dt_fill(data, c("x", "y"), is.infinite, NA_real_)
FM::dt_fill(data, c("x", "y"), is.na, 0)
