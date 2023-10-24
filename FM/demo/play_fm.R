library(data.table)

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
unlist(FM::longest_drawdown(df$close))
pnls <- cumsum(rnorm(100))
unlist(FM::longest_drawdown(pnls))
unlist(FM::max_drawdown(pnls))

data <- data.table(x = c(1, 2, NA, 4, Inf), y = c(2, NA, 3, 4, Inf))
FM::dt_fill(data, c("x", "y"), is.infinite, NA_real_)
FM::dt_fill(data, c("x", "y"), is.na, 0)

data <- data.table(x = 1:10, y = 10:1)
(d1 <- FM::dt_select(data, c("x", "y")))
(d2 <- FM::dt_select(data, c("x", "y"), copy = FALSE))
(d3 <- FM::dt_select(data, c("x", "y"), add = list(z = "x")))
(d4 <- FM::dt_select(data, c("x", "y"), add = list(z = rep(0, 10))))
(d5 <- FM::dt_select(data, c("x", "y"), add = list(z = ~ x + y)))
(d6 <- FM::dt_select(data, c("x", y1 = "y")))

x <- rnorm(100)
FM::fractile(x, 10)
FM::grank(x, 10)
FM::percentile(x)
FM::rank_score(x)
