library(data.table)

dt <- rbindlist(lapply(1L:10L, function(d) as.data.table(FM::GBM_ohlc(DataDate = d))))
x <- rnorm(nrow(dt))
y <- rnorm(nrow(dt))
FC::plot_quantile(dt, x, y)

FC::tsplot(dt, c("close", "open"), date = "DataDate")
FC::tsplot(dt, c("close"), date = "DataDate")

FC::bin_plot(x, y, 10L)
FC::hist_plot(x, 10L)
