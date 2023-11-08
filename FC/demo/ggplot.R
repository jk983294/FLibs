library(ggplot2)
library(ggExtra)
library(data.table)

dt <- rbindlist(lapply(1L:3L, function(ukey_) {
    rbindlist(lapply(1L:5L, function(d) as.data.table(FM::GBM_ohlc(ukey = ukey_, DataDate = d))))
}))
dt[, seq := 1L:length(close), by = ukey] # add seq as x axis
dt[, ret1 := shift(close, -1) / shift(open, -1), by = ukey]
dt[, ret_pco := FM::fcap(open/shift(close) - 1., -0.1, 0.1), by = .(ukey)]


ggplot(dt, aes(seq, close, colour=ukey, group = ukey)) + geom_point()

# line plot
ggplot(dt, aes(seq, close, colour=ukey, group = ukey)) +
    geom_line() +
    labs(y="close", x = "sequence of close") +
    ggtitle("close plot")

# scatter with jitter for discrete variable
ggplot(dt, aes(factor(ukey), volume, colour=ukey, group = factor(ukey))) +
    geom_point() +
    geom_jitter()


# Diverging Chart
last_closes <- dt[, tail(.SD, 1L), by = ukey]
FC::diverge_plot(last_closes, factor_col = "ukey", value_col = "close", 103.)

# Marginal Plot, relationship between two variables and examine their distributions
g <- ggplot(dt, aes(ret1, ret_pco)) +
    geom_count() +
    geom_smooth(method="lm", se=F)
ggExtra::ggMarginal(g, type = "histogram", fill="transparent")
g