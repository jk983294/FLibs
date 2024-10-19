library(data.table)
library(ggplot2)

dt <- FF::dummy_factor()
tvs <- FF::factor_turnovers(dt, shifts = 1L:5L)
acfs <- FF::factor_rank_acf(dt, shifts = 1L:5L)

(tv_stats <- tvs[, FM::stats(tvs, q = c(0.5)), by = .(shift, f_qtl)])
FC::plot_bar(tv_stats, "shift", "mean", facet_col = "f_qtl")
FC::plot_bar(tv_stats, "f_qtl", "mean", facet_col = "shift")

(acf_stats <- acfs[, FM::stats(acf, q = c(0.5)), by = .(shift)])
FC::plot_bar(acf_stats, "shift", "mean")
