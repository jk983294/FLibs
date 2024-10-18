library(data.table)
library(ggplot2)

dt <- FF::dummy_factor()
tvs <- FF::factor_turnovers(dt, shifts = 1L:5L)
acfs <- FF::factor_rank_acf(dt, shifts = 1L:5L)

tvs[, FM::stats(tvs), by = .(shift, f_qtl)]
acfs[, FM::stats(acf), by = .(shift)]
