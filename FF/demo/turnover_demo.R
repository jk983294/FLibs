library(data.table)
library(ggplot2)

dt <- FF::dummy_factor()
tvs <- FF::factor_turnovers(dt)

dt[, f_rank := FM::rank(factor), by = .(DataDate, ticktime)]
