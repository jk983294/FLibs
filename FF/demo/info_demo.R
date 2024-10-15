library(data.table)
library(ggplot2)

dt <- FF::dummy_factor()
ics <- FF::factor_ics(dt)
tbl <- FF::ic_table(ics)
FF::plot_ic_ts(ics)
FF::plot_ic_hist(ics, n_bins = 10L)




