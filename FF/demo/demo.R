library(data.table)

factor_report <- function(dt, long_short = TRUE, group_neutral = FALSE, by_group = FALSE) {
  quantile_stats <- dt[, FM::stats(factor), by = f_qtl]
  return(list("quantile_stats" = quantile_stats))
}

dt <- FF::dummy_factor()
f_rets <- FF::factor_returns(dt)

rpt <- factor_report(dt)
