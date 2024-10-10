library(data.table)

factor_report <- function(dt, long_short = TRUE, group_neutral = FALSE, by_group = FALSE) {
  quantile_stats <- dt[, FM::stats(factor), by = f_qtl]
  return(list("quantile_stats" = quantile_stats))
}

dt <- FF::dummy_factor()
# f_rets <- FF::factor_returns(dt)
mrq <- FF::mean_return_by_quantile(dt, demeaned = TRUE, by_date = FALSE)
mrq_mean <- mrq[, .(f_qtl, name, mean)]
mean_quant_rate_ret <- FF::mean_rate_ret(mrq_mean)

mrq_bd <- FF::mean_return_by_quantile(dt, demeaned = TRUE, by_date = TRUE)
mrq_mean_bd <- mrq_bd[, .(ticktime, DataDate, f_qtl, name, mean)]
mean_quant_rate_ret_bd <- FF::mean_rate_ret(mrq_mean_bd)



rpt <- factor_report(dt)
