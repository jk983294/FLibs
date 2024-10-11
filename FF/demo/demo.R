library(data.table)

factor_report <- function(dt, long_short = TRUE, group_neutral = FALSE, by_group = FALSE) {
  quantile_stats <- dt[, FM::stats(factor), by = f_qtl]
  return(list("quantile_stats" = quantile_stats))
}

dt <- FF::dummy_factor()
f_rets <- FF::factor_returns(dt)
mrq <- FF::mean_return_by_quantile(dt, demeaned = TRUE, by_date = FALSE)
mrq_mean <- mrq[, .(f_qtl, name, mean)]
mrq_mean <- data.table::dcast(mrq_mean, formula = f_qtl ~ name, value.var = "mean")
mean_qtl_rate_ret <- FF::mean_rate_ret(mrq_mean)

mrq_bd <- FF::mean_return_by_quantile(dt, demeaned = TRUE, by_date = TRUE)
mrq_mean_bd <- mrq_bd[, .(ticktime, DataDate, f_qtl, name, mean)]
mrq_mean_bd <- data.table::dcast(mrq_mean_bd, formula = ticktime + DataDate + f_qtl ~ name, value.var = "mean")
mean_qtl_rate_ret_bd <- FF::mean_rate_ret(mrq_mean_bd)

mrq_sd_bd = mrq_bd[, .(ticktime, DataDate, f_qtl, name, sd)]
mrq_sd_bd = data.table::dcast(mrq_sd_bd, formula = ticktime + DataDate + f_qtl ~ name, value.var = "sd")
sd_qtl_daily <- FF::sd_rate_conversion(mrq_sd_bd)

alpha_beta <- FF::factor_alpha_beta(dt, f_rets)
ret_spread <- FF::compute_mean_returns_spread(mean_qtl_rate_ret_bd, sd_qtl_daily)

get_return_table <- function(alpha_beta, mean_qtl_rate_ret, return_diff) {
  returns_table <- FM::dt_select(alpha_beta)
  max_ret_qtl <- unname(unlist(mean_qtl_rate_ret[which.max(mean_qtl_rate_ret$f_qtl)]))
  min_ret_qtl <- unname(unlist(mean_qtl_rate_ret[which.min(mean_qtl_rate_ret$f_qtl)]))

  returns_table <- cbind(returns_table, Top_qtl_ret = max_ret_qtl[2L:length(max_ret_qtl)])
  returns_table <- cbind(returns_table, Bottom_qtl_ret = min_ret_qtl[2L:length(min_ret_qtl)])

  y_columns <- FF::get_forward_returns_columns(return_diff)
  mean_diff_ret <- unname(unlist(return_diff[, lapply(.SD, mean), .SDcols = y_columns]))
  returns_table <- cbind(returns_table, mean_diff_ret = mean_diff_ret)
  return(returns_table)
}

ret_tbl <- get_return_table(alpha_beta, mean_qtl_rate_ret, ret_spread$mean_return_difference)

rpt <- factor_report(dt)
