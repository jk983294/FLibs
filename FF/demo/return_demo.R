library(data.table)
library(ggplot2)

dt <- FF::dummy_factor()
f_rets <- FF::factor_returns(dt)
mrq <- FF::mean_return_by_quantile(dt, demeaned = TRUE, by_date = FALSE)
mrq_mean <- mrq[, .(f_qtl, name, mean)]
mrq_mean <- data.table::dcast(mrq_mean, formula = f_qtl ~ name, value.var = "mean")
mean_qtl_rate_ret <- FF::mean_rate_ret(mrq_mean)

mrq_bd <- FF::mean_return_by_quantile(dt, demeaned = TRUE, by_date = TRUE)
mrq_mean_bd <- mrq_bd[, .(DataDate, ticktime, f_qtl, name, mean)]
mrq_mean_bd <- data.table::dcast(mrq_mean_bd, formula = ticktime + DataDate + f_qtl ~ name, value.var = "mean")
mean_qtl_rate_ret_bd <- FF::mean_rate_ret(mrq_mean_bd)

mrq_sd_bd = mrq_bd[, .(DataDate, ticktime, f_qtl, name, sd)]
mrq_sd_bd = data.table::dcast(mrq_sd_bd, formula = ticktime + DataDate + f_qtl ~ name, value.var = "sd")
sd_qtl_daily <- FF::sd_rate_conversion(mrq_sd_bd)

alpha_beta <- FF::factor_alpha_beta(dt, f_rets)
ret_spread <- FF::compute_mean_returns_spread(mean_qtl_rate_ret_bd, sd_qtl_daily)
ret_tbl <- FF::get_return_table(alpha_beta, mean_qtl_rate_ret, ret_spread$mean_return_difference)

FF::plot_quantile_returns_bar(mean_qtl_rate_ret)
FF::plot_quantile_returns_violin(mean_qtl_rate_ret_bd)
FF::plot_cum_rets(FF::cumret_dt(f_rets))
FF::plot_cum_rets_by_qtl(mean_qtl_rate_ret_bd, "y1d")
