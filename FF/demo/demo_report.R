library(data.table)

dt <- FF::dummy_factor()
rpt <- FF::return_report(dt)
rpt$ret_tbl
FF::plot_quantile_returns_bar(rpt$mean_qtl_rate_ret)
FF::plot_quantile_returns_violin(rpt$mean_qtl_rate_ret_bd)
FF::plot_cum_rets(FF::cumret_dt(rpt$f_rets))
FF::plot_cum_rets_by_qtl(rpt$mean_qtl_rate_ret_bd, "y1d")
