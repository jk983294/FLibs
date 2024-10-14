#' plot_quantile_returns_bar
#'
#' @param dt expect from FF::mean_rate_ret
#'
#' @import tidyr ggplot2
#' @export
plot_quantile_returns_bar <- function(dt) {
  data <- tidyr::pivot_longer(dt, cols = tidyr::starts_with("y"), names_to = "period", values_to = "ret")
  data$f_qtl <- factor(data$f_qtl)

  ggplot2::ggplot(data, aes(x = period, y = ret, fill = f_qtl)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    labs(title = "return by qtl", x = "period", y = "ret", fill = "qtl") +
    theme_minimal()
}


#' plot_quantile_returns_violin
#'
#' @param dt expect from FF::mean_rate_ret by date
#'
#' @import tidyr ggplot2
#' @export
plot_quantile_returns_violin <- function(dt) {
  data <- tidyr::pivot_longer(dt, cols = tidyr::starts_with("y"), names_to = "period", values_to = "ret")
  data$f_qtl <- factor(data$f_qtl)

  ggplot2::ggplot(data, aes(x = period, y = ret, fill = f_qtl)) +
    geom_violin(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.6) +
    labs(title = "return by qtl", x = "period", y = "ret", fill = "qtl") +
    theme_minimal()
}

#' plot_cum_rets
#'
#' @param dt expect from FF::cumret_dt
#' @param first_n 0L
#' @param last_n 0L
#'
#' @export
plot_cum_rets <- function(dt, first_n = 0L, last_n = 0L) {
  FC::mat_plot(dt, first_n = first_n, last_n = last_n, xlab = "time", ylab = "rets", main = "Cum Returns")
}

#' plot_cum_rets_by_qtl
#'
#' @param dt expect from FF::mean_rate_ret
#' @param period default y1d
#' @param first_n 0L
#' @param last_n 0L
#'
#' @import data.table FM
#' @export
plot_cum_rets_by_qtl <- function(dt, period = "y1d", first_n = 0L, last_n = 0L) {
  dt1 <- FM::dt_select(dt, c("ticktime", "DataDate", "f_qtl", period), copy = FALSE)
  dt1[, ret := FM::cumret(get(period)), by = f_qtl]
  dt1[, qtl_name := paste0("ret_qtl_", as.character(f_qtl))]
  ret_wide <- dcast(dt1, formula = ticktime + DataDate ~ qtl_name, value.var = "ret")
  ret_wide[, c("ticktime", "DataDate") := NULL]
  plot_cum_rets(ret_wide, first_n, last_n)
}