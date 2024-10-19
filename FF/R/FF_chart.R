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

#' plot_ic_ts
#'
#' @param dt expect from FF::factor_ics
#' @param MA_LEN 3L
#' @param first_n 0L
#' @param last_n 0L
#'
#' @import data.table FM
#' @export
plot_ic_ts <- function(dt, MA_LEN = 3L, first_n = 0L, last_n = 0L) {
  y_columns <- FF::get_forward_returns_columns(dt)
  dt1 <- FM::dt_select(dt, y_columns)

  for (col in y_columns) {
    new_col_name <- paste0(col, "_ma")
    dt1[, (new_col_name) := frollmean(get(col), n = MA_LEN, na.rm = TRUE, fill = 0.)]
    dt1[, (col) := NULL]
  }

  FC::mat_plot(dt1, first_n = 0L, last_n = 0L, xlab = "time", ylab = "ics", main = "IC")
}

#' plot_ic_hist
#'
#' @param dt expect from FF::factor_ics
#' @param n_bins default to 10 bins
#' @param use_density_hight show density
#'
#' @import data.table tidyr
#' @export
plot_ic_hist <- function(dt, n_bins = 10L, use_density_hight = TRUE) {
  dt1 <- FF::dt_y_columns(dt)
  data <- tidyr::pivot_longer(dt1, cols = names(dt1), names_to = "period", values_to = "ic")
  data <- data.table::as.data.table(data)
  data <- data[!is.na(ic), ]
  FC::plot_hist(data, value_col = "ic", facet_col = "period")
}

#' plot_ic_qq
#'
#' @param dt expect from FF::factor_ics
#'
#' @import data.table tidyr
#' @export
plot_ic_qq <- function(dt) {
  dt1 <- FF::dt_y_columns(dt)
  data <- tidyr::pivot_longer(dt1, cols = names(dt1), names_to = "period", values_to = "ic")
  data <- data.table::as.data.table(data)
  data <- data[!is.na(ic), ]
  FC::plot_qq(data, value_col = "ic", facet_col = "period")
}

#' plot_ic_heatmap
#'
#' @param dt expect from FF::mean_ic
#'
#' @import data.table tidyr
#' @export
plot_ic_heatmap <- function(dt) {
  ynames <- FF::get_forward_returns_columns(ic_month)
  data <- tidyr::pivot_longer(ic_month, cols = tidyr::starts_with("y"), names_to = "period", values_to = "ic")
  data$tgrp <- as.factor(data$tgrp)
  data <- data.table::as.data.table(data)
  data <- data[!is.na(ic), ]
  FC::plot_heat(data, "period", "tgrp", "ic")
}
