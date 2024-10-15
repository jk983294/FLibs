#' factor_ics
#' Computes the Spearman Rank Correlation based Information Coefficient (IC)
#'
#' @param group_adjust default FALSE
#' @param by_group default FALSE
#'
#' @import data.table FM
#' @export
factor_ics <- function(dt, group_adjust = FALSE, by_group = FALSE) {
  dt[, f_rank := FM::rank(factor), by = .(ticktime, DataDate)]
  y_columns <- FF::get_forward_returns_columns(dt)

  by_tick_ranks <- dt[, .N, by = .(ticktime, DataDate)]

  for (col in y_columns) {
    dt[, tmp_rank := FM::rank(get(col)), by = .(ticktime, DataDate)]
    l <- dt[, FM::pcor(f_rank, tmp_rank), by = .(ticktime, DataDate)]$V1
    by_tick_ranks[, (col) := l]
    dt[, tmp_rank := NULL]
  }

  by_tick_ranks[, N := NULL]
  dt[, f_rank := NULL]
  return(by_tick_ranks)
}

#' calc IC table
#'
#' @param dt table from FF::factor_ics
#'
#' @import data.table
#' @export
ic_table <- function(dt) {
  y_columns <- FF::get_forward_returns_columns(dt)
  dt1 <- dt[, FM::stats(.SD, q = c(0.5)), .SDcols = y_columns]
  dt1[, inf_ratio := NULL]
  dt1[, RiskAdjustedIC := mean / sd]

  l <- lapply(y_columns, function(y_col) {
    res <- t.test(dt[[y_col]], mu = 0.)
    return(res$p.value)
  })
  dt1[, pvalue := l]
  return(dt1)
}


#' dt_y_columns
#'
#' @param dt table
#'
#' @import data.table FM
#' @export
dt_y_columns <- function(dt) {
  y_columns <- FF::get_forward_returns_columns(dt)
  dt1 <- FM::dt_select(dt, y_columns, copy = FALSE)
  return(dt1)
}
