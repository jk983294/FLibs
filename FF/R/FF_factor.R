#' dummy_factor
#'
#' @param n_ukey default 10L
#' @param n_day default 100L
#' @param n_qtls default 2L
#'
#' @import data.table
#' @export
dummy_factor <- function(n_ukey = 10L, n_day = 100L, n_qtls = 2L) {
    df <- data.table::as.data.table(FM::GBM_ohlc(n = n_ukey * n_day))
    df <- df[, .(ukey, ticktime, DataDate, close)]

    df[, ukey := rep(1:n_ukey, n_day)]
    df[, ticktime := rep(150000000L, n_ukey * n_day)]
    df[, DataDate := rep(1L:n_day, each = n_ukey)]
    df[, y1d := shift(close, n = -1L, fill = NA) / close - 1., by = .(ukey)]
    df[, y2d := shift(close, n = -2L, fill = NA) / close - 1., by = .(ukey)]
    setnames(df, "close", "factor")
    df[, f_qtl := FF::qcut(factor, n_qtls), by = .(ticktime, DataDate)]
    df[, factor := FF::fill_zero(factor)]
    df[, y1d := FF::fill_zero(y1d)]
    df[, y2d := FF::fill_zero(y2d)]
    return(df)
}

#' factor_returns
#' calculate factor return by ticktime
#'
#' @param demean default TRUE
#' @param equal_weight default FALSE
#'
#' @import data.table
#' @export
factor_returns <- function(dt, demean = TRUE, equal_weight = FALSE) {
    dt[, weight_ := FF::to_weights(factor, demean = demean, equal_weight = equal_weight), by = .(ticktime, DataDate)]

    y_columns <- grepl("^y", names(dt))
    y_columns <- names(dt)[y_columns]

    for (col in y_columns) {
        new_col_name <- paste0(col, "_weighted")
        dt[, (new_col_name) := get(col) * weight_]
    }

    w_y_columns <- unlist(lapply(y_columns, function(col) paste0(col, "_weighted")))
    by_tick_rets <- dt[, lapply(.SD, sum), .SDcols = w_y_columns, by = .(ticktime, DataDate)]
    setnames(by_tick_rets, w_y_columns, y_columns)

    set(dt, j = c(w_y_columns, "weight_"), value = NULL) # remove intermediate result
    return(by_tick_rets)
}
