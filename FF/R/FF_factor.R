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

#' get_forward_returns_columns
#'
#' @param dt table
#'
#' @import data.table
#' @export
get_forward_returns_columns <- function(dt) {
  y_columns <- grepl("^y", names(dt))
  y_columns <- names(dt)[y_columns]
  return(y_columns)
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

    y_columns <- get_forward_returns_columns(dt)

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

#' demean_forward_returns
#'
#' @param dt table
#' @param grouper default NA, "f_qtl"
#'
#' @import data.table
#' @export
demean_forward_returns <- function(dt, grouper = NA) {
    dt1 <- copy(dt)
    y_columns <- get_forward_returns_columns(dt)
    for (col in y_columns) {
        if (is.na(grouper)) {
            dt1[, (col) := get(col) - mean(get(col), rm.na = TRUE)]
        } else {
            dt1[, (col) := get(col) - mean(get(col), rm.na = TRUE), by = get(grouper)]
        }
    }
    return(dt1)
}

#' mean_return_by_quantile
#'
#' @param dt table
#' @param demeaned default TRUE
#' @param by_date default FALSE
#'
#' @import data.table
#' @export
mean_return_by_quantile <- function(dt, demeaned = TRUE, by_date = FALSE) {
    dt1 <- demean_forward_returns(dt, grouper = NA)
    y_columns <- get_forward_returns_columns(dt1)
    if (by_date) {
        dt2 <- dt1[, FM::stats(.SD), .SDcols = y_columns, by = .(ticktime, DataDate, f_qtl)]
    } else {
        dt2 <- dt1[, FM::stats(.SD), .SDcols = y_columns, by = .(f_qtl)]
    }
    return(dt2)
}

get_time_freq_inner <- function(y_label) {
    period <- 60L # in sec
    if (y_label == "y1d" | y_label == "1d") {
        period = period * 24L * 60L
    } else if (y_label == "y2d" | y_label == "2d") {
        period = period * 24L * 60L * 2
    } else if (y_label == "y3d" | y_label == "3d") {
        period = period * 24L * 60L * 3
    } else if (y_label == "y4d" | y_label == "4d") {
        period = period * 24L * 60L * 4
    } else if (y_label == "y5d" | y_label == "5d") {
        period = period * 24L * 60L * 5
    } else {
        stop(paste("unknown y label ", y_label))
    }
    return(period)
}

#' get_time_freq
#'
#' @param y_labels vector of string, like "y1d"
#'
#' @export
get_time_freq <- function(y_labels) {
    unlist(lapply(y_labels, get_time_freq_inner))
}

#' mean_rate_ret
#'
#' @param dt table, expect FM::stats result with (f_qtl, name, mean)
#'
#' @import data.table
#' @export
mean_rate_ret <- function(dt) {
    mrq_mean <- data.table::dcast(dt, formula = f_qtl ~ name, value.var = "mean")

    y_columns <- get_forward_returns_columns(mrq_mean)
    freq_vec <- get_time_freq(y_columns)
    base_period <- min(freq_vec)

    for (i in 1L:length(freq_vec)) {
        conversion_factor <- base_period / freq_vec[i]
        col <- y_columns[i]
        mrq_mean[, (col) := (get(col) + 1.)^conversion_factor - 1.]
    }
    return(mrq_mean)
}