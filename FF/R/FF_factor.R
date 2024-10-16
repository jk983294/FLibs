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
    df[, DataDate := rep(20240100L + (1L:n_day), each = n_ukey)]
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
    q_ <- c(0., 0.5, 1)
    if (by_date) {
        dt2 <- dt1[, FM::stats(.SD, q = q_), .SDcols = y_columns, by = .(ticktime, DataDate, f_qtl)]
    } else {
        dt2 <- dt1[, FM::stats(.SD, q = q_), .SDcols = y_columns, by = .(f_qtl)]
    }
    return(dt2)
}

get_time_freq_inner <- function(y_label) {
    period <- 60L # in sec
    if (y_label == "y1d" | y_label == "1d") {
        period <- period * 24L * 60L
    } else if (y_label == "y2d" | y_label == "2d") {
        period <- period * 24L * 60L * 2
    } else if (y_label == "y3d" | y_label == "3d") {
        period <- period * 24L * 60L * 3
    } else if (y_label == "y4d" | y_label == "4d") {
        period <- period * 24L * 60L * 4
    } else if (y_label == "y5d" | y_label == "5d") {
        period <- period * 24L * 60L * 5
    } else if (y_label == "y1m" | y_label == "y22d") {
        period <- period * 24L * 60L * 22
    } else if (y_label == "y1y" | y_label == "y252d") {
        period <- period * 24L * 60L * 252
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
#' adjust return to base period
#'
#' @param dt table, expect (group, y_cols) format
#'
#' @import data.table
#' @export
mean_rate_ret <- function(dt) {
    # mrq_mean <- data.table::dcast(dt, formula = f_qtl ~ name, value.var = "mean")
    mrq_mean <- copy(dt)
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

#' sd_rate_conversion
#' adjust sd to base period
#'
#' @param dt table, expect (group, y_cols) format
#'
#' @import data.table
#' @export
sd_rate_conversion <- function(dt) {
    dt1_ <- copy(dt)
    y_columns <- get_forward_returns_columns(dt1_)
    freq_vec <- get_time_freq(y_columns)
    base_period <- min(freq_vec)

    for (i in 1L:length(freq_vec)) {
        conversion_factor <- sqrt(freq_vec[i] / base_period)
        col <- y_columns[i]
        dt1_[, (col) := get(col) / conversion_factor]
    }
    return(dt1_)
}

inner_simple_ab <- function(x, y, freq_adjust = 1.) {
    model <- lm(y ~ x)
    coefs <- unname(unlist(coef(model)))
    return(list(Ann_alpha = (1. + coefs[1])^freq_adjust - 1., beta = coefs[2]))
}

#' factor_alpha_beta
#' compute the alpha (excess returns), alpha t-stat (alpha significance), and beta (market exposure) of a factor.
#'
#' @param factor_data dt, expect (group, factor, y_cols) format
#' @param return_dt expect FF::factor_returns format
#' @param long_short default TRUE
#' @param equal_weight default FALSE
#'
#' @import data.table
#' @export
factor_alpha_beta <- function(factor_data, return_dt = NA, long_short = TRUE, equal_weight = FALSE) {
    ret_dt <- NA
    if (is.null(return_dt)) {
        ret_dt <- factor_returns(factor_data, demean = long_short, equal_weight = equal_weight)
    } else {
        ret_dt <- copy(return_dt)
    }
    y_columns <- get_forward_returns_columns(ret_dt)
    freq_vec <- get_time_freq(y_columns)
    base_period <- min(freq_vec)
    y1y_period <- get_time_freq_inner("y1y")
    universe_ret <- factor_data[, lapply(.SD, mean), .SDcols = y_columns, by = .(ticktime, DataDate)]

    res <- vector("list", length(freq_vec))
    names(res) <- y_columns
    for (i in 1L:length(freq_vec)) {
        conversion_factor <- y1y_period / freq_vec[i]
        col <- y_columns[i]
        res[[i]] <- c(period = col, inner_simple_ab(universe_ret[[col]], ret_dt[[col]], conversion_factor))
    }
    dt1 <- data.table::rbindlist(res)
    return(dt1)
}

#' compute_mean_returns_spread
#' compute the difference between the mean returns of quantiles, max - min
#'
#' @param ret_dt dt, expect from FF::mean_rate_ret
#' @param sd_dt expect from FF::sd_rate_conversion
#'
#' @import data.table FM
#' @export
compute_mean_returns_spread <- function(ret_dt, sd_dt) {
    y_columns <- get_forward_returns_columns(ret_dt)
    max_grp <- max(ret_dt$f_qtl)
    min_grp <- min(ret_dt$f_qtl)
    max_dt <- ret_dt[f_qtl == max_grp, ]
    min_dt <- ret_dt[f_qtl == min_grp, ]
    merged <- merge(max_dt, min_dt, by.x = c("ticktime", "DataDate"), by.y = c("ticktime", "DataDate"))

    for (i in 1L:length(y_columns)) {
        col <- y_columns[i]
        col_x <- paste0(col, ".x")
        col_y <- paste0(col, ".y")
        merged[, (col) := get(col_x) - get(col_y)]
    }
    mean_return_difference <- FM::dt_select(merged, c("ticktime", "DataDate", y_columns))

    max_dt <- sd_dt[f_qtl == max_grp, ]
    min_dt <- sd_dt[f_qtl == min_grp, ]
    merged <- merge(max_dt, min_dt, by.x = c("ticktime", "DataDate"), by.y = c("ticktime", "DataDate"))

    for (i in 1L:length(y_columns)) {
        col <- y_columns[i]
        col_x <- paste0(col, ".x")
        col_y <- paste0(col, ".y")
        merged[, (col) := sqrt(get(col_x)^2 + get(col_y)^2)]
    }
    joint_std_err <- FM::dt_select(merged, c("ticktime", "DataDate", y_columns))
    return(list("mean_return_difference" = mean_return_difference, "joint_std_err" = joint_std_err))
}

#' get_return_table
#'
#' @param alpha_beta dt, expect from FF::factor_alpha_beta
#' @param mean_qtl_rate_ret expect from FF::mean_rate_ret
#' @param return_diff expect from FF::compute_mean_returns_spread
#'
#' @import data.table FM
#' @export
get_return_table <- function(alpha_beta, mean_qtl_rate_ret, return_diff) {
    returns_table <- FM::dt_select(alpha_beta)
    max_ret_qtl <- unname(unlist(mean_qtl_rate_ret[which.max(mean_qtl_rate_ret$f_qtl)]))
    min_ret_qtl <- unname(unlist(mean_qtl_rate_ret[which.min(mean_qtl_rate_ret$f_qtl)]))

    returns_table <- cbind(returns_table, Top_qtl_ret = max_ret_qtl[2L:length(max_ret_qtl)])
    returns_table <- cbind(returns_table, Bottom_qtl_ret = min_ret_qtl[2L:length(min_ret_qtl)])

    y_columns <- get_forward_returns_columns(return_diff)
    mean_diff_ret <- unname(unlist(return_diff[, lapply(.SD, mean), .SDcols = y_columns]))
    returns_table <- cbind(returns_table, mean_diff_ret = mean_diff_ret)
    return(returns_table)
}

#' cumret_dt
#'
#' @param f_rets dt, expect from FF::factor_returns
#'
#' @import data.table FM
#' @export
cumret_dt <- function(f_rets) {
    ynames <- get_forward_returns_columns(f_rets)
    l <- lapply(ynames, function(y_name) {
        return(FM::cumret(f_rets[[y_name]]))
    })
    dt <- setDT(lapply(l, unlist))
    names(dt) <- ynames
    return(dt)
}

#' return_report
#'
#' @param dt dt, expect like FF::dummy_factor
#'
#' @import data.table
#' @export
return_report <- function(dt, long_short = TRUE, group_neutral = FALSE, by_group = FALSE) {
    f_rets <- factor_returns(dt, long_short)
    mrq <- mean_return_by_quantile(dt, demeaned = long_short, by_date = FALSE)
    mrq_mean <- mrq[, .(f_qtl, name, mean)]
    mrq_mean <- data.table::dcast(mrq_mean, formula = f_qtl ~ name, value.var = "mean")
    mean_qtl_rate_ret <- mean_rate_ret(mrq_mean)

    mrq_bd <- mean_return_by_quantile(dt, demeaned = long_short, by_date = TRUE)
    mrq_mean_bd <- mrq_bd[, .(ticktime, DataDate, f_qtl, name, mean)]
    mrq_mean_bd <- data.table::dcast(mrq_mean_bd, formula = ticktime + DataDate + f_qtl ~ name, value.var = "mean")
    mean_qtl_rate_ret_bd <- mean_rate_ret(mrq_mean_bd)

    mrq_sd_bd <- mrq_bd[, .(ticktime, DataDate, f_qtl, name, sd)]
    mrq_sd_bd <- data.table::dcast(mrq_sd_bd, formula = ticktime + DataDate + f_qtl ~ name, value.var = "sd")
    sd_qtl_daily <- sd_rate_conversion(mrq_sd_bd)

    alpha_beta <- factor_alpha_beta(dt, f_rets)
    ret_spread <- compute_mean_returns_spread(mean_qtl_rate_ret_bd, sd_qtl_daily)
    ret_tbl <- get_return_table(alpha_beta, mean_qtl_rate_ret, ret_spread$mean_return_difference)
    return(list(
        "mean_qtl_rate_ret" = mean_qtl_rate_ret,
        "mean_qtl_rate_ret_bd" = mean_qtl_rate_ret_bd,
        "ret_tbl" = ret_tbl,
        "f_rets" = f_rets
    ))
}
