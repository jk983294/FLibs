#' plot_quantile
#' @description Plot with quantile sampling
#'
#' @param data dt
#' @param x vec
#' @param y vec
#' @param w weight vec
#' @param na.rm remove missing values
#' @param ... additional parameters passed to plot
#' @param xlab x label
#' @param ylab y label
#'
#' @import data.table FM
#' @export
plot_quantile <- function (data, x, y, w = 1, n = 100, na.rm = TRUE, ..., xlab = NULL, ylab = NULL) {
    stopifnot(is.data.table(data), is.numeric(n), length(n) == 1L, n > 0)
    if (is.null(xlab)) {
        xsub <- substitute(x)
        xlab <- if (is.character(xsub)) xsub else deparse(xsub)
    }
    if (is.null(ylab)) {
        ysub <- substitute(y)
        ylab <- if (is.character(ysub)) ysub else deparse(ysub)
    }
    data <- FM::dt_select(data, select = "DataDate", list(x = x, y = y, w = w), copy = FALSE)
    data <- data[is.finite(x) & is.finite(y)]
    if (nrow(data) <= 1L || FM::fm_all_equal(data$x))
        return(NULL)
    n <- data[, .N, by = DataDate][, min(n, ceiling(mean(N)))]
    data[, `:=`(xrank, FM::grank(x, n)), by = DataDate]
    data[, `:=`(w, sqrt(w))]
    data <- data[, .(x = weighted.mean(x, w, na.rm = na.rm), y = weighted.mean(y, w, na.rm = na.rm)), keyby = xrank]
    data[x != 0, `:=`(beta, y/x)]
    x <- data$x
    y <- data$y
    plot(x, y, col = "darkgray", xlim = range(x, na.rm = na.rm),
        ylim = range(y, na.rm = na.rm), xlab = xlab, ylab = ylab,
        ..., sub = sprintf("(n = %d)", n))
    abline(h = 0, lty = 2)
    abline(v = 0, lty = 2)
    m <- coef(lm(y ~ x))
    if (all(is.finite(m)))
        abline(m)
    if (n >= 10L) {
        pred <- predict(loess(y ~ x), se = TRUE)
        lines(x, pred$fit, lty = 2)
        lines(x, pred$fit - qt(0.975, pred$df) * pred$se, lty = 2, col = "blue")
        lines(x, pred$fit + qt(0.975, pred$df) * pred$se, lty = 2, col = "blue")
    }
    invisible(data)
}

get_datetime <- function (data, date = "cob", time = "ticktime") {
    has_date <- date %in% names(data)
    has_time <- time %in% names(data)
    datetime <- c(if (has_date) date, if (has_time) time)
    list(has_date = has_date, has_time = has_time, datetime = datetime)
}


#' tsplot
#' @description Plot time series
#'
#' @param x a data.table with date/time key
#' @param y selector of columns in x to plot
#' @param ... additional parameters passed to matplot
#'
#' @import data.table FM
#' @export
tsplot <- function (x, y = NULL, map = identity, date = "cob", time = "ticktime",
    benchmark = NULL, type = "p", pch = ".", cex = 2, lty = 1,
    col = NULL, xlab = NULL, ylab = NULL, main = NULL, date_label = NULL,
    time_label = NULL, margin_label = NULL, legend_label = y,
    ..., year_lines = FALSE, month_lines = FALSE, day_lines = FALSE,
    hour_lines = FALSE, vlines = NULL, hlines = NULL, legend = NULL) {
    dt <- get_datetime(x, date, time)
    if (is.null(y)) {
        num_cols <- vapply(x, is.double, logical(1L))
        y <- setdiff(names(x)[num_cols], dt$datetime)
    }
    if (is.null(xlab)) {
        xlab <- paste0(dt$datetime, collapse = ", ")
    }
    if (is.null(ylab)) {
        ylab <- if (length(y) == 1L)
            y
        else "Value"
    }
    if (is.null(col)) {
        col <- seq_along(y)
    }
    mat <- vapply(.subset(x, y), map, numeric(nrow(x)))
    if (!is.matrix(mat)) {
        mat <- t(mat)
    }
    if (!is.null(benchmark)) {
        mat <- mat - mat[, benchmark]
    }
    matplot(mat, type = "n", xaxt = "n", xlab = xlab, ylab = ylab,
        main = main, axes = TRUE, add = FALSE, ...)
    if (is.numeric(vlines)) {
        abline(v = vlines, col = "gray50", lty = 1, lwd = 1)
    }
    else if (is.character(vlines)) {
        vlines_xs <- which(diff(x[[vlines]]) != 0L) + 1L
        abline(v = vlines_xs, col = "gray50", lty = 1, lwd = 1)
    }
    if (is.numeric(hlines)) {
        abline(h = hlines, col = "gray50", lty = 1, lwd = 1)
    }
    if (dt$has_time) {
        if (hour_lines) {
            hour_xs <- which(diff(floor(x[[time]]/1e+07)) != 0L) + 1L
            abline(v = hour_xs, col = "gray30", lty = 1, lwd = 1)
        }
        if (day_lines) {
            day_xs <- which(diff(x[[date]]) != 0L) + 1L
            abline(v = day_xs, col = "gray50", lty = 1, lwd = 1.2)
        }
        if (is.null(time_label)) {
            time_label <- time
        }
        axis(1, at = seq_len(nrow(mat)), padj = -0.75, tick = FALSE,
            labels = x[[time_label]])
    }
    if (dt$has_date) {
        if (month_lines) {
            month_xs <- which(diff(floor(x[[date]]/100)) != 0L) + 1L
            abline(v = month_xs, col = "gray95", lty = 1, lwd = 1.5)
        }
        if (year_lines) {
            year_xs <- which(diff(floor(x[[date]]/10000)) != 0) + 1L
            abline(v = year_xs, col = "gray80", lty = 1, lwd = 2)
        }
        if (is.null(date_label)) {
            date_label <- date
        }
        axis(1, at = seq_len(nrow(mat)), padj = 0.75, tick = FALSE,
            labels = x[[date_label]])
    }
    matplot(mat, type = type, lty = lty, pch = pch, cex = cex,
        col = col, xlab = xlab, ylab = ylab, axes = FALSE, add = TRUE,
        ...)
    if (!is.null(margin_label)) {
        if (isTRUE(margin_label)) {
            margin_label <- legend_label
        }
        last_value <- apply(mat, 2, function(x) last(x[is.finite(x)]))
        margin_args <- list(text = NULL, side = 4, line = 0,
            at = last_value, adj = 1, padj = 1, col = col, las = 1,
            cex = 0.75)
        if (is.character(margin_label)) {
            margin_args$text <- margin_label
        }
        else if (is.list(margin_label)) {
            margin_args[names(margin_label)] <- margin_label
        }
        do.call(graphics::mtext, margin_args)
    }
    if (is.character(legend)) {
        graphics::legend(legend, legend = legend_label, col = col, lty = lty)
    }
    else if (is.list(legend)) {
        legend_args <- list(legend = legend_label, col = col, lty = lty)
        legend_args[names(legend)] <- legend
        do.call(graphics::legend, legend_args)
    }
}


#' hist_plot
#' @description Plot hist
#'
#' @param x vector
#' @param bins bin num
#' @param min_val min val
#' @param max_val max val
#' @param threads thread num
#'
#' @import FM
#' @export
hist_plot <- function (x, bins, min_val = NA, max_val = NA, threads = 1L) {
    rets <- FM::fast_hist_n(x, bins, min_val, max_val, threads)
    x_vals <- head(rets[["vals"]], -1)
    cnts <- rets[["cnts"]]
    main_str <- sprintf("mean = %f, sd = %f, skew = %f, kurt = %f",
                        rets[["mean_"]], rets[["sd_"]], rets[["skew_"]], rets[["kurt_"]])
    plot(x_vals, cnts, type = "s", main = main_str)
}

#' bin_plot
#' @description Plot y mean based on x bins
#'
#' @param x vector
#' @param y vector
#' @param bins bin num
#' @param threads thread num
#'
#' @import FM
#' @export
bin_plot <- function (x, y, bins, threads = 1L) {
    rets <- FM::get_binned_stats(x, y, bins, threads)
    x_means <- rets[["x_mean"]]
    y_means <- rets[["y_mean"]]
    plot(x_means, y_means, type = "s")
}