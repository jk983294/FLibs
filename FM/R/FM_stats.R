#' stats generic
#'
#' @param x An object
#' @param ... Arguments passed to or from other methods
#' @export
stats <- function(x, ...) {
    UseMethod("stats", x)
}

#' stats default method
#'
#' @param x An object
#' @param ... Arguments passed to or from other methods
#' @export
stats.default <- function(x, ...) {
    print("Called default method for stats")
}

#' stats method for numeric class
#'
#' @param x numeric vector
#' @param w weight vector
#' @param q quantile vector
#' @param ... Other arguments passed to or from other methods
#'
#' @export
stats.numeric <- function(x, w = NULL, q = c(0., 0.005, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.995, 1), ...) {
    mean <- if (is.null(w)) mean(x, na.rm = TRUE) else weighted.mean(x, w, na.rm = TRUE, names = FALSE)
    if (length(q)) {
        qs <- quantile(x, q, na.rm = TRUE, names = FALSE)
        names(qs) <- paste0("q", formatC(q * 100))
    } else {
        qs <- numeric()
    }
    sd <- sd(x, na.rm = TRUE)
    n <- length(x)
    kurt_ <- .Call("_FM_fm_kurt", PACKAGE = "FM", x)
    skew_ <- .Call("_FM_fm_skew", PACKAGE = "FM", x)
    zero_ <- .Call("_FM_fm_zero_ratio", PACKAGE = "FM", x, 1e-9)
    c(
        n = n, pos_ratio = sum(x > 0, na.rm = TRUE) / n, zero_ratio = zero_, skew = skew_, kurt = kurt_,
        na_ratio = sum(is.na(x)) / n, inf_ratio = sum(is.infinite(x)) / n, mean = mean, sd = sd, as.list(qs)
    )
}

#' stats method for data.frame class
#'
#' @param x data.frame
#' @param ... Other arguments passed to or from other methods
#'
#' @export
stats.data.frame <- function(x, ...) {
    cbind(name = names(x), rbindlist(lapply(x, stats, ...)))
}

#' fm_quantile
#' plot using ggplot(q, aes(x = qv)) + geom_line(aes(y = density_x))
#'
#' @param x numeric vector
#' @param q quantile vector
#'
#' @export
fm_quantile <- function(x, q = c(0., 0.005, 0.01, 0.05, (1L:9L) * 0.1, 0.95, 0.99, 0.995, 1)) {
    if (length(q)) {
        qv <- quantile(x, q, na.rm = TRUE, names = FALSE)
    } else {
        qv <- numeric()
    }
    as.data.frame(list(q = q, density_x = ifelse(q < 0.5, q, 1 - q), qv = qv))
}


#' dt_fill
#'
#' @param data dt
#' @param columns columns
#' @param selector selector
#' @param fill fill
#'
#' @import data.table
#' @export
dt_fill <- function (data, columns, selector, fill) {
    selector <- match.fun(selector)
    for (col in columns) {
        .selector <- selector(data[[col]])
        data[.selector, `:=`((col), fill)]
    }
}
