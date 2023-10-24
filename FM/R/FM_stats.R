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

#' list_get
#'
#' @param data a list or data.frame
#' @param x name or formula
#' @param default value to return if x is not present in data
#'
#' @export
list_get <- function (data, x, default = stop("object '", x, "' not found")) {
    if (is.null(x)) {
        default
    }
    else if (inherits(x, "formula")) {
        if (length(x) == 2L) {
            eval(x[[2L]], data, environment(x))
        }
        else {
            stop("Invalid formula")
        }
    }
    else if (is.character(x) && length(x) == 1L) {
        if (is.null(out <- data[[x]])) {
            default
        }
        else {
            out
        }
    }
    else {
        x
    }
}

#' dt_select
#'
#' @param data dt
#' @param columns a character vector of columns to select. If named like c(x = "x1"), the columns in value are renamed to corresponding names
#' @param add a named list of column names, values, or formula to specify columns to add
#' @param copy TRUE to copy the resulted data.table. If a character vector is supplied, only specified columns will be copied.
#'
#' @import data.table
#' @export
dt_select <- function (data, select = names(data), add = list(), copy = TRUE) {
    missing_select <- setdiff(select, names(data))
    if (length(missing_select)) {
        stop("Missing columns: ", paste0(missing_select, collapse = ", "))
    }
    .data <- .subset(data, select)
    if (!is.null(names(select))) {
        rename <- nzchar(names(select))
        names(.data)[rename] <- names(select)[rename]
    }
    if (length(add)) {
        setattr(.data, "row.names", .set_row_names(nrow(data)))
        setattr(.data, "class", "data.frame")
        .data[names(add)] <- lapply(add, list_get, data = data)
    }
    setDT(.data)
    if (isTRUE(copy)) {
        .data <- copy(.data)
    }
    else if (is.character(copy)) {
        if (length(copy)) {
            .data[, `:=`((copy), .SD), .SDcols = copy]
        }
    }
    .data
}

#' fractile
#' @description Fractile ranking
#'
#' @param data vec
#' @param n number of fractiles
#'
#' @export
fractile <- function (x, n) {
    if (n == 0L)
        stop("Invalid n: n must be greater than 0")
    q <- unique(quantile(x, seq(0, 1, length.out = n + 1L), na.rm = TRUE, names = FALSE))
    if (length(q) == 1L) {
        x[!is.na(x)] <- 0
        x
    }
    else {
        cq <- findInterval(x, q, rightmost.closed = TRUE, left.open = TRUE, all.inside = TRUE)
        (as.numeric(cq) - 1)/(n - 1)
    }
}

#' grank
#' @description Group ranking
#'
#' @param x vec
#' @param n number of groups
#' @param ... additional parameters passed to frank
#'
#' @export
grank <- function (x, n, ...) {
    s <- !is.na(x)
    r <- frank(x = x[s], ties.method = "first", ...)
    if (length(r)) {
        vec <- seq.int(1L, length(x), length.out = n + 1L)
        x[s] <- findInterval(r, vec, rightmost.closed = TRUE, left.open = FALSE)
    }
    as.integer(x)
}


#' percentile
#' @description Percentile rankings rescales a numeric vector to ⁠[0, 1]⁠ according to the ranking. If all values are equal, each is mapped to 0.5.
#'
#' @param x vec
#' @param ties.method see data.table::frank(ties.method=)
#' @param ... additional parameters passed to data.table::frank
#'
#' @export
percentile <- function (x, ties.method = "max", ...) {
    s <- !is.na(x)
    r <- frank(x = x[s], ties.method = ties.method, ...)
    if (length(r)) {
        rg <- range(r)
        rd <- diff(rg)
        x[s] <- if (rd == 0)
            0.5
        else (r - rg[[1L]])/rd
        x
    }
    else {
        as.numeric(x)
    }
}


#' rank_score
#' @description Rank score maps the minimum value of an input vector to -1 and maximum value to 1 according to their rankings
#'
#' @param x vec
#' @param ties.method see data.table::frank(ties.method=)
#' @param ... additional parameters passed to data.table::frank
#'
#' @export
rank_score <- function (x, ties.method = "average", ...) {
    2 * percentile(x = x, ties.method = ties.method, ...) - 1
}
