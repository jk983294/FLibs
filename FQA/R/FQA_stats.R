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
    c(n = n, pos = sum(x > 0, na.rm = TRUE) / n, zero = sum(x == 0, na.rm = TRUE) / n, na = sum(is.na(x)) / n,
    inf = sum(is.infinite(x)) / n, mean = mean, sd = sd, as.list(qs))
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