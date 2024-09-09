#' color.alpha
#' @description Functions for calculating transparent and desaturated colors.
#'
#' @param col str col
#' @param alpha alpha transparency, where 1 means fully opaque and 0 fully transparent, default = 0.2
#'
#' @export
color.alpha <- function(col, alpha = 0.2) {
    acol <- col2rgb(col)
    acol <- rgb(acol[1] / 255, acol[2] / 255, acol[3] / 255, alpha)
    acol
}

#' color.desat
#' @description Functions for calculating transparent and desaturated colors.
#'
#' @param col str col
#' @param amt amount of desaturation of color to apply, where 1 means totally desaturated (grayscale), default = 0.5
#'
#' @export
color.desat <- function(acol, amt = 0.5) {
    acol <- col2rgb(acol)
    ahsv <- rgb2hsv(acol)
    ahsv[2] <- ahsv[2] * amt
    hsv(ahsv[1], ahsv[2], ahsv[3])
}

#' color.dist
#' @description Functions for calculating transparent and desaturated colors.
#'
#' @param x A vector of values to use for calculating distances
#' @param mu  default = 0.
#' @param sd  default = 1.
#' @param col  default = slateblue
#'
#' @export
color.dist <- function(x, mu = 0, sd = 1, col = "slateblue") {
    cols <- sapply(x, function(z) exp(-(z - mu)^2 / sd))
    cols <- sapply(cols, function(z) FC::color.alpha(col, z))
    cols
}
