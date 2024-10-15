#' mat_plot
#' @description Functions for plot table
#'
#' @param dt data
#' @param first_n 0L first n columns to highlight
#' @param last_n 0L last n columns to highlight
#'
#' @export
mat_plot <- function(dt, first_n = 0L, last_n = 0L, xlab = NULL, ylab = NULL, main = NULL) {
  names_ <- names(dt)
  highlight_n <- first_n + last_n
  col = hcl.colors(length(names_), "viridis")
  if (highlight_n > 0) {
    matplot(dt, type = "l", col = "lightgray", lty = 1, xlab = xlab, ylab = ylab, main = main)
    if (first_n > 0) {
      matlines(dt[, 1:first_n], type = "l", col = col, lty = 1)
    }
    if (last_n > 0) {
      matlines(dt[, (length(dt) - last_n + 1):length(dt)], type = "l", col = col, lty = 1)
    }
    graphics::legend("bottom", names_, col = col, lty = 1, cex = 0.8)
  } else {
    matplot(dt, type = "l", col = col, lwd = 2, lty = 1, xlab = xlab, ylab = ylab, main = main)
    graphics::legend("bottom", names_, col = col, lty = 1, cex = 0.8)
  }
}

#' plot_hist
#' @description Functions for plot histgram
#'
#' @param dt data
#' @param value_col to display
#' @param facet_col group by
#' @param n_bins default to 10 bins
#' @param use_density_hight show density
#'
#' @import ggplot2
#' @export
plot_hist <- function(dt, value_col, facet_col = NA, n_bins = 10L, use_density_hight = TRUE){
  if (is.character(facet_col)) {
    if (use_density_hight) {
      ggplot(dt, aes_string(x = value_col, fill = facet_col)) +
        geom_histogram(bins=n_bins, aes(y = ..density..)) +
        geom_density(color="red", size=1) +
        facet_wrap(as.formula(paste("~", facet_col))) +
        theme_minimal()
    } else {
      ggplot(dt, aes_string(x = value_col, fill = facet_col)) +
        geom_histogram(bins=n_bins) +
        geom_density(color="red", size=1) +
        facet_wrap(as.formula(paste("~", facet_col))) +
        theme_minimal()
    }
  } else {
    if (use_density_hight) {
      ggplot(dt, aes_string(x = value_col)) +
        geom_histogram(bins=n_bins, aes(y = ..density..)) +
        geom_density(color="red", size=1) +
        theme_minimal()
    } else {
      ggplot(dt, aes_string(x = value_col)) +
        geom_histogram(bins=n_bins) +
        geom_density(color="red", size=1) +
        theme_minimal()
    }
  }
}