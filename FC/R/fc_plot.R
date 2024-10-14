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