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
  col <- hcl.colors(length(names_), "viridis")
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
plot_hist <- function(dt, value_col, facet_col = NA, n_bins = 10L, use_density_hight = TRUE) {
  if (is.character(facet_col)) {
    if (use_density_hight) {
      # ggplot(dt, aes_string(x = value_col, fill = facet_col)) +
      ggplot(dt, aes(x = .data[[value_col]], fill = .data[[facet_col]])) +
        geom_histogram(bins = n_bins, aes(y = after_stat(density))) +
        geom_density(color = "red", size = 1) +
        facet_wrap(as.formula(paste("~", facet_col))) +
        theme_minimal()
    } else {
      # ggplot(dt, aes_string(x = value_col, fill = facet_col)) +
      ggplot(dt, aes(x = .data[[value_col]], fill = .data[[facet_col]])) +
        geom_histogram(bins = n_bins) +
        geom_density(color = "red", size = 1) +
        facet_wrap(as.formula(paste("~", facet_col))) +
        theme_minimal()
    }
  } else {
    if (use_density_hight) {
      # ggplot(dt, aes_string(x = value_col)) +
      ggplot(dt, aes(x = .data[[value_col]])) +
        geom_histogram(bins = n_bins, aes(y = after_stat(density))) +
        geom_density(color = "red", size = 1) +
        theme_minimal()
    } else {
      # ggplot(dt, aes_string(x = value_col)) +
      ggplot(dt, aes(x = .data[[value_col]])) +
        geom_histogram(bins = n_bins) +
        geom_density(color = "red", size = 1) +
        theme_minimal()
    }
  }
}

#' plot_qq
#' @description Functions for plot qq
#'
#' @param dt data
#' @param value_col to display
#' @param facet_col group by
#'
#' @import ggplot2
#' @export
plot_qq <- function(dt, value_col, facet_col = NA) {
  if (is.character(facet_col)) {
    # ggplot(dt, aes_string(sample = value_col, color = facet_col)) +
    ggplot(dt, aes(sample = .data[[value_col]], color = .data[[facet_col]])) +
      stat_qq() +
      stat_qq_line() +
      facet_wrap(as.formula(paste("~", facet_col))) +
      theme_minimal()
  } else {
    # ggplot(dt, aes_string(sample = value_col)) +
    ggplot(dt, aes(sample = .data[[value_col]])) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal()
  }
}

#' plot_heat
#' @description Functions for plot heat
#'
#' @param dt data
#' @param x_col to group
#' @param y_col to group
#' @param value_col to display
#'
#' @import ggplot2
#' @export
plot_heat <- function(dt, x_col, y_col, value_col) {
  ggplot(dt, aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[value_col]])) +
    geom_tile(color = "black") +
    scale_fill_gradient(low = "white", high = "red") +
    geom_text(aes(label = .data[[value_col]]), color = "white", size = 4)
}

#' plot_bar
#' @description Functions for plot bar
#'
#' @param dt data
#' @param category_col to display
#' @param value_col to display
#' @param facet_col group by
#'
#' @import ggplot2 data.table
#' @export
plot_bar <- function(dt, category_col, value_col, facet_col = NA) {
  dt[, (category_col) := as.factor(get(category_col))]
  if (is.character(facet_col)) {
    dt[, (facet_col) := as.factor(get(facet_col))]
    ggplot(dt, aes(x = .data[[category_col]], y = .data[[value_col]], fill = .data[[category_col]])) +
      geom_col() +
      geom_text(aes(label = .data[[value_col]]), vjust = -0.5) +
      facet_wrap(as.formula(paste("~", facet_col))) +
      theme_minimal()
  } else {
    ggplot(dt, aes(x = .data[[category_col]], y = .data[[value_col]], fill = .data[[category_col]])) +
      geom_col() +
      geom_text(aes(label = .data[[value_col]]), vjust = -0.5) +
      theme_minimal()
  }
}

#' plot_scatter
#' @description Functions for plot scatter
#'
#' @param dt data
#' @param x_col to display
#' @param y_col to display
#' @param facet_col group by
#' @param color_col group by
#'
#' @import ggplot2 data.table
#' @export
plot_scatter <- function(dt, x_col, y_col, facet_col = NA, color_col = NA) {
  if (is.character(facet_col)) {
    dt[, (facet_col) := as.factor(get(facet_col))]
    ggplot(dt, aes(x = .data[[x_col]], y = .data[[y_col]])) +
      geom_point(color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      facet_wrap(as.formula(paste("~", facet_col))) +
      theme_minimal()
  } else {
    if (is.character(color_col)) {
      dt[, (color_col) := as.factor(get(color_col))]
      ggplot(dt, aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[color_col]])) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        theme_minimal()
    } else {
      ggplot(dt, aes(x = .data[[x_col]], y = .data[[y_col]])) +
        geom_point(color = "steelblue") +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        theme_minimal()
    }
  }
}

#' plot_violin
#' @description Functions for plot violin
#'
#' @param dt data
#' @param x_col to display
#' @param y_col to display
#' @param facet_col group by
#' @param color_col group by
#'
#' @import ggplot2 data.table
#' @export
plot_violin <- function(dt, x_col = NA, y_col, facet_col = NA, color_col = NA) {
  if (is.character(facet_col)) {
    dt[, (facet_col) := as.factor(get(facet_col))]
    if (is.character(color_col)) {
      dt[, (color_col) := as.factor(get(color_col))]
      if (is.na(x_col) | x_col == "") {
        ggplot2::ggplot(dt, aes(x = "", y = .data[[y_col]], fill = .data[[color_col]])) +
          geom_violin(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.6, trim = FALSE) +
          geom_jitter(width = 0.2, alpha = 0.5) + 
          facet_wrap(as.formula(paste("~", facet_col))) +
          theme_minimal()
      } else {
        ggplot2::ggplot(dt, aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[color_col]])) +
          geom_violin(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.6, trim = FALSE) +
          geom_jitter(width = 0.2, alpha = 0.5) + 
          facet_wrap(as.formula(paste("~", facet_col))) +
          theme_minimal()
      }
    } else {
      if (is.na(x_col) | x_col == "") {
        ggplot2::ggplot(dt, aes(x = "", y = .data[[y_col]], fill = .data[[facet_col]])) +
          geom_violin(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.6, trim = FALSE) +
          geom_jitter(width = 0.2, alpha = 0.5) + 
          facet_wrap(as.formula(paste("~", facet_col))) +
          theme_minimal()
      } else {
        ggplot2::ggplot(dt, aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[facet_col]])) +
          geom_violin(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.6, trim = FALSE) +
          geom_jitter(width = 0.2, alpha = 0.5) + 
          facet_wrap(as.formula(paste("~", facet_col))) +
          theme_minimal()
      }
    }
  } else {
    if (is.character(color_col)) {
      dt[, (color_col) := as.factor(get(color_col))]
      if (is.na(x_col) | x_col == "") {
        ggplot2::ggplot(dt, aes(x = "", y = .data[[y_col]], fill = .data[[color_col]])) +
          geom_violin(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.6, trim = FALSE) +
          geom_jitter(width = 0.2, alpha = 0.5) + 
          theme_minimal()
      } else {
        ggplot2::ggplot(dt, aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[color_col]])) +
          geom_violin(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.6, trim = FALSE) +
          geom_jitter(width = 0.2, alpha = 0.5) + 
          theme_minimal()
      }
    } else {
      if (is.na(x_col) | x_col == "") {
        ggplot2::ggplot(dt, aes(x = "", y = .data[[y_col]])) +
          geom_violin(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.6, trim = FALSE) +
          geom_jitter(width = 0.2, alpha = 0.5) + 
          theme_minimal()
      } else {
        ggplot2::ggplot(dt, aes(x = .data[[x_col]], y = .data[[y_col]])) +
          geom_violin(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.6, trim = FALSE) +
          geom_jitter(width = 0.2, alpha = 0.5) + 
          theme_minimal()
      }
    }
  }
}
