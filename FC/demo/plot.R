library(ggplot2)
library(data.table)

data <- data.table(
    value = rnorm(100), # 100 random normal values
    group = factor(rep(c("A", "B"), each = 50)), # Two groups
    facet = factor(rep(c("X", "Y"), times = 50)) # Two facets
)

FC::plot_violin(data, y_col = "value")
FC::plot_violin(data, x_col = "group", y_col = "value", color_col = "group")
FC::plot_violin(data, x_col = "group", y_col = "value", facet_col = "facet", color_col = "group")
