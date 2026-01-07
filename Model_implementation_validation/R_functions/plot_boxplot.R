plot_boxplot <- function(data, x_var, title) {
  ggplot(data, aes(x = .data[[x_var]], y = ratio, fill = MODEL)) +
    geom_boxplot() +
    scale_y_log10() +
    geom_hline(yintercept = c(80, 125), linetype = "dashed", color = "red") +
    labs(title = title, x = x_var, y = "Ratio (%)") +
    theme_bw() +
    theme(strip.text = element_text(size = 10, face = "bold"))
}