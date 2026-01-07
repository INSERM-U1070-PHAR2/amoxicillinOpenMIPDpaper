plot_percentiles <- function(data_percentiles_ref, dosing_schemes) {
  library(ggplot2)
  library(patchwork)
  
  plot_list_ref <- list()
  
  for (i in seq_along(dosing_schemes)) {
    plot_list_ref[[i]] <- ggplot(
      subset(data_percentiles_ref, DOSING_SCHEME == dosing_schemes[i]),
      aes(x = TIME)
    ) +
      geom_line(aes(y = P5, color = MODEL), linetype = "dashed") +
      geom_line(aes(y = P50, color = MODEL), size = 1) +
      geom_line(aes(y = P95, color = MODEL), linetype = "dashed") +
      theme_minimal() +
      labs(
        title = paste("Reference model -", dosing_schemes[i]),
        x = "Time (binned)",
        y = "IPRED (mg/L)",
        color = "MODEL"
      ) +
      theme(
        plot.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 8),
        legend.position = "below"
      )
  }
  
  return(plot_list_ref)
}