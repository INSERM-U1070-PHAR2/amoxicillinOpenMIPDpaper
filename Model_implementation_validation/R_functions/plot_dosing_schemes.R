plot_dosing_schemes <- function(data_ref, dosing_schemes) {
  plot_list_before <- list()
  plot_list_after <- list()
  
  for (i in seq_along(dosing_schemes)) {
    scheme <- dosing_schemes[i]
    
    # Extract FREQ (=interdose interval)
    scheme_data <- subset(data_ref, DOSING_SCHEME == scheme)
    freq_value <- unique(scheme_data$FREQ)
    
    # Split data before and after SS
    data_before <- subset(scheme_data, TIME <= freq_value)
    data_after <- subset(scheme_data, TIME > freq_value) %>%
      mutate(TIME = TIME - freq_value)  # Adjust time so that it means time after SS dose
    
    # Plot for before SS
    plot_list_before[[i]] <- ggplot(data_before, aes(x = TIME, y = IPRED, color = MODEL, group = ID)) +
      geom_line() + 
      facet_wrap(~MODEL) +
      theme_minimal() + 
      labs(
        title = paste("First interdose interval", freq_value, scheme),
        x = "Time (h)",
        y = "IPRED (mg/L)",
        color = "MODEL"
      ) +
      theme(
        plot.title = element_text(size=10),
        strip.text = element_text(size = 8), 
        axis.text = element_text(size = 8),
        legend.position = "none"
      )
    
    # Plot for after SS is reached
    plot_list_after[[i]] <- ggplot(data_after, aes(x = TIME, y = IPRED, color = MODEL, group = ID)) +
      geom_line() + 
      facet_wrap(~MODEL) +
      theme_minimal() + 
      labs(
        title = paste("Steady-state", freq_value, scheme),
        x = "Time after steady-state dose (h)",
        y = "IPRED (mg/L)",
        color = "MODEL"
      ) +
      theme(
        plot.title = element_text(size=10),
        strip.text = element_text(size = 8), 
        axis.text = element_text(size = 8),
        legend.position = "none"
      )
  }
  
  list(before = plot_list_before, after = plot_list_after)
}
