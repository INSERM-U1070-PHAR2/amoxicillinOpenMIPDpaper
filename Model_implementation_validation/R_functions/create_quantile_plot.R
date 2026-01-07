create_quantile_plot <- function(data, variable, y_var = "WEIGHT", model_var = "MODEL", 
                                 title_prefix = "Average model weight for",
                                 y_label = "Average weight", ylim_vals = c(0, 1)) {
  # Remove NAs
  clean_data <- test_data %>%
    dplyr::filter(!is.na(.data[[variable]]) & !is.na(.data[[y_var]]) & !is.na(.data[[model_var]]))
  
  if (is.numeric(clean_data[[variable]])) {
    # Get quantiles
    clean_data <- clean_data %>%
      mutate(quantile_group = ntile(.data[[variable]], 4))
    
    # Calculate quantile ranges
    quantile_ranges <- quantile(clean_data[[variable]], probs = seq(0, 1, 0.25), na.rm = TRUE)
    quantile_labels <- paste0(
      round(quantile_ranges[-length(quantile_ranges)]), "-", 
      round(quantile_ranges[-1])
    )
    
    # Add quantile labels
    clean_data <- clean_data %>%
      mutate(quantile_label = factor(quantile_group, levels = 1:4, labels = quantile_labels))
    
    x_label <- paste0(variable)
    group_var <- "quantile_label"
  } else {
    # Convert to factor
    clean_data <- clean_data %>%
      mutate(factor_label = factor(.data[[variable]]))
    
    x_label <- variable
    group_var <- "factor_label"
  }
  
  # Calculate average weight for each model and group
  averages <- clean_data %>%
    group_by(.data[[model_var]], .data[[group_var]]) %>%
    summarise(avg_y = mean(.data[[y_var]], na.rm = TRUE), .groups = "drop")
  
  # Grouped bar plot
  ggplot(averages, aes(x = .data[[group_var]], y = avg_y, fill = .data[[model_var]])) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    labs(
      title = paste(title_prefix, variable),
      x = x_label,
      y = y_label,
      fill = model_var
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    ylim(ylim_vals) +
    theme(
      legend.position = "bottom",
      text = element_text(size = 12)) +
    theme(
      axis.title = element_text(size = 20), 
      plot.title = element_text(size=24),
    )
}
