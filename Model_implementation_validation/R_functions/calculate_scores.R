calculate_scores <- function(data, quantile_col, param) {
  scores <- data %>%
    group_by(MODEL, .data[[quantile_col]]) %>%
    summarise(
      MPE = mean(abs(diff) / .data[[param]], na.rm = TRUE),  # relative Mean Prediction Error (Bias)
      RMSE = sqrt(mean(diff^2, na.rm = TRUE)),     # Root Mean Squared Error (Precision)
      mean_param = mean(.data[[param]], na.rm = TRUE),        # mean Cmax for relative RMSE
      .groups = "drop"
    ) %>%
    mutate(
      RMSE = RMSE / mean_param,                    # relative RMSE
      score_MPE = exp(abs(MPE) * pen_MPE),
      score_RMSE = exp(abs(RMSE) * pen_RMSE),
      MODEL_INFLUENCE = score_RMSE * score_MPE     # Calculate model influence
    ) %>%
    ungroup()
  
  return(scores)
}
