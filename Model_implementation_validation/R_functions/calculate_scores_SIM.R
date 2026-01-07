calculate_scores_SIM <- function(data, quantile_col) {
  scores <- data %>%
    group_by(MODEL, .data[[quantile_col]]) %>%
    summarise(
      MPE = mean(abs(diff) / IPRED, na.rm = TRUE),  # relative Mean Prediction Error (Bias)
      RMSE = sqrt(mean(diff^2, na.rm = TRUE)),     # Root Mean Squared Error (Precision)
      mean_IPRED = mean(IPRED, na.rm = TRUE),        # mean concentration for relative RMSE
      .groups = "drop"
    ) %>%
    mutate(
      RMSE = RMSE / mean_IPRED,                    # relative RMSE
      score_MPE = exp(abs(MPE) * pen_MPE),
      score_RMSE = exp(abs(RMSE) * pen_RMSE),
      MODEL_INFLUENCE = score_RMSE * score_MPE     # Calculate model influence
    ) %>%
    ungroup()
  
  return(scores)
}