assign_quantiles_categories <- function(data, covariate, is_categorical = FALSE) {
  if (is_categorical) {
    data[[paste0("QUANTILE_", covariate)]] <- as.character(data[[covariate]])
  } else {
    # Calculate quantiles for continuous covariates (WT, CRCL)
    quantile_bins <- quantile(data[[covariate]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    
    # The quantile will be named based on lower and upper bounds
    data[[paste0("QUANTILE_", covariate)]] <- cut(
      data[[covariate]], 
      breaks = quantile_bins, 
      include.lowest = TRUE, 
      labels = paste0(
        signif(head(quantile_bins, -1), 3), "-", 
        signif(tail(quantile_bins, -1), 3)
      )
    )
  }
  
  return(data)
}