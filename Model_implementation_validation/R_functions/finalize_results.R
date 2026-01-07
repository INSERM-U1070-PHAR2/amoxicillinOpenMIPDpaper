# Function to merge covariates and finalize
finalize_results <- function(sim_results, cov_range, model, dose, freq, dur) {
  COV1 <- COV[cov_range, ]
  COV1$ID <- cov_range
  
  # Merge with simulation results
  result <- merge(sim_results, COV1, by = "ID", all.x = TRUE)
  
  result <- result %>%
    mutate(MODEL = model, DOSE = dose, FREQ = freq, DUR = dur) %>%
    distinct(ID, TIME = time, .keep_all = TRUE)
  
  return(result)
}