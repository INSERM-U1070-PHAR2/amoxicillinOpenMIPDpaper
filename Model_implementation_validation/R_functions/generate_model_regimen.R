generate_model_regimen <- function(model_name, cpp_file, cpp_file_zero, regimens, sim_dur) {
  # Load original model (with IIV and RUV)
  model <- mread(model = cpp_file)
  
  # Set OMEGA and SIGMA matrices to 0 to obtain PRED (IPRED and Y are coded in the model)
  model_zero <- zero_re(model)
  
  # List to store results for all regimens
  regimen_results <- list()
  
  # Process each regimen
  for (i in seq_along(regimens)) {
    regimen <- regimens[[i]]
    start_id <- regimen$start_id
    dose <- regimen$dose
    interval <- regimen$interval
    duration <- regimen$duration
    
    # Generate regimen and store result
    regimen_results[[i]] <- generate_regimen(
      start_id = start_id,
      dose = dose,
      interval = interval,
      sim_dur = sim_dur,
      model_name = model_name,
      Mellon = model,
      Mellon_zero = model_zero,
      duration = duration
    )
  }
  
  # Combine all regimen results into one data frame
  combined_results <- do.call(rbind, regimen_results)
  
  return(combined_results)
}