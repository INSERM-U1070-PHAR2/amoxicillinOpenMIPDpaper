# Main function to generate dosing regimens
generate_regimen <- function(
    start_id, dose, interval, sim_dur, model_name, Mellon, Mellon_zero, duration
) {
  # ID range of 100 IDs starting at the start ID defined separately for each dosing scheme
  end_id <- start_id + 99
  id_range <- start_id:end_id
  cov_range <- start_id:end_id
  
  # Generate dosing times (until 72 h not included)
  dosing_times <- seq(0, sim_dur - interval, by = interval)
  
  # Calculate number of observation times
  obs_length <- (sim_dur / interval) * sim_dur + 1
  
  # Create dosing data
  dose_data <- create_dosing_data(
    cov_range = cov_range,
    id_range = id_range,
    amt = dose,
    ii = interval,
    rate = dose / duration,
    cmt = "CENTRAL",
    time_seq = dosing_times
  )
  
  # Create observation data
  obs_data <- create_observation_data(
    cov_range = cov_range,
    id_range = id_range,
    obs_length = obs_length
  )
  
  # Combine dosing and observation data
  sim_data <- rbind(dose_data, obs_data) %>%
    arrange(ID, time, desc(evid))
  
  # Process simulation for the original model
  processed_data <- simulate_mrgsim(
    sim_final = sim_data,
    Mellon = Mellon,
    interval = interval
  )
  
  # Finalize results for the original model
  finalized_data <- finalize_results(
    sim_results = processed_data,
    cov_range = cov_range,
    model = model_name,
    dose = dose,
    freq = interval,
    dur = duration
  )
  
  # Simulation for the zero-re model
  processed_data_zero <- simulate_mrgsim(
    sim_final = sim_data,
    Mellon = Mellon_zero,
    interval = interval
  )
  
  # Extract PRED from the simulation with 0 OMEGA and SIGMA matrices (otherwise only IPRED and Y are simulated)
  pred_data <- processed_data_zero %>%
    select(ID, time, PRED = IPRED, AUC_PRED = AUC, Cmax_PRED = Cmax)
  
  # Merge PRED with the original simulation results
  merged_data <- finalized_data %>%
    left_join(pred_data, by = c("ID", "time"))
  
  # Calculate time to reach 90 % of steady state based on half life and then take the subsequent dosing interval
  merged_data$TSS <- ceiling(3.3 * (0.693 * merged_data$Vc) / merged_data$CL / interval) * interval
  
  merged_data_filtered <- merged_data %>%
    filter((time >= 0 & time <= (0 + interval)) | (time >= TSS & time <= (TSS + interval)))
  
  return(merged_data_filtered)
}