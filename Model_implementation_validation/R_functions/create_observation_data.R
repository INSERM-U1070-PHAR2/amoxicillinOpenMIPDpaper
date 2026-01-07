# Function to create observation grid (lines with concentration measurement)
create_observation_data <- function(cov_range, id_range, obs_length) {
  COV1 <- COV[cov_range, ]  # Extract covariates for the specified range
  ID <- id_range            # ID range for the regimen
  
  # Create base observation data (as these are lines with concentrations, AMT, II, RATE and EVID are always 0)
  obs_data <- data.frame(
    ID = ID,                     # ID meaning a set of covariates
    amt = rep(0, length(ID)),    # Dose amount in mg
    ii = rep(0, length(ID)),     # Interdose interval in h
    rate = rep(0, length(ID)),   # Infusion rate
    evid = rep(0, length(ID)),   # EVID
    cmt = "CENTRAL"              # Central compartment
  )
  
  # Add covariates
  obs_data <- cbind(obs_data, COV1)
  
  # Repeat observation data for all observation times
  obs_repeated <- obs_data[rep(1:nrow(obs_data), each = obs_length), ]
  time <- rep(seq(from = 0, to = 72, length.out = obs_length), times = length(ID))   # Latest possible time is 72 h as amoxicillin has a short half life
  
  # Combine with time
  final_obs_data <- cbind(obs_repeated, time)
  return(final_obs_data)
}