create_dosing_data <- function(cov_range, id_range, amt, ii, rate, cmt, time_seq) {
  COV1 <- COV[cov_range, ]  # Extract covariates from COV.csv for the specified range (the range is defined later)
  ID <- id_range            # ID range for the regimen
  
  # Create base dosing data
  dosing_data <- data.frame(
    ID = ID,                      # ID meaning a set of covariates
    amt = rep(amt, length(ID)),   # Dose amount in mg
    ii = rep(ii, length(ID)),     # Interdose interval in h
    rate = rep(rate, length(ID)), # Infusion rate
    evid = rep(1, length(ID)),    # EVID
    cmt = cmt                     # Compartment
  )
  
  # Add covariates from COV.csv
  dosing_data <- cbind(dosing_data, COV1)
  
  # Repeat dosing data for all time points
  dosing_repeated <- dosing_data[rep(1:nrow(dosing_data), each = length(time_seq)), ]
  time <- rep(time_seq, times = length(ID))
  
  # Combine with time
  final_dosing_data <- cbind(dosing_repeated, time)
  return(final_dosing_data)
}