calculate_ratios_SIM <- function(data) {
  ref_IPRED <- data$IPRED[data$REFERENCE == 1][1]
  
  # Calculate differences
  data$ratio <- (data$PRED / ref_IPRED)*100
  
  return(data)
}