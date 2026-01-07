calculate_differences_SIM <- function(data) {
  ref_IPRED <- data$IPRED[data$REFERENCE == 1][1]
  
  # Calculate differences
  data$diff <- data$PRED - ref_IPRED
  
  return(data)
}