calculate_differences_CMAX <- function(data) {
  ref_CMAX <- data$CMAX[data$REFERENCE == 1][1]
  
  # Calculate differences
  data$diff <- data$CMAX_PRED - ref_CMAX
  
  return(data)
}
