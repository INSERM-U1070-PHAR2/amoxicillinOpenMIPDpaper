calculate_differences_CMIN <- function(data) {
  ref_CMIN <- data$CMIN[data$REFERENCE == 1][1]
  
  # Calculate differences
  data$diff <- data$CMIN_PRED - ref_CMIN
  
  return(data)
}