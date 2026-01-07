calculate_ratios_CMIN <- function(data) {
  ref_CMIN<- data$CMIN[data$REFERENCE == 1][1]
  
  # Calculate differences
  data$ratio <- (data$CMIN_PRED / ref_CMIN)*100
  
  return(data)
}