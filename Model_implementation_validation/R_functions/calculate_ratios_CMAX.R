calculate_ratios_CMAX <- function(data) {
  ref_CMAX<- data$CMAX[data$REFERENCE == 1][1]
  
  # Calculate differences
  data$ratio <- (data$CMAX_PRED / ref_CMAX)*100
  
  return(data)
}