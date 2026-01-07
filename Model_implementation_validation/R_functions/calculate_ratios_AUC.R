calculate_ratios_AUC <- function(data) {
  ref_AUC<- data$AUC[data$REFERENCE == 1][1]
  
  # Calculate differences
  data$ratio <- (data$AUC_PRED / ref_AUC)*100
  
  return(data)
}