sim_norm_without_negatives <- function(n,mean,sd){
  
  #' Simulate values from normal distribution but resample negative values
  #'
  #' @param n numeric. number of samples to simulate
  #' @param mean numeric. mean of the normal distribution
  #' @param sd numeric. standard deviation of the normal distribtuion
  #' @return numeric vector of n values
  sim_values <- rnorm(n,mean,sd)
  neg_values <- sim_values[sim_values<=0]
  pos_values <- sim_values[sim_values>0]
  
  while(length(neg_values)>0){
    new_values <- rnorm(length(neg_values),mean,sd)
    sim_values <- c(pos_values,new_values)
    neg_values <- sim_values[sim_values<=0]
    pos_values <- sim_values[sim_values>0]
  }
  sim_values
}