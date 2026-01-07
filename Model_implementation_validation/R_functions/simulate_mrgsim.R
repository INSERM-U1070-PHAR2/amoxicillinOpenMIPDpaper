# Function to simulate based on mrgsolve models
simulate_mrgsim <- function(sim_final, Mellon, interval) { # Mellon is used here as a synonyme for model
  sim_results <- mrgsim(Mellon, sim_final) %>% as.data.frame()
  
  return(sim_results)
}