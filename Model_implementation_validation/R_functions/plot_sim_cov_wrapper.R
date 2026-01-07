plot_sim_cov_wrapper <- function(cov_data,cov_name,cov_sim){
  cov_df <- extract_cov_param(cov_data, cov_name)
  
  evaluate_cov_sim(cov_df$p,cov_df$q,cov_df$cov_name,cov_df$cov_unit,
                   cov_sim)
}