fit_cov_wrapper <- function(cov_data, cov_name, min_plot, max_plot, tol = 0.001) {
  
  #' Wrapper to get covariate quantiles from covariate characteristics table and fit normal and lognormal distribution
  #' to observed quantiles 
  #'
  #' @param cov_data tibble containing the covariate information
  #' @param cov_name character. name of the covariate to select, comes from the Covariate column of cov_data.
  #' @param min_plot minimal value of the covariate for which the distribution should be plotted
  #' @param max_plot maximal value of the covariate for which the distribution should be plotted
  #' @param tol numeric. tolerance value for the fitting algorithm, default is 0.001
  #' @return list with :
  #'  - plot : ggplot2 showing the fitted distributions and the observed covariate
  #'  - lnorm_par : named numeric vector with the lognormal distribution parameters
  #'  - norm_par : named numeric vector with the normal distribution parameters
  
  
  cov_df <- extract_cov_param(cov_data, cov_name)
  
  evaluate_distrib(
    p = cov_df$p,
    q = cov_df$q,
    min_plot = min_plot,
    max_plot = max_plot,
    cov_name = cov_df$cov_name,
    cov_unit = cov_df$cov_unit,
    tol = tol
  )
  
}