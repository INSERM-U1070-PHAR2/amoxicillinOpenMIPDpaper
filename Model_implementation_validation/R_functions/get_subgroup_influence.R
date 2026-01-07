get_subgroup_influence <- function(covariate_value, covariate_name, all_scores) {
  # Get the corresponding scores for the covariate
  covariate_scores <- all_scores %>% dplyr::filter(COVARIATE == covariate_name)
  
  # Filter for the range that matches the covariate value and return the corresponding SUBGROUP_INFLUENCE
  match <- covariate_scores %>% 
    dplyr::filter(covariate_value >= lower & covariate_value <= upper)
  
  # If a match exists, return the SUBGROUP_INFLUENCE, else return NA
  if (nrow(match) > 0) {
    return(first(match$SUBGROUP_INFLUENCE))
  } else {
    return(NA)
  }
}