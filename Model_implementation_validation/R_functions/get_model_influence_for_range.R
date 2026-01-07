get_model_influence_for_range <- function(value, covariate, model, all_scores) {
  model_influence <- all_scores %>%
    dplyr::filter(MODEL == model & COVARIATE == covariate & value >= lower & value <= upper) %>%
    dplyr::select(MODEL_INFLUENCE) %>%
    pull() 
  if (length(model_influence) == 1) return(model_influence) else return(NA)
}