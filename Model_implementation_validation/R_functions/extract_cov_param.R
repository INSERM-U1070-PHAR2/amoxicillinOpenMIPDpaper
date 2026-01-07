extract_cov_param <- function(cov_data, cov_name) {
  
  #' Get covariate quantiles from covariate characteristics table
  #'
  #' @param cov_data tibble containing the covariate information
  #' @param cov_name character. name of the covariate to select, comes from the Covariate column of cov_data.
  #' @return list with :
  #'  - p : numeric vector of probabilities
  #'  - q : numeric vector of quantiles
  #'  - cov_name : character string containing the covariate name
  #'  - cov_unit : character string containing the unit of the covariate
  
  
  cov_filtered_df <- cov_data |>
    filter(Covariate == cov_name) |>
    pivot_longer(
      cols = c(Median, Q1, Q3, Min, Max), # if the format of the csv columns ever changes, this will need to change
      names_to = "p",
      values_to = "q"
    ) |>
    mutate(p = case_match(p, "Median" ~ 0.5, "Q1" ~ 0.25, "Q3" ~ 0.75, "Min" ~ 0.01, "Max" ~ 0.99)) |>
    # if the format of the csv columns ever changes, the code above will need to change
    filter(!is.na(q)) |>
    group_by(Paper) |>
    arrange(p, .by_group = TRUE) |>
    ungroup()
  
  list(
    p = cov_filtered_df$p,
    q = cov_filtered_df$q,
    cov_name = unique(cov_filtered_df$Covariate),
    cov_unit = unique(cov_filtered_df$Unit)
  )
  
  
}