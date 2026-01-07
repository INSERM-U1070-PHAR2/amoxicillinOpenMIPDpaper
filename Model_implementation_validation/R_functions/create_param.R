#' Create a named vector containing parameters and covariates for simulation
#' @param model mrgsolve model .cpp file.
#' @param param_file Either NULL (default) or path to .csv file containing parameters values see details
#'   for format
#' @param cov_default Named list default values for covariates and MIC if
#'   TOVERMIC is calculated during the simulation. See details additional
#'   informations.
#' @returns A numeric vector.
#' @details If NULL, default values of the model are used (except for covariates).
#'   The CSV file must contain a column for parameters names and another
#'   for parameters means. cov_default : When calling the function, in the arguments,
#'   the names of the parameters that aren't in the CSV file must be written
#'   exactly as it is written in the model :
#' @example here::here("quarto/functions/example_create_param_mic_amox.R")

create_param <- function(model, param_file = NULL, cov_default) {
  
  if(is.null(param_file)) {param_table <- mrgsolve::param(model) |>
    mrgsolve::as.data.frame() |>
    dplyr::select(-c(names(cov_default))) |>
    dplyr::bind_cols(cov_default) |>
    unlist()
  } else {
  param_table <- readr::read_csv(file = param_file, show_col_types = FALSE) |>
    dplyr::select(parameter_name, mean) |>
    tidyr::pivot_wider(
      names_from = parameter_name,
      values_from = mean
    ) |>
    dplyr::bind_cols(cov_default) |>
    unlist()

  model_param_names <- mrgsolve::param(model) |> names()
  # check that all parameters are provided else return error
  if (!all(model_param_names %in% names(param_table))) {
    missing_parameters <-
      model_param_names[!model_param_names %in% names(param_table)]
    error_message <- paste(
      "Parameters/Covariates",
      missing_parameters,
      "are missing"
    )
    stop(error_message)
  }
  param_table |> unlist() 
    }
}
