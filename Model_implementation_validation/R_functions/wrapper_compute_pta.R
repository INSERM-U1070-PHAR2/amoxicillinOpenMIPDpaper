#' compute PTA for  1 dosing regimen and a given value for each covariates
#' enables looping through them
#'
#' @param dosing_reg_id A list of string of characters used to select the
#'   dosing regimen in the dosing regimens' csv file.
#' @param path_dosing_regimens A csv file, see details for additional informations
#' @param sim_dur simulation duration
#' @param sim_step : simulation interval
#' @param model : mrgsolve model
#' @param patients a csv file containing the number of "individuals" simulated
#'   and values of the covariates.
#' @param path_parameters if an update is needed, path to .csv file containing 
#' parameters values see details for format.
#' @param cov_default :  Named list default values for covariates and MIC if
#'   TOVERMIC is calculated during the simulation. See details additional
#'   informations.
#' @param omega_mat Can be : a) a matrix of variance covariance for inter-individual
#'   variabilities. Values has to be in the order in which they appear in the
#'   model. or b) NULL in order to use the model's default's values of the variance 
#'   covariance matrix. or c) "no_iiv" to fill the matrix with 0. 
#' @param sigma_mat Can be : a) a matrix of variance covariance for residual 
#'   unexplained variability. Values has to be in the order in which they appear
#'   in the model. or b) NULL (the default argument) in order to use the model's 
#'   default's values of the variance covariance matrix. or c) "no_ruv" to fill 
#'   the matrix with 0. 
#' @param from beginning of the time interval on which PTA are computed
#' @param to end of the time interval on which PTA are computed
#' @param targets vector containing targets (pta values)
#' @details path_dosing_regimens : The csv file containing dosing regimens' informations 
#' must have columns time, amt, tinf, ii, ss, cmt, evid and their values. 
#' path_parameters : The csv file must contain a column for parameters names and another
#' for parameters means.
#' cov_default : When calling the function, in the arguments, names of the parameters 
#' that aren't in the CSV file must be written exactly as it is written in the model

wrapper_compute_pta <- function(dosing_reg_id,
                                path_dosing_regimens,
                                sim_dur,
                                sim_step,
                                model,
                                patients,
                                path_parameters = NULL,
                                cov_default,
                                omega_mat,
                                sigma_mat = NULL,
                                from,
                                to,
                                targets) {
  dosing_reg <- select_dosing_regimens(
    selected_regimen = dosing_reg_id,
    path_to_the_file = path_dosing_regimens
  )
  
  sim <- simulate_mrgsolve(
    dosing_regimen = dosing_reg,
    sim_dur = sim_dur,
    model = model,
    patients = patients,
    sim_step = sim_step,
    param_file = path_parameters,
    cov_default = cov_default,
    omega_mat = omega_mat,
    sigma_mat = sigma_mat
  )
  
  pta <-
    purrr::map(
      targets,
      ~ compute_ft_over_mic(
        sim_tbl = sim,
        from = from,
        to = to
      ) |>
        compute_pta_t_over_mic(target = .x)
    ) |>
    purrr::reduce(dplyr::left_join) |>
    dplyr::mutate(
      CLCR = cov_default$CLCR,
      regimen_id = dosing_reg_id
    )
  
  pta
}