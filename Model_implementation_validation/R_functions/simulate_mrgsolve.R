#' Create a data frame with the result of the simulation for one patient
#'
#' @param dosing_regimen A tibble that must have columns time, amt, tinf, ii,
#'   ss, cmt, evid and their values.
#' @param sim_dur : simulation duration
#' @param model : mrgsolve model
#' @param patients To simulate for muliple patients, a csv file containing the number 
#'   of "individuals" simulated and values of the covariates. Otherwise (by default), 
#'   tibble : tibble(ID = 1) and simulation for one patient only.
#' @param sim_step : simulation interval
#' @param param_file If the parameters of the PARAM block have to be updated, 
#'   path to .csv file containing parameters values see details
#'   for format. Otherwise, default value is NULL.
#' @param cov_default Named list default values for covariates and MIC if
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
#' @returns A data frame.
#' @details param_file: The csv file must contain a column for parameters names
#'   and another for parameters means. cov_default : Contain covariates and MICs
#'   values. When calling the function, the arguments names must be written
#'   exactly as they are written in the model. Even if some individual
#'   covariates values are in "patients", a default value must be declared when
#'   calling the function. 
#'   omega_mat : a way to create the matrix is to use cmat, 
#'   bmat or dmat that create matrices from vector input. In mrgsolve, OMEGA is
#'   a diagonal matrix by default. 
#' 
#'   


simulate_mrgsolve <-
  function(dosing_regimen,
           sim_dur,
           model,
           patients = tibble::tibble(ID = 1),
           sim_step,
           param_file = NULL,
           cov_default,
           omega_mat,
           sigma_mat = NULL) {
    dose_ss <- create_mrgsolve_ev(dosing_regimen, sim_dur)
    param_table <- create_param(model, param_file, cov_default)
    idataset <- patients
    
    add_omega_mat <- function(model,omat_arg = omega_mat){
      if(is.null(omat_arg)) {model} else if(unlist(omat_arg) |> list() == "no_iiv") {
        model |>
          mrgsolve::zero_re(omega)} else {mrgsolve::omat(model,omat_arg)}
    }  
    
    add_sigma_mat <- function(model,smat_arg = sigma_mat){
      if(is.null(smat_arg)) {model} else if(unlist(smat_arg) |> list() == "no_ruv") {
        mrgsolve::zero_re(model,sigma)} else {mrgsolve::smat(model,smat_arg)}
     
      }

  model |>
      add_omega_mat(omega_mat) |>
      add_sigma_mat(sigma_mat) |>
      mrgsolve::idata_set(data = idataset) |>
      mrgsolve::param(param_table) |>
      mrgsolve::ev(dose_ss) |>
      mrgsolve::mrgsim(end = sim_dur,
                       delta = sim_step,
                       recover = "regimen_id") |>
      mrgsolve::as.data.frame()
  }

