#' Create ev object for mrgsolve simulation
#'
#' @param dosing_regimen A tibble containing dosing regimen to be simulated see
#'   details for format.
#' @param sim_dur A number setting the simulation duration
#' @returns An mrgsolve ev object
#' @details The dosing regimen tibble must contain columns time, amt, tinf, ii,
#'   ss, cmt, evid and their values. dosing_regimen can be created using
#'   select_a_dosing_regimen() or dosing_regimens()


create_mrgsolve_ev <- function(dosing_regimen, sim_dur) {
  dosing_regimen |>
    dplyr::mutate(addl = dplyr::case_when(
      ii != 0 ~ sim_dur / ii - 1,
      ii == 0 ~ 0
    )) |>
    mrgsolve::as.ev()
}
