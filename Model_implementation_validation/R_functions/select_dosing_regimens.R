#' Create a tibble of dosing regimens for mrgsolve event
#'
#' @param selected_regimen A list of string of characters used to select the
#'   dosing regimen in the csv file
#' @param path_to_the_file A csv file, see details for additional informations
#' @returns A tibble
#' @details The csv file must have columns time, amt, tinf, ii, ss, cmt, evid
#'   and their values. These column names can't be changed : are chosen to be
#'   used by mrgsolve (see 4.2 here :
#'   https://mrgsolve.org/user-guide/datasets.html)


select_dosing_regimens <- function(selected_regimens, path_to_the_file) {
  readr::read_csv(file = path_to_the_file, show_col_types = FALSE) |>
    dplyr::filter(regimen_id %in% selected_regimens)
}
