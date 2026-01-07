#' Compute fToverMIC between specified timepoints from mrgsolve simulation
#'
#' @param sim_tbl tibble output of an mrgsolve simulation
#' @param from start of time interval over which fToverMIC will be computed
#' @param to end of time interval over which fToverMIC will be computed
#'
#' @return a tibble with added columns TOVERMIC and fT_over_mic. See details.
#' @details Make sure from and to are the same unit as time in the sim_tbl.
#'   Column descriptions:
#' * TOVERMIC absolute duration for which the unbound concentration is above
#'   the MIC. Has the same units as the time column of sim_tbl
#' * ft_over_mic fraction of the interval for which the unbound concentration is
#'   above the MIC
#' @md
#' @examples
compute_ft_over_mic <- function(sim_tbl,
                                from = 0,
                                to = 24) {
  duration <- to - from

  sim_tbl |>
    dplyr::filter(time %in% c(from, to)) |>
    dplyr::mutate(TOVERMIC = TOVERMIC) |>
    dplyr::group_by(ID, regimen_id, MIC) |>
    dplyr::summarise(
      TOVERMIC = TOVERMIC[time == to] - TOVERMIC[time == from],
      ft_over_mic = round(TOVERMIC / duration, digits = 4)
    ) |>
    dplyr::ungroup()
}
