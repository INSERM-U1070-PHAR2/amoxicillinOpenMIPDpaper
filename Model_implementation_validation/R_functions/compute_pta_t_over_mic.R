#' Compute probability of target attainment based on fToverMIC
#'
#' @param tbl_t_over_mic tibble with columns ft_over_mic and MIC
#' @param target target fraction of time above MIC
#'
#' @return tibble with fraction of target attainment for each MIC value in
#'   tbl_t_over_mic
#' @export
#'
#' @examples
compute_pta_t_over_mic <- function(tbl_t_over_mic,
                                   target) {
  tbl_t_over_mic |>
    dplyr::summarise(
      .by = MIC,
      mean_ft_over_mic = mean(ft_over_mic),
      sd_ft_over_mic = sd(ft_over_mic),
      median_ft_over_mic = median(ft_over_mic),
      "PTA_{target}" := sum(ft_over_mic >= target) / dplyr::n()
    )
}
