#' Calculate variance from variation coefficient of a parameter.
#'
#' @param distribution parameter distribution across the population, can take values "normal" or "lognormal"
#' @param mean parameter mean if the parameter is normally distributed
#' @param cv parameter's variation coefficient
#'
#' @return the variance of the parameter
#' @details Make sure to write "normal" or "lognormal" between quotation marks.
#'

cv_to_var = function(distribution, mean = NULL, cv) {
  if(distribution == "normal") {var = (cv*mean)^2}
  if(distribution == "lognormal") {var = log(cv^2 + 1)}
  var
}


