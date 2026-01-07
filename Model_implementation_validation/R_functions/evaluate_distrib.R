evaluate_distrib <- function(p, q, cov_name, cov_unit, min_plot, max_plot,tol = 0.001) {
  #' Fit normal and lognormal distribution to observed quantiles
  #' @param p numeric vector of probabilities
  #' @param q numeric vector of quantiles
  #' @param cov_name character string containing the covariate name
  #' @param cov_unit character string containing the unit of the covariate
  #' @param min_plot minimal value of the covariate for which the distribution should be plotted
  #' @param max_plot maximal value of the covariate for which the distribution should be plotted
  #' @param tol numeric. tolerance value for the fitting algorithm, default is 0.001
  #' @return list with :
  #'  - plot : ggplot2 showing the fitted distributions and the observed covariate
  #'  - lnorm_par : named numeric vector with the lognormal distribution parameters
  #'  - norm_par : named numeric vector with the normal distribution parameters
  
  
  lnorm_par <- get.lnorm.par(p = p, q = q, plot = FALSE, show.output = FALSE,tol = tol)
  norm_par <- get.norm.par(p = p, q = q, plot = FALSE, show.output = FALSE,tol=tol)
  
  #create a dataset with the true percentiles to be able to show them on the plot
  true_percentiles <- tibble(p, q) |> 
    mutate(label = case_match(p, 
                              0 ~ "True min",
                              0.25 ~ "True Q1",
                              0.5 ~ "True median",
                              0.75 ~ "True Q3",
                              1 ~ "True max")) 
  
  #create a dataset with the percentiles of the fitted distributiosn to be able to show them on the plot
  distrib_percentiles <-  tibble(
    probability = p,
    lnorm = qlnorm(probability, meanlog = lnorm_par["meanlog"], sdlog =
                     lnorm_par["sdlog"]),
    norm = qnorm(probability, mean = norm_par["mean"], sd = norm_par["sd"])
  ) |>
    pivot_longer(cols = c(lnorm, norm), names_to = "distrib") |>
    mutate(density = case_match(
      distrib,
      "norm" ~ dnorm(value, mean = norm_par["mean"], sd =
                       norm_par["sd"]),
      "lnorm" ~ dlnorm(value, meanlog = lnorm_par["meanlog"], sdlog =
                         lnorm_par["sdlog"])
    )) |> 
    mutate(label = case_match(probability, 
                              0 ~ "Min",
                              0.25 ~ "Q1",
                              0.5 ~ "Median",
                              0.75 ~ "Q3",
                              1 ~ "Max")) 
  
  
  #simulate 1000 datapoints from the fitted distributions to show on the plot
  param_data <- tibble(
    variable = seq(min_plot, max_plot, length.out = 1000),
    lnorm = dlnorm(variable, meanlog = lnorm_par["meanlog"], sdlog = lnorm_par["sdlog"]),
    norm = dnorm(variable, mean = norm_par["mean"], sd = norm_par["sd"])
  ) |>
    pivot_longer(cols = c(lnorm, norm), names_to = "distrib")
  
  
  plot <- ggplot(param_data, aes(x = variable, y = value, color = distrib)) +
    geom_vline(data = true_percentiles, mapping = aes(xintercept = q),lty = "dashed") +
    geom_line() +
    geom_segment(data = distrib_percentiles, aes(x = value, y = 0, yend =
                                                   density)) +
    theme_bw() +
    scale_color_brewer("Distribution", palette = "Dark2") +
    ylab("Density") +
    xlab(paste0(cov_name, " (", cov_unit, ")")) +
    annotate(
      geom = "text",
      label = paste0(
        "N ~ (",
        signif(norm_par["mean"], 3),
        ", ",
        signif(norm_par["sd"], 3),
        ")\n",
        "LN ~ (",
        signif(exp(lnorm_par["meanlog"]), 3),
        ", ",
        signif(compute_cv_from_sd_lnorm(lnorm_par["sdlog"]), 3) * 100,
        " %)"
      ),
      x = min(param_data$variable) + 0.99 * diff(range(param_data$variable)),
      y = min(param_data$value) + 0.99 * diff(range(param_data$value)),
      hjust = 1,
      vjust = 1
    )+
    geom_text(
      data = true_percentiles,
      mapping = aes(x = q,label = label,color=NULL),
      show.legend = FALSE,
      y = min(param_data$value) + 0.95 * diff(range(param_data$value)),
      hjust = 1,
      vjust = -1,
      angle = 90
    )+
    geom_text(
      data = distrib_percentiles,
      mapping = aes(x = value, y=density,label = label),
      show.legend = FALSE,
      hjust = 1,
      vjust = -1,
      angle = 90
    )
  
  list(plot = plot,
       lnorm_par = lnorm_par,
       norm_par = norm_par)
  
}
