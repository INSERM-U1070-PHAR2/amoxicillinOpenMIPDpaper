evaluate_cov_sim <- function(p, q, cov_name, cov_unit, cov_sim) {
  #' Plot simulated covariate distribution versus observed quantiles from the paper
  #' @param p numeric vector of probabilities
  #' @param q numeric vector of quantiles
  #' @param cov_name character string containing the covariate name
  #' @param cov_unit character string containing the unit of the covariate
  #' @param cov_sim tibble containing the simulated values for the covariate
  #' @return ggplot2 showing the distributions of the simulated covariate and the observed covariate quantiles
  
  
  #create a dataset with the true percentiles to be able to show them on the plot
  true_percentiles <- tibble(p, q) |> 
    mutate(label = case_match(p, 
                              0 ~ "True min",
                              0.25 ~ "True Q1",
                              0.5 ~ "True median",
                              0.75 ~ "True Q3",
                              1 ~ "True max")) 
  
  #create a dataset with the percentiles of the fitted distributions to be able to show them on the plot
  sim_percentiles <-  tibble(
    probability = p,
    q = quantile(pull(cov_sim,cov_name),p) 
  ) |> 
    mutate(label = case_match(probability, 
                              0 ~ "Sim min",
                              0.25 ~ "Sim Q1",
                              0.5 ~ "Sim median",
                              0.75 ~ "Sim Q3",
                              1 ~ "Sim max"))
  
  # create an initial histogram in order to be able to extract statistics of the binned covariate
  hist_ini <- ggplot(cov_sim, aes(x = .data[[cov_name]])) +
    geom_histogram(bins=15)
  
  hist_data <- layer_data(hist_ini)
  
  plot <- ggplot(hist_data) +
    geom_rect(aes(xmin=xmin,xmax=xmax, ymin=0, ymax=count),
              alpha = 0.5) +
    geom_vline(data = true_percentiles,
               mapping = aes(xintercept = q,color = "True"),
               lty = "dashed") +
    geom_vline(data = sim_percentiles,
               mapping = aes(xintercept = q,color = "Sim"),
               lty = "solid") +
    theme_bw() +
    ylab("Count") +
    xlab(paste0(cov_name, " (", cov_unit, ")"))  +
    
    
    geom_text(
      data = true_percentiles,
      mapping = aes(x = q, label = label, color = "True"),
      show.legend = FALSE,
      y = 0.95*max(hist_data$ymax),
      hjust = 1,
      vjust = -1,
      angle = 90
    ) +
    geom_text(
      data = sim_percentiles,
      mapping = aes(x = q, label = label, color = "Sim"),
      show.legend = FALSE,
      y = 0.95*max(hist_data$ymax),
      hjust = 1,
      vjust = -1,
      angle = 90
    ) +
    scale_color_brewer(name = "Quantiles",
                       palette ="Dark2")
  plot
}
