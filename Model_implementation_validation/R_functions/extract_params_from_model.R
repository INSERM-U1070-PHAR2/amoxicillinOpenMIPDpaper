#' Create a csv file with parameters names and default values extracted from the model 
#'
#' @param article_doi DOI of the article, has to be inside quotation marks
#' @param path_to_the_model Path to the model cpp file
#' @param path Path to the created csv file
#' @returns A csv file
#' 

create_csv_param_2 = function(article_doi, path_to_the_model, path_csv_parameters) {
  
  my_model = mrgsolve::mread(model = path_to_the_model,show_col_types = F)
  
  data = mrgsolve:::details(my_model) |>
    mrgsolve::as.data.frame() |>
    dplyr::filter(block == "PARAM"|block == "OMEGA" | block == "SIGMA") |>
    dplyr::select(- options) |>
    dplyr::mutate(DOI = article_doi) |>
    dplyr::relocate(DOI) |>
    dplyr::rename(parameter_name = name,
                  parameter_description = descr,
                  mean = value)
  
  data[data == '.'] <- 'unitless'
  
  # replace NA by variance/covariance values in OMEGA
  omega_tbl <- mrgsolve::omat(my_model) |> mrgsolve::as.list() |> getElement(1) |> gdata::unmatrix() |> stack() |>
    tidyr::separate_wider_delim(
      cols = ind,
      delim = ":",
      names = c("param_var_1", "param_var_2")
    ) |>
    dplyr::mutate(
      name = dplyr::case_when(
        param_var_1 == param_var_2 ~ param_var_1,
        param_var_1 != param_var_2 ~ paste(param_var_1, param_var_2, sep = "-")
      )
    ) |> 
    dplyr::select(-c(param_var_1,param_var_2))
  
  data2 = data |> 
    dplyr::left_join(omega_tbl,
                     by = c("parameter_name" = "name")) |> 
    dplyr::mutate(mean = dplyr::case_when(
      is.na(mean) ~ values,
      .default = mean
    )
    ) |> 
    dplyr::select(-values)
  
  # replace NA by variance/covariance values in SIGMA and create csv file
  if("SIGMA"%in%data2$block) {
  sigma_tbl <- mrgsolve::smat(my_model) |> mrgsolve::as.list() |> getElement(1) |> gdata::unmatrix() |> stack() |>
    tidyr::separate_wider_delim(
      cols = ind,
      delim = ":",
      names = c("param_var_1", "param_var_2")
    ) |>
    dplyr::mutate(
      name = dplyr::case_when(
        param_var_1 == param_var_2 ~ param_var_1,
        param_var_1 != param_var_2 ~ paste(param_var_1, param_var_2, sep = "-")
      )
    ) |> 
    dplyr::select(-c(param_var_1,param_var_2))
  
  data3 = data2 |> 
    dplyr::left_join(sigma_tbl,
                     by = c("parameter_name" = "name")) |> 
    dplyr::mutate(mean = dplyr::case_when(
      is.na(mean) ~ values,
      .default = mean
    )
    ) |> 
    dplyr::select(-values)
  
  write.csv(x = data3, file = path_csv_parameters)} else {
    write.csv(x = data2, file = path_csv_parameters)
    }
}



