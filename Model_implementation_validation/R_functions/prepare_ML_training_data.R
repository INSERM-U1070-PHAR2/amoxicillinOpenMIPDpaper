prepare_ML_training_data <- function(source_data, target_file, n_select = 3) {
  selected_IDs <- c()
  
  for (start in seq(1, 10000, by = 25)) {
    end <- start + 24
    IDs_in_25 <- unique(source_data$ID[source_data$ID >= start & source_data$ID <= end])
    selected_IDs <- c(selected_IDs, head(IDs_in_25, n_select))
  }
  
  filtered_data <- source_data[source_data$ID %in% selected_IDs, ]
  write.csv(filtered_data, target_file, row.names = FALSE, quote = FALSE)
  
  return(filtered_data)
}