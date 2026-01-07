library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(fs)
library(rmarkdown)
library(conflicted)
conflicts_prefer(dplyr::first)
library(DBI)
library(duckdb)

set.seed(1991)

subject_sizes <- c(100)
n_runs <- 100 # Number of runs
n_cohort <- 4 # Number of model cohorts

con <- DBI::dbConnect(duckdb::duckdb())

DBI::dbExecute(con, "
  CREATE OR REPLACE VIEW AMOX_CMIN_TRAIN AS 
  SELECT * FROM read_parquet('AMOX_CMIN3000.parquet')
")

unique_ids <- dplyr::tbl(con, "AMOX_CMIN_TRAIN") %>%
  group_by(MODEL_COHORT) %>%
  summarise(unique_ids = sql("array_agg(DISTINCT ID)")) %>%
  collect()

batch_test <- read_csv("AMOX_CMIN_TEST.csv")
clin_data <- read.csv("Clinical_amox_data_clean_ICU.csv", quote = "")

# Loop over different subject sizes
for (n_total in subject_sizes) {
  n <- n_total / n_cohort
  
  if (any(sapply(unique_ids$unique_ids, length) < n * n_runs)) {
    message(paste("Not enough subjects for", n_total, "total subjects. Skipping."))
    next
  }
  
  used_ids <- list()
  
  for (i in 1:n_runs) {
    # Random sampling
    batch_ids <- unique_ids %>%
      mutate(sampled_ids = map(unique_ids, function(ids) {
        available_ids <- setdiff(ids, unlist(used_ids))
        sample(available_ids, n)
      })) %>%
      tidyr::unnest(sampled_ids) %>%
      pull(sampled_ids)
    
    used_ids <- append(used_ids, list(batch_ids))
    
    batch_train <- dplyr::tbl(con, "AMOX_CMIN_TRAIN") %>%
     dplyr::filter(ID %in% batch_ids) %>%
      collect()
    
    subfolder <- paste0(n_total, "_", i)
    dir.create(subfolder, showWarnings = FALSE)
    dir.create(file.path(subfolder, "Figures"), showWarnings = FALSE)
    dir.create(file.path(subfolder, "boxplot_data"), showWarnings = FALSE)
    
    file_name <- paste0("AMOX_CMIN_TRAIN_", n_total, "_", i, ".csv")
    write_csv(batch_train, file.path(subfolder, file_name))
    write_csv(batch_test, file.path(subfolder, "AMOX_CMIN_TEST.csv"))
    write_csv(clin_data, file.path(subfolder, "Clinical_amox_data_clean_ICU.csv"))
    file.copy("template.Rmd", file.path(subfolder, "template.Rmd"), overwrite = TRUE)
    
    setwd(subfolder)
    
    tryCatch(
      {
        rmarkdown::render(
          input = "template.Rmd",
          output_file = paste0("Precision_dosing_methods", n_total, "_", i, ".html"),
          params = list(n_total = n_total, run = i, training_data_path = subfolder)
        )
      },
      error = function(e) {
        message("Error in run ", i, " for ", n_total, " subjects: ", e$message)
      }
    )
    
    setwd("..")
    cat("Finished batch", i, "for", n_total, "total subjects\n")
  }
}

# Disconnect duckdb
DBI::dbDisconnect(con, shutdown = TRUE)