library(dplyr)
library(readr)
library(purrr)
library(combinat)
library(fs)
library(rmarkdown)

wd <- getwd()

# Load complete datasets
train_data_full <- read_csv("AMOX_CMIN_TRAIN.csv")
test_data_full  <- read_csv("AMOX_CMIN_TEST.csv")
clin_data <- read.csv("Clinical_amox_data_clean_ICU.csv", quote = "")

# Only categorical covariates change between the cohorts
model_covariates <- list(
  CARLIER  = c("SEX"),
  FOURNIER = c("BURN",  "SEX"),
  MELLON   = c("OBESE", "SEX", "ICU"),
  RAMBAUD  = c( "SEX")
)

# Model list
models <- c("CARLIER", "FOURNIER", "MELLON", "RAMBAUD")

# Generate all combinations of one and two models to remove
model_combinations <- c(
  combn(models, 1, simplify = FALSE),
  combn(models, 2, simplify = FALSE)
)

# Number of subjects to keep (1900 in the original training data)
n <- 1900

# Loop over each model combination
for (combo in model_combinations) {
  
  # Create subfolder
  combo_name <- paste(combo, collapse = "_")
  subfolder <- file.path("exclude", combo_name) 
  dir_create(subfolder)
  
  # Filter training data for MODEL and MODEL_COHORT
  train_filtered <- train_data_full %>%
    filter(!(MODEL %in% combo | MODEL_COHORT %in% combo))
  
  clin_filtered <- clin_data %>%
    filter(!(MODEL %in% combo))
  
  # Get first 1900 subjects
  valid_ids <- unique(train_filtered$ID)
  selected_ids <- valid_ids[1:min(n, length(valid_ids))]
  batch_train <- train_filtered %>% filter(ID %in% selected_ids)
  
  # Filter test data only for MODEL (same test data for each scenario, but excluded models are not used for predictions)
  batch_test <- test_data_full %>% filter(!(MODEL %in% combo))
  
  # Save filtered datasets to the subfolder
  train_name <- paste0("AMOX_CMIN_TRAIN_", combo_name, ".csv")
  write_csv(batch_train, file.path(subfolder, train_name))
  test_name <- paste0("AMOX_CMIN_TEST_", combo_name, ".csv")
  write_csv(batch_test, file.path(subfolder, test_name))
  file.copy("template.Rmd", file.path(subfolder, "template.Rmd"), overwrite = TRUE)
  
  
  write_csv(clin_filtered, file.path(subfolder, "Clinical_amox_data_clean_ICU.csv"))
  
  # Create categorical cov argument
  included_models <- setdiff(models, combo)
  covariates_included <- model_covariates[included_models]
  categorical_cov <- sort(unique(unlist(covariates_included)))
  
  rmarkdown::render(
  input = file.path(subfolder, "template.Rmd"),
  output_file = paste0("Precision_dosing_methods_", combo_name, ".html"),
  output_dir = subfolder,
  params = list(categorical_cov = categorical_cov))
}


