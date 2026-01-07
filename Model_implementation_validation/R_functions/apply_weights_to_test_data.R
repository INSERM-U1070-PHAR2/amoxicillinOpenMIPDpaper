apply_weights_to_test_data <- function(test, pca_result, latent_data, optimal_components) {
  # Principal component analysis (95 % of variance explained)
  pca_result <- prcomp(combined_dataset, scale. = TRUE) # Scaling of the data
  explained_var <- summary(pca_result)$importance[2,]
  optimal_components <- which(cumsum(explained_var) >= 0.95)[1] # Choosing components that explain 95 % of the variance
  
  # Calculate the latent space for each dataset using PCA
  latent_CARLIER <- predict(pca_result, newdata = latent_data$CARLIER)[, 1:optimal_components]
  latent_FOURNIER <- predict(pca_result, newdata = latent_data$FOURNIER)[, 1:optimal_components]
  latent_RAMBAUD <- predict(pca_result, newdata = latent_data$RAMBAUD)[, 1:optimal_components]
  
  # Combine all latent space data
  combined_latent_data <- rbind(latent_CARLIER, latent_FOURNIER, latent_RAMBAUD)
  
  # Calculate centroids
  centroid_CARLIER <- colMeans(latent_CARLIER)
  centroid_FOURNIER <- colMeans(latent_FOURNIER)
  centroid_RAMBAUD <- colMeans(latent_RAMBAUD)
  
  # Regularization element to avoid singular (so non-invertable) matrice
  cov_matrix_Carlier <- cov(latent_CARLIER)
  regularized_cov_matrix_Carlier <- cov_matrix_Carlier + diag(rep(1e-6, ncol(cov_matrix_Carlier)))
  
  cov_matrix_Fournier <- cov(latent_FOURNIER)
  regularized_cov_matrix_Fournier <- cov_matrix_Fournier + diag(rep(1e-6, ncol(cov_matrix_Fournier)))
  
  cov_matrix_Rambaud <- cov(latent_RAMBAUD)
  regularized_cov_matrix_Rambaud <- cov_matrix_Rambaud + diag(rep(1e-6, ncol(cov_matrix_Rambaud)))
  
  weights_list <- list()
  
  # Iterate through each row of the test dataset (each row is a different subject)
  for (i in 1:nrow(test)) {
    # Create new patient data
    new_patient <- as.data.frame(test[i, ])
    
    # Transform the new patient using PCA
    new_patient_pca <- predict(pca_result, newdata = new_patient)[, 1:optimal_components]
    new_patient_pca <- as.data.frame(t(new_patient_pca))  # Transpose to match dimensions
    
    # Calculate Mahalanobis distances
    mahalanobis_distances <- c(
      CARLIER = mahalanobis(new_patient_pca, centroid_CARLIER, regularized_cov_matrix_Carlier),
      FOURNIER = mahalanobis(new_patient_pca, centroid_FOURNIER, regularized_cov_matrix_Fournier),
      RAMBAUD = mahalanobis(new_patient_pca, centroid_RAMBAUD, regularized_cov_matrix_Rambaud)
    )
    
    # Calculate weights by taking the reciprocal Mahalanobis distances
    weights <- 1 / mahalanobis_distances
    weights <- weights / sum(weights)  # Normalize the weights
    
    # Add the weights as columns to the test dataset
    weights_list[[i]] <- weights
    
  }
  
  # Convert the list of weights into a data frame
  weights_df <- do.call(rbind, weights_list)
  
  # Add the weight columns to the original test dataset
  test_weights <- cbind(test, weights_df)
  colnames(test_weights)[(ncol(test) + 1):ncol(test_weights)] <- c("CARLIER", "FOURNIER", "RAMBAUD")
  
  return(test_weights)
}