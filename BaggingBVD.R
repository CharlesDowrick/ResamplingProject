library(randomForest)
library(dplyr)
library(modeldata)

set.seed(42)

# Load Ames housing data
data(ames, package = "modeldata")

# Setup predictors
predictor_cols <- setdiff(names(ames), "Sale_Price")
n_predictors <- length(predictor_cols)

# Cross-validation parameters
n_folds <- 5       
n_samples <- 20    
n_trees_values <- c(2, 4, 6, 8, 10, 15, 20, 25, 30, 35, 40, 50, 75, 100, 250, 500)

# Create fold assignments
fold_indices <- sample(rep(1:n_folds, length.out = nrow(ames)))

results <- data.frame()

# Loop over number of trees
for (i in seq_along(n_trees_values)) {
  n_trees <- n_trees_values[i]
  fold_bias2_noise <- numeric(n_folds)
  fold_variance <- numeric(n_folds)
  
  # 5-fold cross-validation
  for (fold in 1:n_folds) {
    
    # Split data
    test_idx <- which(fold_indices == fold)
    train_idx <- which(fold_indices != fold)
    
    train_data <- ames[train_idx, ]
    test_data <- ames[test_idx, ]
    
    sample_size <- floor(nrow(train_data) / 2)
    
    # Store predictions
    pred_matrix <- matrix(NA, nrow = n_samples, ncol = nrow(test_data))
    
    # Bootstrap samples
    for (s in 1:n_samples) {
      
      sample_idx <- sample(1:nrow(train_data), sample_size, replace = FALSE)
      sample_data <- train_data[sample_idx, ]
      
      # Bagging with mtry = p (all features) and nodesize = 1 (full depth)
      rf <- randomForest(
        Sale_Price ~ ., 
        data = sample_data[, c(predictor_cols, "Sale_Price")],
        ntree = n_trees,
        mtry = n_predictors,      # All features = Bagging
        nodesize = 1,              # Full depth
        replace = TRUE
      )
      
      pred_matrix[s, ] <- predict(rf, test_data[, predictor_cols])
    }
    
    # Calculate bias^2+noise and variance
    obs <- test_data$Sale_Price
    avg_pred <- colMeans(pred_matrix)
    
    bias2_noise <- mean((obs - avg_pred)^2)
    variance <- mean(apply(pred_matrix, 2, var))
    
    fold_bias2_noise[fold] <- bias2_noise
    fold_variance[fold] <- variance
  }
  
  # Average across folds
  results <- rbind(results, data.frame(
    n_trees = n_trees,
    Bias2_Noise = mean(fold_bias2_noise),
    Variance = mean(fold_variance)
  ))
}

# Calculate MSE
results <- results %>%
  mutate(MSE = Bias2_Noise + Variance)

# Print results table
print(results %>%
        select(n_trees, Bias2_Noise, Variance, MSE) %>%
        mutate(across(where(is.numeric), ~round(., 0))))