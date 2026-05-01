library(tidyverse)
library(modeldata)

set.seed(42)

# Load data
data(ames, package = "modeldata")

# Setup
predictor_cols <- setdiff(names(ames), "Sale_Price")
n_folds <- 5
n_samples <- 20
n_min_values <- c(1, 2, 3, 4, 5, 6, 8, 10, 15, 20, 30, 50, 100)
fold_indices <- sample(rep(1:n_folds, length.out = nrow(ames)))
results <- data.frame()

# Loop over min bucket sizes
for (i in seq_along(n_min_values)) {
  n_min <- n_min_values[i]
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
    pred_matrix <- matrix(NA, nrow = n_samples, ncol = nrow(test_data))
    
    # Bootstrap samples
    for (s in 1:n_samples) {
      sample_idx <- sample(1:nrow(train_data), sample_size, replace = FALSE)
      sample_data <- train_data[sample_idx, ]
      
      # Fit tree with full depth
      tree <- rpart(Sale_Price ~ ., 
                    data = sample_data[, c(predictor_cols, "Sale_Price")],
                    control = rpart.control(minbucket = n_min, minsplit = 2, cp = 0))
      
      pred_matrix[s, ] <- predict(tree, test_data[, predictor_cols])
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
    minbucket = n_min,
    Bias2_Noise = mean(fold_bias2_noise),
    Variance = mean(fold_variance)
  ))
}

# Calculate MSE
results <- results %>%
  mutate(MSE = Bias2_Noise + Variance)

# Print results
print(results %>%
        select(minbucket, Bias2_Noise, Variance, MSE) %>%
        mutate(across(where(is.numeric), ~round(., 0))))
