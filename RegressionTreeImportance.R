library(rpart)
library(ggplot2)
library(dplyr)
library(modeldata)

set.seed(42)

# Load Ames housing data
data(ames, package = "modeldata")

# Identify predictors
predictor_cols <- setdiff(names(ames), "Sale_Price")

# Fit a single regression tree with default settings
tree_default <- rpart(Sale_Price ~ ., 
                      data = ames[, c(predictor_cols, "Sale_Price")])

# Extract variable importance (impurity reduction)
importance_default <- tree_default$variable.importance

# Convert to data frame and sort by importance
importance_df <- data.frame(
  Feature = names(importance_default),
  Importance = importance_default
) %>% arrange(desc(Importance))

# Convert importance to percentage of total
total_imp <- sum(importance_df$Importance)
importance_df <- importance_df %>%
  mutate(Percentage_of_Total = (Importance / total_imp) * 100)

# Create top 15 features
tree_top15 <- importance_df %>% head(15)

# Print all features with importance > 0
print(importance_df)

# Create bar plot (red) showing percentage of total importance for top 15
p_tree <- ggplot(tree_top15, aes(x = reorder(Feature, Percentage_of_Total), y = Percentage_of_Total)) +
  geom_bar(stat = "identity", fill = "#d62728", alpha = 0.8) +
  coord_flip() +
  labs(title = "Regression Tree: Top 15 Features by Impurity Reduction", 
       x = "Feature", 
       y = "Percentage of Total Importance (%)") +
  theme_minimal()

# Display plot
print(p_tree)
