# How Resampling Methods Improve the Stability of House-Price Prediction

## Overview

This repository analyses the Ames Housing dataset as well as the relative performance of various models to understand why resampling methods reduce instability in house price prediction.

## Scripts

### Main Analysis
- **10FoldCvWilcoxon.R** - 10-fold CV comparing three models with Wilcoxon signed-rank tests

### Bias-Variance Decomposition
- **RegressionTreeBVD.R** / **RegressionTreePlotBVD.R** - Tree bias-variance by minbucket
- **BaggingBVD.R** / **BaggingPlotBVD.R** - Bagging bias-variance by number of trees
- **RandomForestBVD.R** / **RandomForestPlotBVD.R** - RF bias-variance by mtry

### Feature Importance
- **RegressionTreeImportance.R** - Tree impurity reduction importance
- **BaggingImportance.R** / **RandomForestImportance.R** - Permutation importance

### Diagnostics
- **AmesHistogram.R** - Sale price distribution
- **ResidualAnalysis.R** - Linear model residual diagnostics (Residuals vs Fitted, Q-Q)

## Requirements
```r
install.packages(c("rpart", "randomForest", "ggplot2", "dplyr", "tidyr", "modeldata"))
