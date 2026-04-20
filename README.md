# maternal.immune

Repository for APP/GAIN Maternal Immune Activation

## Overview

`maternal.immune` is an R package for conducting Partial Least Squares Discriminant Analysis (PLS-DA) to analyze cytokine predictors and maternal immune status. The package provides tools for:

- Running PLS-DA analysis with cytokines as predictors
- Calculating Variable Importance Projection (VIP) scores
- Visualizing important cytokine biomarkers
- Data validation and preprocessing

## Installation

```r
# Install from GitHub (requires devtools)
# devtools::install_github("mponzini/maternal.immune")

# Or install locally
# install.packages("path/to/maternal.immune", repos = NULL, type = "source")
```

## Dependencies

The package requires the following R packages:
- `mixOmics` - for PLS-DA analysis
- `ggplot2` - for visualization
- `dplyr` - for data manipulation

## Quick Start

### Basic PLS-DA Analysis

```r
library(maternal.immune)

# Example with simulated data
set.seed(123)

# Create example cytokine data (100 samples, 10 cytokines)
cytokines <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
colnames(cytokines) <- paste0("Cytokine_", 1:10)

# Create maternal immune status (binary outcome)
immune_status <- factor(sample(c("yes", "no"), 100, replace = TRUE))

# Run PLS-DA analysis
result <- run_plsda(
  X = cytokines,
  Y = immune_status,
  ncomp = 2,
  scale = TRUE
)

# View results
print(result)

# Access VIP scores
head(result$vip_scores)
```

### Data Preparation

```r
# Prepare data with validation and preprocessing
prepared <- prepare_plsda_data(
  X = cytokines,
  Y = immune_status,
  remove_zero_var = TRUE,
  na_action = "omit",
  log_transform = FALSE,
  verbose = TRUE
)

# Run PLS-DA on prepared data
result <- run_plsda(prepared$X, prepared$Y, ncomp = 2)
```

### Visualize VIP Scores

```r
# Plot VIP scores for top 15 cytokines
plot_vip(result$vip_scores, top_n = 15, threshold = 1.0)

# Save plot
ggsave("vip_scores.png", width = 8, height = 6)
```

## Key Functions

### `run_plsda()`

Performs PLS-DA analysis using cytokine data as predictors and maternal immune status as the outcome.

**Parameters:**
- `X`: Matrix or data frame of cytokine measurements (predictors)
- `Y`: Factor or character vector of maternal immune status (outcome)
- `ncomp`: Number of PLS components (default: 2)
- `scale`: Whether to scale predictors (default: TRUE)
- `center`: Whether to center predictors (default: TRUE)

**Returns:**
- `model`: Fitted PLS-DA model from mixOmics
- `vip_scores`: Data frame of VIP scores for each cytokine
- `X`: Processed predictor matrix
- `Y`: Processed outcome vector

### `calculate_vip()`

Calculates Variable Importance Projection (VIP) scores from a fitted PLS-DA model.

**Parameters:**
- `plsda_model`: A fitted PLS-DA model from mixOmics::plsda

**Returns:**
- Data frame with variable names and VIP scores, sorted by importance

**Interpretation:**
- VIP > 1.0: Variable is important for the model
- VIP < 1.0: Variable has below-average importance

### `plot_vip()`

Creates a bar plot of VIP scores.

**Parameters:**
- `vip_df`: Data frame of VIP scores (from calculate_vip())
- `top_n`: Number of top variables to display (default: 20)
- `threshold`: Threshold line to draw (default: 1.0)
- `title`: Plot title

### `prepare_plsda_data()`

Prepares and validates data for PLS-DA analysis.

**Parameters:**
- `X`: Matrix or data frame of predictors
- `Y`: Factor or character vector of outcomes
- `remove_zero_var`: Remove zero-variance predictors (default: TRUE)
- `na_action`: How to handle missing values: "fail", "omit", or "impute_mean"
- `log_transform`: Apply log transformation (default: FALSE)
- `verbose`: Print information (default: TRUE)

### `validate_plsda_data()`

Validates input data meets PLS-DA requirements.

**Parameters:**
- `X`: Matrix or data frame of predictors
- `Y`: Factor or character vector of outcomes
- `min_samples_per_class`: Minimum samples per class (default: 2)

## Understanding VIP Scores

Variable Importance Projection (VIP) scores quantify the contribution of each cytokine to the PLS-DA model. The VIP score is calculated as:

$$VIP_j = \sqrt{p \times \frac{\sum_{h=1}^{H} (w_{jh}^2 \times SSY_h)}{\sum_{h=1}^{H} SSY_h}}$$

Where:
- $p$ = number of variables
- $H$ = number of PLS components
- $w_{jh}$ = weight of variable $j$ in component $h$
- $SSY_h$ = sum of squares explained by component $h$

**Guidelines:**
- VIP > 1.0: Variable is considered important
- Higher VIP scores indicate stronger discriminatory power
- VIP scores help identify the most relevant cytokine biomarkers

## Workflow Example

```r
library(maternal.immune)

# 1. Load your data
# cytokine_data <- read.csv("cytokines.csv")
# immune_status <- read.csv("immune_status.csv")$status

# 2. Validate data
validate_plsda_data(cytokine_data, immune_status)

# 3. Prepare data
prepared <- prepare_plsda_data(
  X = cytokine_data,
  Y = immune_status,
  remove_zero_var = TRUE,
  na_action = "omit"
)

# 4. Run PLS-DA
result <- run_plsda(
  X = prepared$X,
  Y = prepared$Y,
  ncomp = 2
)

# 5. Examine results
print(result)

# 6. Identify important cytokines (VIP > 1)
important_cytokines <- result$vip_scores[result$vip_scores$vip_score > 1, ]
print(important_cytokines)

# 7. Visualize VIP scores
vip_plot <- plot_vip(result$vip_scores, top_n = 20)
print(vip_plot)

# 8. Access the underlying mixOmics model for further analysis
# e.g., cross-validation, performance assessment
mixomics_model <- result$model
```

## Additional mixOmics Functionality

The package uses the `mixOmics` package for PLS-DA fitting. You can access the underlying model object for advanced analyses:

```r
# Perform cross-validation
cv_result <- mixOmics::perf(
  result$model,
  validation = "Mfold",
  folds = 5,
  nrepeat = 10
)

# Plot performance
plot(cv_result)

# Extract loadings
loadings <- result$model$loadings$X

# Create score plot
mixOmics::plotIndiv(result$model, comp = c(1, 2), group = result$Y)

# Create loading plot
mixOmics::plotLoadings(result$model, comp = 1)
```

## License

MIT License - see LICENSE file for details

## Author

Copyright 2025 mponzini
