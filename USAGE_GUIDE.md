# PLS-DA Analysis Guide for Researchers

## Overview

This guide explains how to use the `maternal.immune` package to analyze cytokine data and identify biomarkers associated with maternal immune status using Partial Least Squares Discriminant Analysis (PLS-DA).

## What is PLS-DA?

PLS-DA (Partial Least Squares Discriminant Analysis) is a supervised machine learning method that:
- Identifies patterns in high-dimensional data (like cytokine measurements)
- Finds combinations of variables that best discriminate between groups
- Ranks variables by their importance (using VIP scores)

## What are VIP Scores?

Variable Importance Projection (VIP) scores quantify how much each cytokine contributes to the model:
- **VIP > 1.0**: Important cytokine for group discrimination
- **VIP < 1.0**: Less important cytokine
- Higher VIP = stronger discriminatory power

## Quick Start

### 1. Install the Package

```r
# If the package is on GitHub
devtools::install_github("mponzini/maternal.immune")

# Or install from local source
install.packages("path/to/maternal.immune", repos = NULL, type = "source")
```

### 2. Load Your Data

Your data should have:
- **Cytokine measurements**: Numeric matrix with samples as rows, cytokines as columns
- **Immune status**: Factor/character vector indicating group membership (e.g., "yes"/"no")

```r
library(maternal.immune)

# Example: Load from CSV files
cytokine_data <- read.csv("cytokines.csv", row.names = 1)
immune_status <- read.csv("immune_status.csv")$status
```

### 3. Run the Analysis

```r
# Validate your data
validate_plsda_data(cytokine_data, immune_status)

# Prepare the data (handles preprocessing)
prepared <- prepare_plsda_data(
  X = cytokine_data,
  Y = immune_status,
  remove_zero_var = TRUE,
  na_action = "omit"  # or "impute_mean" or "fail"
)

# Run PLS-DA
result <- run_plsda(
  X = prepared$X,
  Y = prepared$Y,
  ncomp = 2  # Number of components
)

# View results
print(result)
```

### 4. Examine Important Cytokines

```r
# See all VIP scores
print(result$vip_scores)

# Filter for important cytokines (VIP > 1)
important <- result$vip_scores[result$vip_scores$vip_score > 1, ]
print(important)

# Visualize top cytokines
plot_vip(result$vip_scores, top_n = 15)

# Save the plot
ggsave("vip_scores.png", width = 8, height = 6)
```

## Detailed Workflow

### Step 1: Data Preparation

#### Data Format Requirements

Your cytokine data should look like this:

```
              IL-1beta  IL-6  IL-8  TNF-alpha  ...
Sample_001      2.34   5.67  1.23    3.45     ...
Sample_002      1.98   4.32  2.11    2.87     ...
Sample_003      3.21   6.54  1.76    4.12     ...
...
```

Your immune status should be:
```
yes, yes, no, yes, no, no, yes, ...
```

#### Handling Missing Values

The package provides three options for missing data:

```r
# Option 1: Fail if any missing values (recommended for clean data)
prepared <- prepare_plsda_data(X, Y, na_action = "fail")

# Option 2: Remove samples with missing values
prepared <- prepare_plsda_data(X, Y, na_action = "omit")

# Option 3: Impute with column means
prepared <- prepare_plsda_data(X, Y, na_action = "impute_mean")
```

#### Optional Transformations

```r
# Log-transform cytokine values (useful for skewed distributions)
prepared <- prepare_plsda_data(
  X = cytokine_data,
  Y = immune_status,
  log_transform = TRUE
)
```

### Step 2: Choosing the Number of Components

Start with 2 components and use cross-validation to optimize:

```r
# Run with 2 components
result <- run_plsda(X, Y, ncomp = 2)

# Evaluate performance with cross-validation
cv_result <- mixOmics::perf(
  result$model,
  validation = "Mfold",
  folds = 5,
  nrepeat = 10
)

# View performance
plot(cv_result)

# Choose optimal number based on classification error rate
```

### Step 3: Interpreting Results

#### VIP Scores

```r
# View VIP scores sorted by importance
head(result$vip_scores, 20)

# Example output:
#     variable  vip_score
# 1   IL-1beta      2.34   <- Very important
# 2   IL-6          1.87   <- Important
# 3   TNF-alpha     1.45   <- Important
# 4   IL-8          0.92   <- Less important
# ...
```

**Interpretation:**
- Cytokines with VIP > 1.0 are the most important discriminators
- These are potential biomarkers for maternal immune status
- Focus biological interpretation on top-ranked cytokines

#### Score Plots

Visualize sample separation:

```r
mixOmics::plotIndiv(
  result$model,
  comp = c(1, 2),
  group = result$Y,
  legend = TRUE,
  title = "PLS-DA Score Plot"
)
```

Good separation between groups indicates the model successfully discriminates immune status.

#### Loading Plots

See which cytokines contribute to each component:

```r
mixOmics::plotLoadings(
  result$model,
  comp = 1,
  title = "Component 1 Loadings"
)
```

### Step 4: Exporting Results

```r
# Export VIP scores to CSV
write.csv(result$vip_scores, "vip_scores.csv", row.names = FALSE)

# Export important cytokines only
important <- result$vip_scores[result$vip_scores$vip_score > 1, ]
write.csv(important, "important_cytokines.csv", row.names = FALSE)

# Save the VIP plot
vip_plot <- plot_vip(result$vip_scores, top_n = 20)
ggsave("vip_plot.png", plot = vip_plot, width = 10, height = 7, dpi = 300)
```

## Common Issues and Solutions

### Issue: "All classes must have at least 2 samples"

**Solution:** Ensure each group (yes/no) has enough samples. PLS-DA requires at least 2 samples per class.

### Issue: "X contains missing values"

**Solution:** Use `na_action = "omit"` or `"impute_mean"` in `prepare_plsda_data()`.

### Issue: No cytokines with VIP > 1

**Possible causes:**
1. Groups are not well separated
2. Need more components (try `ncomp = 3` or higher)
3. Data might need transformation (try `log_transform = TRUE`)
4. Sample size might be too small

### Issue: Model performs poorly

**Solutions:**
1. Check data quality and preprocessing
2. Ensure groups are correctly labeled
3. Consider removing outliers
4. Try different number of components
5. Check if groups are truly different

## Tips for Best Results

1. **Sample Size:** Aim for at least 20 samples per group
2. **Quality Control:** Remove outliers and low-quality samples
3. **Scaling:** Keep `scale = TRUE` (default) for cytokines measured on different scales
4. **Cross-Validation:** Always validate your model with cross-validation
5. **Biological Interpretation:** Focus on top-ranked cytokines (VIP > 1.0)
6. **Replication:** Validate findings in independent cohorts

## Example Complete Analysis

```r
library(maternal.immune)

# 1. Load data
cytokines <- read.csv("data/cytokines.csv", row.names = 1)
status <- read.csv("data/immune_status.csv")$status

# 2. Check dimensions
cat("Samples:", nrow(cytokines), "\n")
cat("Cytokines:", ncol(cytokines), "\n")
cat("Groups:", table(status), "\n")

# 3. Validate
validate_plsda_data(cytokines, status)

# 4. Prepare
prepared <- prepare_plsda_data(
  X = cytokines,
  Y = status,
  remove_zero_var = TRUE,
  na_action = "omit",
  verbose = TRUE
)

# 5. Analyze
result <- run_plsda(prepared$X, prepared$Y, ncomp = 2)

# 6. Cross-validate
cv <- mixOmics::perf(result$model, validation = "Mfold",
                     folds = 5, nrepeat = 10)
plot(cv)

# 7. Identify biomarkers
important <- result$vip_scores[result$vip_scores$vip_score > 1, ]
print(important)

# 8. Visualize
plot_vip(result$vip_scores, top_n = 15)
ggsave("results/vip_scores.png", width = 8, height = 6)

mixOmics::plotIndiv(result$model, comp = c(1,2),
                    group = result$Y, legend = TRUE)

# 9. Export
write.csv(result$vip_scores, "results/all_vip_scores.csv",
          row.names = FALSE)
write.csv(important, "results/important_cytokines.csv",
          row.names = FALSE)
```

## Getting Help

- See `?run_plsda` for function documentation
- See `?calculate_vip` for VIP score details
- Read the getting started vignette: `vignette("getting-started")`
- For mixOmics details: http://mixomics.org/

## Citation

If you use this package in your research, please cite:
- The `maternal.immune` package (this repository)
- The mixOmics package: Rohart F, Gautier B, Singh A, Lê Cao K-A (2017).
  "mixOmics: An R package for 'omics feature selection and multiple data integration."
  PLoS Computational Biology 13(11): e1005752.
