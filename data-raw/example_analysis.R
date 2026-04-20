#!/usr/bin/env Rscript
#
# Example script demonstrating PLS-DA analysis with maternal.immune package
#
# This script shows a complete workflow for:
# 1. Loading/simulating cytokine data
# 2. Preparing data for analysis
# 3. Running PLS-DA
# 4. Calculating VIP scores
# 5. Visualizing results

# Load the package
library(maternal.immune)

# Set random seed for reproducibility
set.seed(123)

cat("========================================\n")
cat("PLS-DA Analysis Example\n")
cat("========================================\n\n")

# 1. Create example data
cat("1. Creating example cytokine data...\n")
n_samples <- 100
n_cytokines <- 12

# Simulate cytokine measurements
cytokines <- matrix(rnorm(n_samples * n_cytokines),
                    nrow = n_samples,
                    ncol = n_cytokines)

# Add group differences to make some cytokines discriminatory
immune_status <- factor(rep(c("yes", "no"), each = n_samples / 2))

# Cytokines 1-4: higher in "yes" group
for (i in 1:4) {
  cytokines[immune_status == "yes", i] <-
    cytokines[immune_status == "yes", i] + rnorm(n_samples/2, mean = 2, sd = 0.5)
}

# Cytokines 5-6: lower in "yes" group
for (i in 5:6) {
  cytokines[immune_status == "yes", i] <-
    cytokines[immune_status == "yes", i] - rnorm(n_samples/2, mean = 1.5, sd = 0.5)
}

# Name the cytokines
colnames(cytokines) <- c(
  "IL-1beta", "IL-6", "IL-8", "TNF-alpha",
  "IL-10", "IL-4",
  "IFN-gamma", "IL-12", "IL-17", "IL-2", "IL-5", "IL-13"
)

cat("  - Created", nrow(cytokines), "samples with", ncol(cytokines), "cytokines\n")
cat("  - Outcome classes:", paste(names(table(immune_status)), "=",
                                   table(immune_status), collapse=", "), "\n\n")

# 2. Validate data
cat("2. Validating data...\n")
validate_plsda_data(cytokines, immune_status)
cat("  - Data validation passed\n\n")

# 3. Prepare data
cat("3. Preparing data for analysis...\n")
prepared <- prepare_plsda_data(
  X = cytokines,
  Y = immune_status,
  remove_zero_var = TRUE,
  na_action = "fail",
  log_transform = FALSE,
  verbose = FALSE
)
cat("  - Data preparation complete\n\n")

# 4. Run PLS-DA
cat("4. Running PLS-DA analysis...\n")
result <- run_plsda(
  X = prepared$X,
  Y = prepared$Y,
  ncomp = 2,
  scale = TRUE
)
cat("  - PLS-DA model fitted successfully\n\n")

# 5. Display results
cat("5. Results Summary\n")
cat("==================\n\n")
print(result)
cat("\n")

# 6. Identify important cytokines
cat("6. Important Cytokines (VIP > 1.0)\n")
cat("===================================\n")
important <- result$vip_scores[result$vip_scores$vip_score > 1.0, ]
if (nrow(important) > 0) {
  print(important)
  cat("\nFound", nrow(important), "important cytokine(s)\n\n")
} else {
  cat("No cytokines with VIP > 1.0 found\n\n")
}

# 7. Create visualization
cat("7. Creating VIP score plot...\n")
vip_plot <- plot_vip(
  result$vip_scores,
  top_n = 10,
  threshold = 1.0,
  title = "Top 10 Cytokines by VIP Score"
)

# Save plot if ggplot2 is available
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggsave(
    "vip_scores_example.png",
    plot = vip_plot,
    width = 8,
    height = 6,
    dpi = 300
  )
  cat("  - Plot saved to: vip_scores_example.png\n\n")
} else {
  cat("  - ggplot2 not available, skipping plot save\n\n")
}

cat("========================================\n")
cat("Analysis Complete!\n")
cat("========================================\n")
