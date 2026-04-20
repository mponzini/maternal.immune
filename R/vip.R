#' Calculate Variable Importance Projection (VIP) Scores
#'
#' Calculates VIP scores for a PLS-DA model. VIP scores measure the importance
#' of each variable in the projection used in a PLS model.
#'
#' @param plsda_model A fitted PLS-DA model from mixOmics::plsda
#'
#' @return A data frame with columns:
#'   \item{variable}{Name of the predictor variable}
#'   \item{vip_score}{VIP score for the variable}
#'   \item{component}{For component-specific VIP scores (if multiple components)}
#'
#' @details
#' VIP scores are calculated as:
#' \deqn{VIP_j = \sqrt{p \times \sum_{h=1}^{H} (w_{jh}^2 \times SSY_h) / \sum_{h=1}^{H} SSY_h}}
#'
#' where:
#' \itemize{
#'   \item \eqn{p} is the number of variables
#'   \item \eqn{H} is the number of components
#'   \item \eqn{w_{jh}} is the weight of variable j in component h
#'   \item \eqn{SSY_h} is the sum of squares explained by component h
#' }
#'
#' Variables with VIP > 1 are typically considered important.
#'
#' @examples
#' \dontrun{
#' # Example usage after fitting a PLS-DA model
#' set.seed(123)
#' X <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' colnames(X) <- paste0("Cytokine_", 1:10)
#' Y <- factor(sample(c("yes", "no"), 100, replace = TRUE))
#' model <- mixOmics::plsda(X, Y, ncomp = 2)
#' vip <- calculate_vip(model)
#' }
#'
#' @export
calculate_vip <- function(plsda_model) {
  # Extract necessary components from the model
  W <- plsda_model$loadings$X  # Loadings (weights)
  ncomp <- plsda_model$ncomp
  p <- nrow(W)  # Number of variables

  # Get variable names
  var_names <- rownames(W)
  if (is.null(var_names)) {
    var_names <- paste0("Var_", seq_len(p))
  }

  # Calculate sum of squares explained by each component
  # Using the proportion of variance explained
  t_scores <- plsda_model$variates$X  # X scores (T matrix)

  # Calculate SSY for each component
  ssy <- apply(t_scores, 2, function(t) sum(t^2))

  # Calculate VIP scores
  vip_scores <- numeric(p)
  for (j in seq_len(p)) {
    # Sum across all components
    numerator <- sum(W[j, ]^2 * ssy)
    denominator <- sum(ssy)
    vip_scores[j] <- sqrt(p * numerator / denominator)
  }

  # Create data frame with results
  vip_df <- data.frame(
    variable = var_names,
    vip_score = vip_scores,
    stringsAsFactors = FALSE
  )

  # Sort by VIP score (descending)
  vip_df <- vip_df[order(-vip_df$vip_score), ]
  rownames(vip_df) <- NULL

  return(vip_df)
}


#' Plot VIP Scores
#'
#' Creates a bar plot of Variable Importance Projection scores.
#'
#' @param vip_df A data frame containing VIP scores (output from calculate_vip)
#' @param top_n Integer. Number of top variables to display. Default is 20.
#' @param threshold Numeric. Threshold line to draw on the plot (typically 1.0).
#'   Default is 1.0.
#' @param title Character. Plot title. Default is "Variable Importance Projection (VIP) Scores".
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' result <- run_plsda(cytokines, immune_status, ncomp = 2)
#' plot_vip(result$vip_scores, top_n = 15)
#' }
#'
#' @export
plot_vip <- function(vip_df, top_n = 20, threshold = 1.0,
                     title = "Variable Importance Projection (VIP) Scores") {
  # Select top N variables
  top_vip <- head(vip_df, top_n)

  # Create the plot
  p <- ggplot2::ggplot(top_vip, ggplot2::aes(x = stats::reorder(variable, vip_score),
                                              y = vip_score)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed",
                       color = "red", linewidth = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Variable",
      y = "VIP Score",
      title = title,
      subtitle = paste0("Top ", nrow(top_vip), " variables (threshold = ", threshold, ")")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12)
    )

  return(p)
}
