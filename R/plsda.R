#' Run PLS-DA Analysis
#'
#' Performs Partial Least Squares Discriminant Analysis (PLS-DA) using cytokine
#' data as predictors and maternal immune status as the outcome variable.
#'
#' @param X A numeric matrix or data frame of cytokine measurements (predictors).
#'   Rows represent samples and columns represent cytokines.
#' @param Y A factor or character vector indicating maternal immune status
#'   (e.g., "yes" or "no"). Must have the same length as the number of rows in X.
#' @param ncomp Integer. The number of components to include in the model.
#'   Default is 2.
#' @param scale Logical. Should the data be scaled? Default is TRUE.
#' @param center Logical. Should the data be centered? Default is TRUE.
#' @param ... Additional arguments passed to \code{mixOmics::plsda}.
#'
#' @return A list containing:
#'   \item{model}{The fitted PLS-DA model from mixOmics}
#'   \item{vip_scores}{A data frame of Variable Importance Projection scores}
#'   \item{X}{The processed predictor matrix}
#'   \item{Y}{The processed outcome vector}
#'
#' @examples
#' \dontrun{
#' # Example with simulated data
#' set.seed(123)
#' cytokines <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' colnames(cytokines) <- paste0("Cytokine_", 1:10)
#' immune_status <- factor(sample(c("yes", "no"), 100, replace = TRUE))
#'
#' result <- run_plsda(cytokines, immune_status, ncomp = 2)
#' print(result$vip_scores)
#' }
#'
#' @export
run_plsda <- function(X, Y, ncomp = 2, scale = TRUE, center = TRUE, ...) {
  # Validate inputs
  if (!is.matrix(X) && !is.data.frame(X)) {
    stop("X must be a matrix or data frame")
  }

  if (nrow(X) != length(Y)) {
    stop("Number of rows in X must match length of Y")
  }

  if (ncomp < 1) {
    stop("ncomp must be at least 1")
  }

  # Convert X to matrix if it's a data frame
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  # Ensure Y is a factor
  if (!is.factor(Y)) {
    Y <- as.factor(Y)
  }

  # Check for missing values
  if (any(is.na(X))) {
    stop("X contains missing values. Please impute or remove them.")
  }

  if (any(is.na(Y))) {
    stop("Y contains missing values. Please remove them.")
  }

  # Fit PLS-DA model using mixOmics
  plsda_model <- mixOmics::plsda(
    X = X,
    Y = Y,
    ncomp = ncomp,
    scale = scale,
    ...
  )

  # Calculate VIP scores
  vip_scores <- calculate_vip(plsda_model)

  # Return results
  result <- list(
    model = plsda_model,
    vip_scores = vip_scores,
    X = X,
    Y = Y
  )

  class(result) <- c("plsda_result", "list")
  return(result)
}


#' Print method for plsda_result
#'
#' @param x A plsda_result object
#' @param ... Additional arguments (not used)
#' @export
print.plsda_result <- function(x, ...) {
  cat("PLS-DA Analysis Results\n")
  cat("=======================\n\n")
  cat("Number of samples:", nrow(x$X), "\n")
  cat("Number of predictors:", ncol(x$X), "\n")
  cat("Number of components:", x$model$ncomp, "\n")
  cat("Outcome classes:", paste(levels(x$Y), collapse = ", "), "\n\n")
  cat("Top 10 VIP Scores:\n")
  print(head(x$vip_scores, 10))
}
