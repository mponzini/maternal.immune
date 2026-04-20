#' Validate PLS-DA Input Data
#'
#' Validates that input data meets requirements for PLS-DA analysis.
#'
#' @param X A matrix or data frame of predictors
#' @param Y A factor or character vector of outcomes
#' @param min_samples_per_class Integer. Minimum number of samples required per class.
#'   Default is 2.
#'
#' @return TRUE if validation passes, otherwise throws an error
#'
#' @examples
#' \dontrun{
#' X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
#' Y <- factor(rep(c("yes", "no"), each = 25))
#' validate_plsda_data(X, Y)
#' }
#'
#' @export
validate_plsda_data <- function(X, Y, min_samples_per_class = 2) {
  # Check X is numeric
  if (!is.matrix(X) && !is.data.frame(X)) {
    stop("X must be a matrix or data frame")
  }

  # Check that all columns in X are numeric
  if (is.data.frame(X)) {
    if (!all(sapply(X, is.numeric))) {
      stop("All columns in X must be numeric")
    }
  }

  # Check dimensions match
  if (nrow(X) != length(Y)) {
    stop(sprintf(
      "Number of rows in X (%d) does not match length of Y (%d)",
      nrow(X), length(Y)
    ))
  }

  # Check for missing values
  if (any(is.na(X))) {
    n_missing <- sum(is.na(X))
    warning(sprintf("X contains %d missing values", n_missing))
  }

  if (any(is.na(Y))) {
    stop("Y contains missing values. Please remove or impute them.")
  }

  # Check Y is categorical
  if (!is.factor(Y) && !is.character(Y)) {
    stop("Y must be a factor or character vector")
  }

  # Convert to factor if needed
  if (is.character(Y)) {
    Y <- as.factor(Y)
  }

  # Check class balance
  class_counts <- table(Y)
  if (any(class_counts < min_samples_per_class)) {
    stop(sprintf(
      "All classes must have at least %d samples. Class counts: %s",
      min_samples_per_class,
      paste(names(class_counts), class_counts, sep = "=", collapse = ", ")
    ))
  }

  # Check for zero variance predictors
  if (is.matrix(X)) {
    var_X <- apply(X, 2, stats::var, na.rm = TRUE)
  } else {
    var_X <- sapply(X, stats::var, na.rm = TRUE)
  }

  zero_var_cols <- which(var_X == 0 | is.na(var_X))
  if (length(zero_var_cols) > 0) {
    warning(sprintf(
      "Found %d predictors with zero variance: %s",
      length(zero_var_cols),
      paste(colnames(X)[zero_var_cols], collapse = ", ")
    ))
  }

  return(TRUE)
}


#' Prepare Data for PLS-DA
#'
#' Prepares cytokine data and outcome variable for PLS-DA analysis.
#' Includes options for handling missing data and removing low-variance predictors.
#'
#' @param X A matrix or data frame of cytokine measurements
#' @param Y A factor or character vector of maternal immune status
#' @param remove_zero_var Logical. Should zero-variance predictors be removed?
#'   Default is TRUE.
#' @param na_action Character. How to handle missing values: "fail" (default),
#'   "omit" (remove samples with any NA), or "impute_mean" (impute with column means).
#' @param log_transform Logical. Should predictors be log-transformed? Default is FALSE.
#' @param verbose Logical. Print information about data preparation? Default is TRUE.
#'
#' @return A list containing:
#'   \item{X}{Processed predictor matrix}
#'   \item{Y}{Processed outcome vector}
#'   \item{removed_cols}{Names of removed columns (if any)}
#'   \item{removed_rows}{Row indices of removed samples (if any)}
#'
#' @examples
#' \dontrun{
#' X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
#' colnames(X) <- paste0("Cytokine_", 1:10)
#' Y <- factor(rep(c("yes", "no"), each = 25))
#' prepared <- prepare_plsda_data(X, Y)
#' }
#'
#' @export
prepare_plsda_data <- function(X, Y, remove_zero_var = TRUE,
                               na_action = c("fail", "omit", "impute_mean"),
                               log_transform = FALSE,
                               verbose = TRUE) {
  na_action <- match.arg(na_action)

  # Convert to matrix if data frame
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  # Store original dimensions
  orig_nrow <- nrow(X)
  orig_ncol <- ncol(X)

  # Store column names
  col_names <- colnames(X)
  if (is.null(col_names)) {
    col_names <- paste0("Var_", seq_len(ncol(X)))
    colnames(X) <- col_names
  }

  removed_cols <- character(0)
  removed_rows <- integer(0)

  # Handle missing values
  if (any(is.na(X))) {
    if (na_action == "fail") {
      stop("X contains missing values. Use na_action = 'omit' or 'impute_mean'")
    } else if (na_action == "omit") {
      complete_rows <- stats::complete.cases(X)
      removed_rows <- which(!complete_rows)
      X <- X[complete_rows, , drop = FALSE]
      Y <- Y[complete_rows]
      if (verbose) {
        message(sprintf("Removed %d samples with missing values", length(removed_rows)))
      }
    } else if (na_action == "impute_mean") {
      col_means <- colMeans(X, na.rm = TRUE)
      for (j in seq_len(ncol(X))) {
        na_idx <- is.na(X[, j])
        if (any(na_idx)) {
          X[na_idx, j] <- col_means[j]
        }
      }
      if (verbose) {
        message("Imputed missing values with column means")
      }
    }
  }

  # Log transform if requested
  if (log_transform) {
    # Add small constant to avoid log(0)
    if (any(X <= 0)) {
      min_positive <- min(X[X > 0], na.rm = TRUE)
      X <- log(X + min_positive / 2)
      if (verbose) {
        message("Applied log transformation with constant shift")
      }
    } else {
      X <- log(X)
      if (verbose) {
        message("Applied log transformation")
      }
    }
  }

  # Remove zero variance predictors
  if (remove_zero_var) {
    var_X <- apply(X, 2, stats::var, na.rm = TRUE)
    zero_var_idx <- which(var_X == 0 | is.na(var_X))
    if (length(zero_var_idx) > 0) {
      removed_cols <- colnames(X)[zero_var_idx]
      X <- X[, -zero_var_idx, drop = FALSE]
      if (verbose) {
        message(sprintf("Removed %d zero-variance predictors", length(removed_cols)))
      }
    }
  }

  # Ensure Y is a factor
  if (!is.factor(Y)) {
    Y <- as.factor(Y)
  }

  # Print summary
  if (verbose) {
    message(sprintf(
      "Prepared data: %d samples, %d predictors (original: %d samples, %d predictors)",
      nrow(X), ncol(X), orig_nrow, orig_ncol
    ))
    message(sprintf("Outcome classes: %s",
                   paste(names(table(Y)), "=", table(Y), collapse = ", ")))
  }

  return(list(
    X = X,
    Y = Y,
    removed_cols = removed_cols,
    removed_rows = removed_rows
  ))
}
