test_that("validate_plsda_data accepts valid input", {
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  Y <- factor(rep(c("yes", "no"), each = 25))

  expect_true(validate_plsda_data(X, Y))
})


test_that("validate_plsda_data catches dimension mismatch", {
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  Y <- factor(rep(c("yes", "no"), each = 20))

  expect_error(validate_plsda_data(X, Y), "does not match")
})


test_that("validate_plsda_data catches insufficient samples per class", {
  X <- matrix(rnorm(10 * 5), nrow = 10, ncol = 5)
  Y <- factor(c(rep("yes", 9), "no"))

  expect_error(validate_plsda_data(X, Y, min_samples_per_class = 2),
               "must have at least")
})


test_that("validate_plsda_data warns about missing values in X", {
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  X[1, 1] <- NA
  Y <- factor(rep(c("yes", "no"), each = 25))

  expect_warning(validate_plsda_data(X, Y), "contains.*missing values")
})


test_that("validate_plsda_data catches missing values in Y", {
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  Y <- factor(rep(c("yes", "no"), each = 25))
  Y[1] <- NA

  expect_error(validate_plsda_data(X, Y), "Y contains missing values")
})


test_that("validate_plsda_data warns about zero variance predictors", {
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  X[, 1] <- 1  # Zero variance
  colnames(X) <- paste0("Var_", 1:5)
  Y <- factor(rep(c("yes", "no"), each = 25))

  expect_warning(validate_plsda_data(X, Y), "zero variance")
})


test_that("prepare_plsda_data removes zero variance predictors", {
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  X[, 1] <- 1  # Zero variance
  colnames(X) <- paste0("Var_", 1:5)
  Y <- factor(rep(c("yes", "no"), each = 25))

  result <- prepare_plsda_data(X, Y, remove_zero_var = TRUE, verbose = FALSE)

  expect_equal(ncol(result$X), 4)
  expect_equal(result$removed_cols, "Var_1")
})


test_that("prepare_plsda_data handles missing values with omit", {
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  X[1:3, 1] <- NA
  Y <- factor(rep(c("yes", "no"), each = 25))

  result <- prepare_plsda_data(X, Y, na_action = "omit", verbose = FALSE)

  expect_equal(nrow(result$X), 47)
  expect_equal(length(result$removed_rows), 3)
})


test_that("prepare_plsda_data handles missing values with impute", {
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  X[1, 1] <- NA
  Y <- factor(rep(c("yes", "no"), each = 25))

  result <- prepare_plsda_data(X, Y, na_action = "impute_mean", verbose = FALSE)

  expect_equal(nrow(result$X), 50)
  expect_false(any(is.na(result$X)))
})


test_that("prepare_plsda_data applies log transformation", {
  X <- matrix(abs(rnorm(50 * 5)) + 1, nrow = 50, ncol = 5)
  Y <- factor(rep(c("yes", "no"), each = 25))

  result <- prepare_plsda_data(X, Y, log_transform = TRUE, verbose = FALSE)

  # Check that values are transformed (all should be different)
  expect_false(identical(result$X, X))
  # Log of positive values should be smaller in most cases
  expect_true(mean(result$X) < mean(X))
})


test_that("prepare_plsda_data handles data frames", {
  X <- data.frame(matrix(rnorm(50 * 5), nrow = 50, ncol = 5))
  colnames(X) <- paste0("Var_", 1:5)
  Y <- factor(rep(c("yes", "no"), each = 25))

  result <- prepare_plsda_data(X, Y, verbose = FALSE)

  expect_true(is.matrix(result$X))
})


test_that("prepare_plsda_data ensures Y is factor", {
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  Y <- rep(c("yes", "no"), each = 25)  # Character

  result <- prepare_plsda_data(X, Y, verbose = FALSE)

  expect_s3_class(result$Y, "factor")
})
