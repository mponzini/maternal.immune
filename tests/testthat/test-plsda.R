test_that("run_plsda works with valid input", {
  skip_if_not_installed("mixOmics")

  # Create test data
  set.seed(123)
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  colnames(X) <- paste0("Cytokine_", 1:5)
  Y <- factor(rep(c("yes", "no"), each = 25))

  # Run PLS-DA
  result <- run_plsda(X, Y, ncomp = 2)

  # Check output structure
  expect_type(result, "list")
  expect_s3_class(result, "plsda_result")
  expect_named(result, c("model", "vip_scores", "X", "Y"))

  # Check VIP scores
  expect_s3_class(result$vip_scores, "data.frame")
  expect_equal(nrow(result$vip_scores), ncol(X))
  expect_named(result$vip_scores, c("variable", "vip_score"))

  # Check that VIP scores are positive
  expect_true(all(result$vip_scores$vip_score > 0))
})


test_that("run_plsda validates input dimensions", {
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  Y <- factor(rep(c("yes", "no"), each = 20))  # Wrong length

  expect_error(run_plsda(X, Y), "Number of rows in X must match length of Y")
})


test_that("run_plsda handles missing values appropriately", {
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  X[1, 1] <- NA
  Y <- factor(rep(c("yes", "no"), each = 25))

  expect_error(run_plsda(X, Y), "contains missing values")
})


test_that("run_plsda converts data.frame to matrix", {
  skip_if_not_installed("mixOmics")

  set.seed(123)
  X <- data.frame(matrix(rnorm(50 * 5), nrow = 50, ncol = 5))
  colnames(X) <- paste0("Cytokine_", 1:5)
  Y <- factor(rep(c("yes", "no"), each = 25))

  result <- run_plsda(X, Y, ncomp = 1)

  expect_type(result$X, "double")
  expect_true(is.matrix(result$X))
})


test_that("run_plsda ensures Y is a factor", {
  skip_if_not_installed("mixOmics")

  set.seed(123)
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  Y <- rep(c("yes", "no"), each = 25)  # Character, not factor

  result <- run_plsda(X, Y, ncomp = 1)

  expect_s3_class(result$Y, "factor")
})


test_that("print.plsda_result works", {
  skip_if_not_installed("mixOmics")

  set.seed(123)
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  colnames(X) <- paste0("Cytokine_", 1:5)
  Y <- factor(rep(c("yes", "no"), each = 25))

  result <- run_plsda(X, Y, ncomp = 2)

  # Should not error
  expect_output(print(result), "PLS-DA Analysis Results")
  expect_output(print(result), "Number of samples")
  expect_output(print(result), "VIP Scores")
})
