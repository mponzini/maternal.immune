test_that("calculate_vip produces valid output", {
  skip_if_not_installed("mixOmics")

  # Create test data and fit model
  set.seed(123)
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  colnames(X) <- paste0("Cytokine_", 1:5)
  Y <- factor(rep(c("yes", "no"), each = 25))

  model <- mixOmics::plsda(X, Y, ncomp = 2)
  vip_scores <- calculate_vip(model)

  # Check structure
  expect_s3_class(vip_scores, "data.frame")
  expect_equal(nrow(vip_scores), ncol(X))
  expect_named(vip_scores, c("variable", "vip_score"))

  # Check that scores are positive
  expect_true(all(vip_scores$vip_score > 0))

  # Check that results are sorted in descending order
  expect_true(all(diff(vip_scores$vip_score) <= 0))
})


test_that("calculate_vip handles models with one component", {
  skip_if_not_installed("mixOmics")

  set.seed(123)
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  colnames(X) <- paste0("Cytokine_", 1:5)
  Y <- factor(rep(c("yes", "no"), each = 25))

  model <- mixOmics::plsda(X, Y, ncomp = 1)
  vip_scores <- calculate_vip(model)

  expect_equal(nrow(vip_scores), ncol(X))
  expect_true(all(vip_scores$vip_score > 0))
})


test_that("calculate_vip handles unnamed variables", {
  skip_if_not_installed("mixOmics")

  set.seed(123)
  X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
  # No column names
  Y <- factor(rep(c("yes", "no"), each = 25))

  model <- mixOmics::plsda(X, Y, ncomp = 2)
  vip_scores <- calculate_vip(model)

  expect_true(all(grepl("^Var_", vip_scores$variable)))
})


test_that("plot_vip creates a ggplot object", {
  skip_if_not_installed("mixOmics")
  skip_if_not_installed("ggplot2")

  set.seed(123)
  X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  colnames(X) <- paste0("Cytokine_", 1:10)
  Y <- factor(rep(c("yes", "no"), each = 25))

  result <- run_plsda(X, Y, ncomp = 2)
  p <- plot_vip(result$vip_scores, top_n = 5)

  expect_s3_class(p, "ggplot")
})


test_that("plot_vip handles custom parameters", {
  skip_if_not_installed("mixOmics")
  skip_if_not_installed("ggplot2")

  # Create mock VIP data
  vip_df <- data.frame(
    variable = paste0("Var_", 1:20),
    vip_score = seq(2.0, 0.1, length.out = 20)
  )

  p <- plot_vip(vip_df, top_n = 10, threshold = 0.8, title = "Custom Title")

  expect_s3_class(p, "ggplot")
  # Check that only top 10 are shown
  expect_equal(length(p$data$variable), 10)
})
