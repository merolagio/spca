test_that("pca() returns a valid object for tall data", {
  X = make_tall_data()

  fit =   suppressWarnings(
    pca(X, n_comps = 3, fat_matrix = FALSE, qq_plot = FALSE)
  )

  expect_pca_object(fit, n_comps = 3, has_scores = TRUE)
  expect_equal(nrow(fit$loadings), ncol(X))
})

test_that("pca() returns a valid object for covariance input", {
  S = make_cov_matrix()

  fit =   suppressWarnings(
    pca(S, n_comps = 3, qq_plot = FALSE)
    )
  
  expect_pca_object(fit, n_comps = 3, has_scores = FALSE)
  expect_equal(nrow(fit$loadings), ncol(S))
})

test_that("pca() returns a valid object for fat data", {
  X = make_fat_data()
  
  fit = suppressWarnings(
    pca(X, n_comps = 3, fat_matrix = TRUE, qq_plot = FALSE)
    )
  expect_pca_object(fit, n_comps = 3, has_scores = TRUE)
  expect_equal(nrow(fit$loadings), ncol(X))
})

test_that("pca() supports the power-method backend", {
  X = make_tall_data()

  fit = suppressWarnings(
    pca(X, n_comps = 2, fat_matrix = FALSE, pm = TRUE, qq_plot = FALSE)
  )

  expect_pca_object(fit, n_comps = 2, has_scores = TRUE)
})

test_that("pca() rejects invalid inputs", {
  X = make_tall_data()

  expect_warning(pca(X, n_comps = 0, qq_plot = FALSE))
  expect_error(pca(X, n_comps = 2, fat_matrix = NA, qq_plot = FALSE))
  expect_error(pca(X, n_comps = 2, eps_pm = -1, qq_plot = FALSE))
  expect_error(pca(X, n_comps = 2, maxiter_pm = 0, qq_plot = FALSE))
})
