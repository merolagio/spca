test_that("spca fits tall data and tall covariance inputs", {
  m = spca_test_matrices()

  fit_data = spca(m$X_tall, n_comps = 2, method = "c",
                   var_selection = "fwd", stop_criterion = "r2",
                   fat_matrix = FALSE)
  expect_spca_object(fit_data, n_comps = 2, has_scores = TRUE)
  expect_false(fit_data$parameters$fat_matrix)
  expect_true(is.matrix(fit_data$scores))
  expect_equal(dim(fit_data$scores), c(nrow(m$X_tall), 2))

  fit_cov = spca(m$S_tall, n_comps = 2, method = "c",
                  var_selection = "fwd", stop_criterion = "r2")
  expect_spca_object(fit_cov, n_comps = 2, has_scores = FALSE)
  expect_false(fit_cov$parameters$fat_matrix)
  expect_null(fit_cov$scores)
})

test_that("spca accepts numeric data frames", {
  m = spca_test_matrices()

  fit = spca(m$DF_tall, n_comps = 2, method = "c",
              var_selection = "fwd", stop_criterion = "r2",
              fat_matrix = FALSE)

  expect_spca_object(fit, n_comps = 2, has_scores = TRUE)
  expect_equal(rownames(fit$loadings), names(m$DF_tall))
})

test_that("spca fits fat data matrices through the fat backend", {
  m = spca_test_matrices()

  warn = expect_warning(
    spca(m$X_fat, n_comps = 2, method = "c",
                var_selection = "fwd", stop_criterion = "r2",
                fat_matrix = NULL),
    "fat_matrix backend selected because n < p"
  )

  fit = suppressWarnings(
    spca(m$X_fat, n_comps = 2, method = "c",
         var_selection = "fwd", stop_criterion = "r2",
         fat_matrix = NULL)
  )
  expect_spca_object(fit, n_comps = 2, has_scores = TRUE)
  expect_true(fit$parameters$fat_matrix)
  expect_true(is.matrix(fit$scores))
  expect_equal(dim(fit$scores), c(nrow(m$X_fat), 2))
})
