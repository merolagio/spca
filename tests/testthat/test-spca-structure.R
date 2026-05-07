test_that("spca fits tall data and tall covariance inputs", {
  m <- spca_test_matrices()

  fit_data <- spca(m$X_tall, ncomps = 2, method = "c",
                   var_selection = "fwd", stop_criterion = "r2",
                   fat_matrix = FALSE)
  expect_valid_spca(fit_data, p = ncol(m$X_tall), ncomps = 2)
  expect_false(fit_data$parameters$fat_matrix)
  expect_true(is.matrix(fit_data$scores))
  expect_equal(dim(fit_data$scores), c(nrow(m$X_tall), 2))

  fit_cov <- spca(m$S_tall, ncomps = 2, method = "c",
                  var_selection = "fwd", stop_criterion = "r2")
  expect_valid_spca(fit_cov, p = ncol(m$S_tall), ncomps = 2)
  expect_false(fit_cov$parameters$fat_matrix)
  expect_null(fit_cov$scores)
})

test_that("spca accepts numeric data frames", {
  m <- spca_test_matrices()

  fit <- spca(m$DF_tall, ncomps = 2, method = "c",
              var_selection = "fwd", stop_criterion = "r2",
              fat_matrix = FALSE)

  expect_valid_spca(fit, p = ncol(m$DF_tall), ncomps = 2)
  expect_equal(rownames(fit$loadings), names(m$DF_tall))
})

test_that("spca fits fat data matrices through the fat backend", {
  m <- spca_test_matrices()

  fit <- NULL
  expect_warning(
    fit <- spca(m$X_fat, ncomps = 2, method = "c",
                var_selection = "fwd", stop_criterion = "r2",
                fat_matrix = NULL),
    "fat backend"
  )

  expect_valid_spca(fit, p = ncol(m$X_fat), ncomps = 2)
  expect_true(fit$parameters$fat_matrix)
  expect_true(is.matrix(fit$scores))
  expect_equal(dim(fit$scores), c(nrow(m$X_fat), 2))
})
