test_that("thin backend supports selection methods and stop criteria", {
  m <- spca_test_matrices()

  for (selection in c("fwd", "bkw", "step")) {
    fit <- spca(m$X_tall, ncomps = 2, method = "c",
                var_selection = selection, stop_criterion = "r2",
                fat_matrix = FALSE)
    expect_valid_spca(fit, p = ncol(m$X_tall), ncomps = 2)
  }

  fit_cvexp <- spca(m$X_tall, ncomps = 2, method = "c",
                    var_selection = "fwd", stop_criterion = "cvexp",
                    fat_matrix = FALSE)
  expect_valid_spca(fit_cvexp, p = ncol(m$X_tall), ncomps = 2)

  fit_intensive <- spca(m$X_tall, ncomps = 2, method = "c",
                        var_selection = "fwd", stop_criterion = "cvexp",
                        intensive = TRUE, fat_matrix = FALSE)
  expect_valid_spca(fit_intensive, p = ncol(m$X_tall), ncomps = 2)
})

test_that("loading methods produce valid fits", {
  m <- spca_test_matrices()

  for (method in c("c", "u", "p")) {
    fit <- spca(m$S_diag, ncomps = 2, method = method,
                var_selection = "fwd", stop_criterion = "r2")
    expect_valid_spca(fit, p = ncol(m$S_diag), ncomps = 2)
  }

  fit_recycled <- spca(m$S_diag, ncomps = 3, method = c("c", "p"),
                       var_selection = "fwd", stop_criterion = "r2")
  expect_valid_spca(fit_recycled, p = ncol(m$S_diag), ncomps = 3)
})

test_that("fat backend switches unsupported selection options", {
  m <- spca_test_matrices()

  fit <- NULL
  expect_warning(
    fit <- spca(m$X_fat, ncomps = 2, method = "c",
                var_selection = "bkw", stop_criterion = "r2",
                fat_matrix = TRUE),
    "Only forward variable selection for fat matrices is available"
  )

  expect_valid_spca(fit, p = ncol(m$X_fat), ncomps = 2)
  expect_true(fit$parameters$fat_matrix)
})
