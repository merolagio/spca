test_that("power method options return valid fits", {
  m <- spca_test_matrices()

  fit_pmc <- spca(m$X_tall, ncomps = 2, method = "c",
                  var_selection = "fwd", stop_criterion = "r2",
                  fat_matrix = FALSE, PMC = TRUE)
  expect_valid_spca(fit_pmc, p = ncol(m$X_tall), ncomps = 2)

  fit_pmvs <- spca(m$X_tall, ncomps = 2, method = "c",
                   var_selection = "fwd", stop_criterion = "r2",
                   fat_matrix = FALSE, PMVS = TRUE)
  expect_valid_spca(fit_pmvs, p = ncol(m$X_tall), ncomps = 2)
})
