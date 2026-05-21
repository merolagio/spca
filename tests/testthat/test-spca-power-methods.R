test_that("power method options return valid fits", {
  m = spca_test_matrices()

#  expectWarning
  fit_pmc = suppressWarnings(spca(m$X_tall, n_comps = 2, method = "c",
                  var_selection = "fwd", objective = "r2",
                  fat_matrix = FALSE, pm_loading = TRUE)
  )
  expect_spca_object(fit_pmc, n_comps = 2, has_scores =  TRUE)

  fit_pmvs = suppressWarnings(spca(m$X_tall, n_comps = 2, method = "c",
                   var_selection = "fwd", objective = "r2",
                   fat_matrix = FALSE, pm_varsel = TRUE)
  )
  expect_spca_object(fit_pmvs, n_comps = 2 , has_scores =  TRUE)

})
