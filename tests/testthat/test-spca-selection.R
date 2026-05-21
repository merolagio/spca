test_that("thin backend supports selection methods and stop criteria", {
  m = spca_test_matrices()

  for (selection in c("fwd", "bkw", "step")) {
    fit = spca(m$X_tall, n_comps = 2, method = "c",
                var_selection = selection, objective = "r2",
                fat_matrix = FALSE)
    expect_spca_object(fit, n_comps = 2, has_scores = TRUE)
  }

  fit_cvexp = spca(m$X_tall, n_comps = 2, method = "c",
                    var_selection = "fwd", objective = "cvexp",
                    fat_matrix = FALSE)
  expect_spca_object(fit_cvexp, n_comps = 2, has_scores = TRUE)

  fit_intensive = spca(m$X_tall, n_comps = 2, method = "c",
                        var_selection = "fwd", objective = "cvexp",
                        intensive = TRUE, fat_matrix = FALSE)
  expect_spca_object(fit_intensive, n_comps = 2, has_scores = TRUE)
})

test_that("loading methods produce valid fits", {
  m = spca_test_matrices()

  for (method in c("c", "u", "p")) {
    fit = spca(m$S_diag, n_comps = 2, method = method,
                var_selection = "fwd", objective = "r2")
    expect_spca_object(fit, n_comps = 2, has_scores = FALSE)
  }

  fit_recycled = spca(m$S_diag, n_comps = 3, method = c("c", "p"),
                       var_selection = "fwd", objective = "r2")
  expect_spca_object(fit_recycled, n_comps = 3, has_scores = FALSE)
})

test_that("fat backend runs with supported selection options", {
  m = spca_test_matrices()
  
  fit = spca(m$X_fat, n_comps = 2, method = "c",
               var_selection = "fwd", objective = "r2",
               fat_matrix = TRUE)
  expect_spca_object(fit, n_comps = 2, has_scores = TRUE)
  expect_true(fit$parameters$fat_matrix)
})


test_that("fat backend issue eror unsupported selection options", {
  m = spca_test_matrices()


  fit = expect_error(
    spca(m$X_fat, n_comps = 2, method = "c",
                var_selection = "bkw", objective = "r2",
                fat_matrix = TRUE),
    "var_selection bkw is not implemented for fat matrices"
  )
 
  fit = expect_error(
    spca(m$X_fat, n_comps = 2, method = "c",
               var_selection = "fwd", intensive = TRUE, 
               fat_matrix = TRUE),
    "Intensive variable selection not implemented for fat matrices."
  )
  
})
