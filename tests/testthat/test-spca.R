test_that("spca() returns a valid object for tall data", {
  X = make_tall_data()

  fit = spca(X, n_comps = 2, method = "cspca", var_selection = "fwd",
             stop_criterion = "cvexp", fat_matrix = FALSE)

  expect_spca_object(fit, n_comps = 2, has_scores = TRUE)
  expect_equal(fit$parameters$method, "cspca")
  expect_false(fit$parameters$fat_matrix)
})

test_that("spca() returns a valid object for covariance input", {
  S = make_cov_matrix()

  fit = spca(S, n_comps = 2, method = "cspca", var_selection = "fwd",
             stop_criterion = "cvexp")

  expect_spca_object(fit, n_comps = 2, has_scores = FALSE)
  expect_false(fit$parameters$fat_matrix)
})

test_that("spca() returns a valid object for fat data", {
  X = make_fat_data()

  warn = expect_warning(
  spca(X, n_comps = 2, method = "cspca", var_selection = "fwd",
             stop_criterion = "r2", fat_matrix = TRUE),
  "Centering column means to zero"
)
  fit = suppressWarnings(
    spca(X, n_comps = 2, method = "cspca", var_selection = "fwd",
         stop_criterion = "r2", fat_matrix = TRUE)
  )
  expect_spca_object(fit, n_comps = 2, has_scores = TRUE)
  expect_true(fit$parameters$fat_matrix)
})

test_that("spca() accepts method abbreviations", {
  X = make_tall_data()

  fit_c = spca(X, n_comps = 2, method = "c", var_selection = "f",
               stop_criterion = "c", fat_matrix = FALSE)
  fit_u = spca(X, n_comps = 2, method = "u", var_selection = "f",
               stop_criterion = "c", fat_matrix = FALSE)
  fit_p = spca(X, n_comps = 2, method = "p", var_selection = "f",
               stop_criterion = "c", fat_matrix = FALSE)

  expect_spca_object(fit_c, n_comps = 2, has_scores = TRUE)
  expect_spca_object(fit_u, n_comps = 2, has_scores = TRUE)
  expect_spca_object(fit_p, n_comps = 2, has_scores = TRUE)
})

test_that("spca() supports cvexp stopping", {
  X = make_tall_data()

  fit = spca(X, n_comps = NULL, ncomp_by_cvexp = 0.50, method = "cspca",
             var_selection = "fwd", stop_criterion = "cvexp",
             fat_matrix = FALSE)

  expect_spca_object(fit, has_scores = TRUE)
  expect_true(fit$n_comps >= 1)
})

test_that("spca() rejects invalid control combinations", {
  X = make_tall_data()

  expect_error(spca(X), "specify either n_comps or ncomp_by_cvexp")
  expect_error(spca(X, n_comps = NULL, ncomp_by_cvexp = NULL))
  expect_error(spca(X, n_comps = 2, method = "wrong"))
  expect_error(spca(X, n_comps = 2, var_selection = "wrong"))
  expect_error(spca(X, n_comps = 2, stop_criterion = "wrong"))
  expect_error(spca(X, n_comps = 2, intensive = TRUE,
                    var_selection = "bkw"))
  expect_error(spca(X, n_comps = 2, intensive = TRUE,
                    stop_criterion = "r2"))
})
