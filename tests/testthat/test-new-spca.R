test_that("new_spca() builds an spca object from loadings and covariance", {
  X = make_tall_data()
  S = stats::var(X) 
    load_mat = make_load_mat()

  fit = suppressMessages(
    new_spca(A = load_mat, S = S, X = X, method_name = "manual")
  )
  expect_spca_object(fit, n_comps = 2, has_scores = TRUE)
  expect_equal(fit$method, "manual")
})

test_that("new_spca() doesn't work without loadings", {
  X = make_tall_data()
  S = stats::cor(X) 
  load_mat = make_load_mat()
  
  fit = new_spca(load_mat, S = S, method_name = "manual")
  
  expect_true(validate_spca(fit))
  expect_spca_object(fit, n_comps = 2, has_scores = FALSE)
})

test_that("new_spca() rejects inconsistent inputs", {
  X = make_tall_data()
  S = stats::cor(X)

  expect_error(new_spca("not a matrix", S = S))
  expect_error(new_spca(matrix(1, nrow = 3, ncol = 2), S = S))
})

