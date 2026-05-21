test_that("spca rejects invalid inputs", {
  m = spca_test_matrices()
X = m$X_tall
X[1,1] = NA
  expect_error(spca("not a matrix"), "M must be a matrix or data.frame", fixed = TRUE)
  expect_error(spca(matrix(letters[1:4], 2, 2)), "M must be numeric")
  expect_error(spca(X, 1), "M cannot contain missing values")
  

  expect_error(spca(m$X_tall, alpha = 0), "alpha must be NULL or a numeric scalar in (0, 1]", fixed = TRUE)
  expect_error(spca(m$X_tall, alpha = 1.1), "alpha must be NULL or a numeric scalar in (0, 1]", fixed = TRUE)
  expect_error(spca(m$X_tall, n_comps = -1), "n_comps must be NULL or a nonnegative integer", fixed = TRUE)
  expect_error(spca(m$X_tall, ncomp_by_cvexp =  1.2),
               "n_comp_by_cvexp must be NULL or a numeric scalar in (0, 1]",
               fixed = TRUE)

  expect_error(spca(m$X_tall, n_comps = 2, method = "bad"),
               "method must start with one of c, p or u")
  expect_error(spca(m$X_tall, n_comps = 2, var_selection = "xxx"),
               "var_selection must start with one of f, b, or s")
  expect_error(spca(m$X_tall, n_comps = 2, objective = "bad"),
               "objective must start with one of r or c")

  expect_error(spca(m$X_tall, intensive = NA))
  
  expect_error(spca(m$X_tall, n_comps = 2, fat_matrix = NA))

  expect_error(spca(m$X_tall, n_comps = 2, fixed_index_list = 1),
               "fixed_index_list can be either NULL or contain at least 2 elements")
  expect_error(spca(m$X_tall, n_comps = 2, fixed_index_list = list(c(1, NA))),
               "fixed_index_list must not contain missing values")

  expect_error(spca(m$X_tall, n_comps = 2, eps_pm_loading = 0), "eps_pm_loading must be NULL or a positive numeric scalar")
  
  expect_error(spca(m$X_tall, n_comps = 2, maxiter_pm_loading = 0), "maxiter_pm_loading must be NULL or a positive integer")
  expect_error(spca(m$X_tall, n_comps = 2, eps_pm_varsel = 0), "eps_pm_varsel must be NULL or a positive numeric scalar")
  expect_error(spca(m$X_tall, n_comps = 2, maxiter_pm_varsel = 0), "maxiter_pm_varsel must be NULL or a positive integer scalar")
})

