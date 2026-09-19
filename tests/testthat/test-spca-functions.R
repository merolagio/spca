
test_that("is.spca() detects valid and invalid objects", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)
  
  expect_true(is.spca(fit))
  expect_false(is.spca(list()))
  expect_false(is.spca(NULL))
})

test_that("legacy spca fields remain supported", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)
  legacy = fit
  legacy$loadings = legacy$weights
  legacy$weights = NULL
  legacy$loadings_list = legacy$weights_list
  legacy$weights_list = NULL
  
  expect_true(is.spca(legacy))
  expect_true(validate_spca(legacy, quiet = TRUE))
  expect_equal(.get_spca_weights(legacy), fit$weights)
  expect_equal(.get_spca_weights_list(legacy), fit$weights_list)
  expect_equal(.get_spca_loadings(legacy), fit$weights)
  expect_equal(.get_spca_loadings_list(legacy), fit$weights_list)
  
  changed = NULL
  expect_warning(
    {
      changed = change_weights_sign_spca(legacy, 1)
    },
    "change_sign"
  )
  expect_equal(changed$loadings[, 1], -legacy$loadings[, 1])
  expect_equal(changed$loadings_list[[1]], -legacy$loadings_list[[1]])
})


test_that("compare_spca() returns tables and plot on request", {
  X = make_tall_data()
  fit1 = spca(X, n_comps = 2, method = "cspca", fat_matrix = FALSE)
  fit2 = spca(X, n_comps = 2, method = "uspca", fat_matrix = FALSE)

  out = compare_spca(
    list(fit1, fit2),
    n_comps = 2,
    plot_weights = TRUE,
    show_plot = FALSE,
    return_plot = TRUE,
    return_tables = TRUE,
    print_tables = FALSE,
    print_weights = FALSE
  )

  expect_type(out, "list")
  expect_true(length(out) >= 1)
})


test_that("new_spca() builds an spca object from weights and covariance", {
  X = make_tall_data()
  S = stats::var(X) 
  load_mat = make_load_mat()
  
  fit = suppressMessages(
    new_spca(A = load_mat, S = S, X = X, method_name = "manual")
  )
  expect_spca_object(fit, n_comps = 2, has_scores = TRUE)
  expect_equal(fit$method, "manual")
})

test_that("new_spca() works without observation data", {
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


