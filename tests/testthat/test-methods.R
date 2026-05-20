test_that("is.spca() detects valid and invalid objects", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)

  expect_true(is.spca(fit))
  expect_false(is.spca(list()))
  expect_false(is.spca(NULL))
})

test_that("print.spca() can return a table", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)

  tab = print(fit, return_table = TRUE)

  expect_true(is.matrix(tab) || is.data.frame(tab))
  expect_true(ncol(tab) >= 1)
})

test_that("summary.spca() can return a table", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)

  tab = summary(fit, cols = 2, print_table = FALSE, return_table = TRUE)

  expect_true(is.matrix(tab) || is.data.frame(tab))
  expect_true(ncol(tab) >= 1)
})

test_that("change_loadings_sign_spca() changes the requested component sign", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)
  old_loadings = fit$loadings[, 1]

  changed = change_loadings_sign_spca(fit, 1)

  expect_spca_object(changed, n_comps = 2, has_scores = TRUE)
  expect_equal(changed$loadings[, 1], -old_loadings)
  expect_equal(changed$loadings[, 2], fit$loadings[, 2])
})

test_that("show_contributions_spca() returns a list on request", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)

  cont = show_contributions_spca(fit, return_list = TRUE)

  expect_type(cont, "list")
  expect_equal(length(cont), fit$n_comps)
})
