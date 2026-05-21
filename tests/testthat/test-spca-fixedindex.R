test_that("fixed_index_list bypasses variable selection by component", {
  m = spca_test_matrices()
dim(m$S_diag)

  fit = spca(m$S_diag, n_comps = 2, method = "c",
              fixed_index_list = list(c(1, 2), c(3, 4)),
              var_selection = "fwd", objective = "r2")

  expect_spca_object(fit, n_comps = 2, has_scores = FALSE)
  expect_equal(fit$indices[[1]], c(1L, 2L))
  expect_equal(fit$indices[[2]], c(3, 4))
  expect_equal(fit$cardinality, c(2L, 2L))
})

test_that("fixed_index_list validates component count and uSPCA cardinality", {
  m = spca_test_matrices()

  expect_error(
    spca(m$S_diag, n_comps = 2, method = "c",
         fixed_index_list = list(1, 2, 3, 4, 5)),
    "fixed_index_list must be a mutually exclusive and exhaustive
             partition of the variables of length > 1"
  )

  expect_error(
    spca(m$S_diag, n_comps = 2, method = "u",
         fixed_index_list = list(c(1, 2, 3), c(4))),
    "for uspca components need cardinality not less than component order"
  )
})
