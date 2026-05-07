test_that("fixedindex_list bypasses variable selection by component", {
  m <- spca_test_matrices()

  fit <- spca(m$S_diag, ncomps = 2, method = "c",
              fixedindex_list = list(c(1, 2), 3),
              var_selection = "fwd", stop_criterion = "r2")

  expect_valid_spca(fit, p = ncol(m$S_diag), ncomps = 2)
  expect_equal(fit$indices[[1]], c(1L, 2L))
  expect_equal(fit$indices[[2]], 3L)
  expect_equal(fit$cardinality, c(2L, 1L))
})

test_that("fixedindex_list validates component count and uSPCA cardinality", {
  m <- spca_test_matrices()

  expect_error(
    spca(m$S_diag, ncomps = 2, method = "c",
         fixedindex_list = list(1, 2, 3)),
    "fixedindex_list cannot be longer"
  )

  expect_error(
    spca(m$S_diag, ncomps = 2, method = "u",
         fixedindex_list = list(1, 2)),
    "for uncorrelated components need cardinality"
  )
})
