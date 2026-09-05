test_that("fixed_index_list accepts overlapping and nonexhaustive supports", {
  m = spca_test_matrices()

  fit = spca(m$S_diag, n_comps = 2, method = "c",
              fixed_index_list = list(c(1, 2), c(2, 3)),
              var_selection = "fwd", objective = "r2")

  expect_spca_object(fit, n_comps = 2, has_scores = FALSE)
  expect_equal(fit$indices[[1]], c(1L, 2L))
  expect_equal(fit$indices[[2]], c(2L, 3L))
  expect_equal(fit$cardinality, c(2L, 2L))

  fit_fat = spca(m$X_fat, n_comps = 2, method = "c",
                 fixed_index_list = list(c(1, 2), c(2, 3)),
                 var_selection = "fwd", objective = "r2")

  expect_spca_object(fit_fat, n_comps = 2, has_scores = TRUE)
  expect_equal(fit_fat$indices[[1]], c(1L, 2L))
  expect_equal(fit_fat$indices[[2]], c(2L, 3L))
})

test_that("fixed_index_list can fix selected components only", {
  m = spca_test_matrices()

  first_only = spca(m$S_diag, n_comps = 2, method = "c",
                    fixed_index_list = list(c(1, 3)))
  expect_equal(first_only$indices[[1]], c(1L, 3L))

  second_only = spca(m$S_diag, n_comps = 2, method = "c",
                     fixed_index_list = list(NULL, c(2, 4)))
  expect_equal(second_only$indices[[2]], c(2L, 4L))

  partial = spca(diag(c(5, 4, 3, 2, 1)), n_comps = 4, method = "c",
                 fixed_index_list = list(c(1, 2), c(2, 3)))
  expect_equal(partial$n_comps, 4L)
  expect_equal(partial$indices[[1]], c(1L, 2L))
  expect_equal(partial$indices[[2]], c(2L, 3L))

  first_only_fat = spca(m$X_fat, n_comps = 2, method = "c",
                        fixed_index_list = list(c(1, 3)))
  expect_equal(first_only_fat$indices[[1]], c(1L, 3L))
})

test_that("extra and duplicate fixed indices are handled with warnings", {
  m = spca_test_matrices()

  extra = expect_warning(
    spca(m$S_diag, n_comps = 2, method = "c",
         fixed_index_list = list(c(1, 2), c(2, 3), c(3, 4))),
    "extra elements are ignored"
  )
  expect_equal(extra$indices[[1]], c(1L, 2L))
  expect_equal(extra$indices[[2]], c(2L, 3L))

  duplicate = expect_warning(
    spca(m$S_diag, n_comps = 2, method = "c",
         fixed_index_list = list(c(1, 1, 2), c(3, 4))),
    "duplicate indices removed"
  )
  expect_equal(duplicate$indices[[1]], c(1L, 2L))
})

test_that("fixed_index_list retains the uSPCA cardinality requirement", {
  m = spca_test_matrices()

  expect_error(
    spca(m$S_diag, n_comps = 2, method = "u",
         fixed_index_list = list(c(1, 2, 3), c(4))),
    "for uspca components need cardinality not less than component order"
  )
})

test_that("n_comps takes precedence over ncomp_by_cvexp", {
  m = spca_test_matrices()

  fit = spca(m$S_diag, n_comps = 2, ncomp_by_cvexp = 0.01,
             method = "c", fixed_index_list = list(c(1, 2)))

  expect_equal(fit$n_comps, 2L)
  expect_equal(fit$indices[[1]], c(1L, 2L))
})
