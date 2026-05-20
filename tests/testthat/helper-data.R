make_tall_data = function(n_vars = 8) {
  data("holzinger", package = "spca", envir = environment())
  X = as.matrix(holzinger[, seq_len(n_vars)])
  storage.mode(X) = "double"
  X
}

make_fat_data = function(n_vars = 8) {
  t(make_tall_data(n_vars = n_vars))
}

make_cov_matrix = function(n_vars = 8) {
  stats::cor(make_tall_data(n_vars = n_vars))
}


expect_spca_object = function(x, n_comps = NULL, has_scores = NULL) {
  expect_s3_class(x, "spca")
  expect_true(is.spca(x))
#browser()
  required_names = c(
    "loadings", "contributions", "n_comps", "cardinality",
    "vexp", "vexp_pc", "cvexp", "rvexp", "rcvexp",
    "cor_with_pc", "tot_var", "loadings_list", "indices",
     "spc_cor"
  )
  expect_true(all(required_names %in% names(x)))
#browser()
  if (!is.null(n_comps)) {
    expect_equal(x$n_comps, n_comps)
    expect_equal(ncol(x$loadings), n_comps)
    expect_equal(ncol(x$contributions), n_comps)
    expect_equal(length(x$cardinality), n_comps)
    expect_equal(length(x$vexp), n_comps)
    expect_equal(length(x$cvexp), n_comps)
    expect_equal(length(x$loadings_list), n_comps)
    expect_equal(length(x$indices), n_comps)
  }

  expect_equal(dim(x$contributions), dim(x$loadings))
  expect_true(all(is.finite(x$vexp)))
  expect_true(all(is.finite(x$cvexp)))
  expect_true(all(x$cardinality >= 0))
  expect_true(all(x$cor_with_pc >= -1 & x$cor_with_pc <= 1))

  if (!is.null(has_scores)) {
    if (has_scores) {
      expect_true(is.matrix(x$scores))
      expect_equal(ncol(x$scores), x$n_comps)
    } else {
      expect_null(x$scores)
    }
  }
}

expect_pca_object = function(x, n_comps = NULL, has_scores = NULL) {
  expect_spca_object(x, n_comps = n_comps, has_scores = has_scores)
  expect_equal(x$method, "PCA")
  expect_true(all(x$cardinality == nrow(x$loadings)))
  expect_true(all(abs(x$rvexp - 1) < 1e-12))
  expect_true(all(abs(x$rcvexp - 1) < 1e-12))
  expect_true(all(abs(x$cor_with_pc - 1) < 1e-12))
}
