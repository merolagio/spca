spca_test_matrices <- function() {
  X_tall <- scale(matrix(c(
    2.0,  1.0,  0.5,  0.0,
    1.8,  0.9,  0.4,  0.1,
   -1.9, -1.0, -0.5,  0.0,
   -2.1, -1.1, -0.6, -0.1,
    0.2,  1.5, -1.0,  0.5,
   -0.1,  1.4, -0.9,  0.6,
    0.0, -1.3,  1.1, -0.4,
    0.1, -1.4,  1.0, -0.5
  ), nrow = 8, byrow = TRUE), center = TRUE, scale = FALSE)

  X_fat <- scale(matrix(c(
    2.0,  1.0,  0.5,  0.0,  1.8,  0.2,
    1.7,  0.8,  0.4,  0.1,  1.5,  0.3,
   -1.9, -1.0, -0.5,  0.0, -1.7, -0.2,
   -2.1, -1.1, -0.6, -0.1, -1.8, -0.3
  ), nrow = 4, byrow = TRUE), center = TRUE, scale = FALSE)

  S_tall <- crossprod(X_tall)

  S_diag <- matrix(c(
    4.0, 0.2, 0.1, 0.0,
    0.2, 2.0, 0.1, 0.0,
    0.1, 0.1, 1.0, 0.1,
    0.0, 0.0, 0.1, 0.5
  ), nrow = 4, byrow = TRUE)

  colnames(X_tall) <- paste0("x", seq_len(ncol(X_tall)))
  colnames(X_fat) <- paste0("x", seq_len(ncol(X_fat)))
  dimnames(S_tall) <- list(colnames(X_tall), colnames(X_tall))
  dimnames(S_diag) <- list(paste0("x", seq_len(ncol(S_diag))),
                           paste0("x", seq_len(ncol(S_diag))))

  list(
    X_tall = X_tall,
    S_tall = S_tall,
    X_fat = X_fat,
    S_diag = S_diag,
    DF_tall = as.data.frame(X_tall)
  )
}

expect_valid_spca <- function(object, p, ncomps = NULL) {
  expect_s3_class(object, "spca")
  expect_true(is.matrix(object$loadings))
  expect_equal(nrow(object$loadings), p)
  expect_equal(ncol(object$loadings), object$ncomps)

  if (!is.null(ncomps)) {
    expect_equal(object$ncomps, ncomps)
  }

  expect_length(object$cardinality, object$ncomps)
  expect_length(object$vexp, object$ncomps)
  expect_length(object$cvexp, object$ncomps)
  expect_length(object$vexpPC, object$ncomps)
  expect_length(object$cor_with_PC, object$ncomps)
  expect_length(object$indices, object$ncomps)
  expect_length(object$loadlist, object$ncomps)

  expect_false(anyNA(object$loadings))
  expect_false(anyNA(object$vexp))
  expect_false(anyNA(object$cvexp))
  expect_false(anyNA(object$cor_with_PC))

  expect_true(all(object$cardinality >= 1L))
  expect_true(all(object$vexp >= -1e-10))
  expect_true(all(object$cvexp >= -1e-10))
  expect_true(all(diff(object$cvexp) >= -1e-10))
  expect_true(all(object$cor_with_PC >= -1e-10))
  expect_true(all(object$cor_with_PC <= 1 + 1e-10))

  for (j in seq_len(object$ncomps)) {
    selected <- object$indices[[j]]
    expect_true(all(selected >= 1L))
    expect_true(all(selected <= p))
    expect_equal(length(selected), object$cardinality[j])
    expect_equal(sum(abs(object$loadings[, j]) > 0), object$cardinality[j])
    expect_equal(sqrt(sum(object$loadings[, j]^2)), 1, tolerance = 1e-8)
  }
}
