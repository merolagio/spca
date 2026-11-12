test_that("backend lists pair selection indices with the correct matrix weights", {
  set.seed(20261112)
  for (dims in list(c(80, 10), c(15, 25))) {
    X <- scale(matrix(rnorm(prod(dims)), dims[1], dims[2]), scale = FALSE)
    colnames(X) <- paste0("v", seq_len(ncol(X)))
    fixed <- list(c(8L, 2L, 6L, 1L), c(7L, 3L, 9L, 2L))
    for (method in c("cspca", "uspca", "pspca")) {
      for (support in list(NULL, fixed)) {
        fit <- spca(X, n_comps = 2, alpha = .8, method = method,
                    var_selection = "fwd", objective = "r2",
                    fixed_index_list = support)
        for (j in seq_along(fit$indices)) {
          expect_equal(fit$weights_list[[j]], fit$weights[fit$indices[[j]], j])
        }
        if (!is.null(support))
          expect_identical(unname(lapply(fit$indices, as.integer)), fixed)
      }
    }
  }
})

test_that("forward R2 output retains selection order in both backends", {
  set.seed(912)
  for (dims in list(c(80, 10), c(15, 25))) {
    X <- scale(matrix(rnorm(prod(dims)), dims[1], dims[2]), scale = FALSE)
    X <- sweep(X, 2, seq_len(ncol(X)), "*")
    # Independent sequential regression oracle for the first PC.
    u <- svd(X, nu = 1, nv = 0)$u[, 1]
    Xw <- X; uw <- u; remaining <- seq_len(ncol(X))
    selected <- integer(); r2 <- 0
    while (r2 < .99 && length(remaining)) {
      candidate <- Xw[, remaining, drop = FALSE]
      gains <- colSums(candidate * uw)^2 / colSums(candidate^2)
      k <- remaining[which.max(gains)]
      selected <- c(selected, k)
      remaining <- setdiff(remaining, k)
      x <- Xw[, k]
      r2 <- r2 + sum(uw * x)^2 / sum(x^2)
      uw <- uw - x * sum(uw * x) / sum(x^2)
      if (length(remaining)) {
        Xw[, remaining] <- Xw[, remaining, drop = FALSE] -
          tcrossprod(x, as.numeric(crossprod(x, Xw[, remaining, drop = FALSE])) / sum(x^2))
      }
    }
    fit <- spca(X, n_comps = 1, alpha = .99, method = "cspca",
                var_selection = "fwd", objective = "r2")
    expect_identical(as.integer(fit$indices[[1]]), selected)
    expect_false(identical(selected, sort(selected)))
  }
})
