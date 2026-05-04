# tests/testthat/test-Varsel_pc_r.R

# Deterministic test problem with moderate correlation structure.
make_pc_problem <- function() {
  set.seed(20260310)

  n <- 80
  z1 <- rnorm(n)
  z2 <- rnorm(n)
  z3 <- rnorm(n)
  e  <- matrix(rnorm(n * 6, sd = 0.25), nrow = n, ncol = 6)

  X <- cbind(
    1.00 * z1 + 0.30 * z2 + e[, 1],
    0.85 * z1 + 0.10 * z3 + e[, 2],
    0.70 * z1 - 0.20 * z2 + e[, 3],
    0.95 * z2 + 0.10 * z3 + e[, 4],
    0.80 * z2 + 0.20 * z1 + e[, 5],
    0.60 * z3 + 0.10 * z1 + e[, 6]
  )

  X <- scale(X, center = TRUE, scale = FALSE)
  S <- stats::cov(X)
  ee <- eigen(S, symmetric = TRUE)
  v1 <- ee$vectors[, 1]
  lambda1 <- ee$values[1]
  si <- v1 * sqrt(lambda1) #i think it should be lambda
  y <- drop(X %*% v1)

  list(X = X, y = y, S = S, si = si, totvexp = lambda1)
}


lm_r2_from_subset <- function(X, y, ind) {
  if (length(ind) == 0L) {
    return(0)
  }

  dat <- data.frame(y = y, X[, ind, drop = FALSE])
  out <- stats::lm(y ~ ., data = dat)
  unname(summary(out)$r.squared)
}

expect_valid_result <- function(fit, p) {
  testthat::expect_type(fit$converged, "logical")
  testthat::expect_length(fit$converged, 1L)
  testthat::expect_type(fit$card, "integer")
  testthat::expect_length(fit$card, 1L)
  testthat::expect_equal(fit$card, length(fit$ind))
  testthat::expect_true(all(fit$ind >= 1L & fit$ind <= p))
  testthat::expect_false(anyDuplicated(fit$ind) > 0L)
  testthat::expect_true(is.numeric(fit$r2))
  testthat::expect_length(fit$r2, 1L)
  testthat::expect_true(fit$r2 >= -1e-12)
  testthat::expect_true(fit$r2 <= 1 + 1e-12)
}


testthat::test_that("forward starts from the variable with largest simple R2", {
  pr <- make_pc_problem()
  vt <- pr$si^2 / diag(pr$S)
  best <- which.max(vt)

  fit <- Varsel_pc_r(
    S = pr$S,
    si = pr$si,
    totvexp = pr$totvexp,
    pvexp = 0.99 * max(vt),
    method = 0L
  )

  expect_valid_result(fit, ncol(pr$X))
  testthat::expect_true(fit$converged)
  testthat::expect_equal(fit$card, 1L)
  testthat::expect_equal(fit$ind, best)
  testthat::expect_equal(fit$r2, max(vt), tolerance = 1e-10)

  r2_lm <- lm_r2_from_subset(pr$X, pr$y, fit$ind)
  testthat::expect_equal(fit$r2, r2_lm, tolerance = 1e-8)
})


testthat::test_that("all methods return coherent output and R2 matches an independent lm fit", {
  pr <- make_pc_problem()
  pv <- 0.85

  for (m in c(0L, 1L, 2L)) {
    fit <- Varsel_pc_r(
      S = pr$S,
      si = pr$si,
      totvexp = pr$totvexp,
      pvexp = pv,
      method = m
    )

    expect_valid_result(fit, ncol(pr$X))

    r2_lm <- lm_r2_from_subset(pr$X, pr$y, fit$ind)
    testthat::expect_equal(fit$r2, r2_lm, tolerance = 1e-8)

    if (fit$converged) {
      testthat::expect_true(fit$r2 >= pv - 1e-10)
    }
  }
})


testthat::test_that("force_in variables are kept and force_out variables are excluded", {
  pr <- make_pc_problem()
  fin  <- c(2L, 4L)
  fout <- c(1L, 6L)

  for (m in c(0L, 1L, 2L)) {
    fit <- Varsel_pc_r(
      S = pr$S,
      si = pr$si,
      totvexp = pr$totvexp,
      pvexp = 0.75,
      method = m,
      force_in = fin,
      force_out = fout
    )

    expect_valid_result(fit, ncol(pr$X))
    testthat::expect_true(all(fin %in% fit$ind))
    testthat::expect_false(any(fout %in% fit$ind))

    r2_lm <- lm_r2_from_subset(pr$X, pr$y, fit$ind)
    testthat::expect_equal(fit$r2, r2_lm, tolerance = 1e-8)
  }
})


testthat::test_that("overlap and duplicate constraints generate warnings and force_in wins", {
  pr <- make_pc_problem()

  testthat::expect_warning(
    fit <- Varsel_pc_r(
      S = pr$S,
      si = pr$si,
      totvexp = pr$totvexp,
      pvexp = 0.70,
      method = 0L,
      force_in = c(2L, 2L, 4L),
      force_out = c(4L, 6L, 6L)
    ),
    regexp = "Duplicate indices|appears in both force_in and force_out"
  )

  expect_valid_result(fit, ncol(pr$X))
  testthat::expect_true(all(c(2L, 4L) %in% fit$ind))
  testthat::expect_false(6L %in% fit$ind)
})


testthat::test_that("invalid inputs are rejected", {
  pr <- make_pc_problem()

  testthat::expect_error(
    Varsel_pc_r(pr$S[, -1, drop = FALSE], pr$si, pr$totvexp, 0.8, 0L),
    regexp = "square matrix"
  )

  testthat::expect_error(
    Varsel_pc_r(pr$S, pr$si[-1], pr$totvexp, 0.8, 0L),
    regexp = "same length"
  )

  testthat::expect_error(
    Varsel_pc_r(pr$S, pr$si, pr$totvexp, 1.1, 0L),
    regexp = "pvexp"
  )

  testthat::expect_error(
    Varsel_pc_r(pr$S, pr$si, pr$totvexp, 0.8, 9L),
    regexp = "method"
  )

  testthat::expect_error(
    Varsel_pc_r(pr$S, pr$si, pr$totvexp, 0.8, 0L, fullrank = -1),
    regexp = "fullrank"
  )

  testthat::expect_error(
    Varsel_pc_r(pr$S, pr$si, pr$totvexp, 0.8, 0L, force_in = 0L),
    regexp = "must be positive"
  )

  testthat::expect_error(
    Varsel_pc_r(pr$S, pr$si, pr$totvexp, 0.8, 0L, force_out = 0L),
    regexp = "must be positive"
  )
})


testthat::test_that("empty admissible set returns an empty model with a warning", {
  pr <- make_pc_problem()
  p <- ncol(pr$X)

  testthat::expect_warning(
    fit_fwd <- Varsel_pc_r(
      S = pr$S,
      si = pr$si,
      totvexp = pr$totvexp,
      pvexp = 0.80,
      method = 0L,
      force_out = seq_len(p)
    ),
    regexp = "No admissible variables available"
  )

  testthat::expect_false(fit_fwd$converged)
  testthat::expect_equal(fit_fwd$card, 0L)
  testthat::expect_length(fit_fwd$ind, 0L)
  testthat::expect_equal(fit_fwd$r2, 0)

  testthat::expect_warning(
    fit_bwd <- Varsel_pc_r(
      S = pr$S,
      si = pr$si,
      totvexp = pr$totvexp,
      pvexp = 0.80,
      method = 1L,
      force_out = seq_len(p)
    ),
    regexp = "No admissible variables available"
  )

  testthat::expect_false(fit_bwd$converged)
  testthat::expect_equal(fit_bwd$card, 0L)
  testthat::expect_length(fit_bwd$ind, 0L)
  testthat::expect_equal(fit_bwd$r2, 0)
})


testthat::test_that("with pvexp = 0 the selected model reduces to the forced set", {
  pr <- make_pc_problem()

  fit0 <- Varsel_pc_r(
    S = pr$S,
    si = pr$si,
    totvexp = pr$totvexp,
    pvexp = 0,
    method = 0L,
    force_in = c(3L, 5L)
  )

  testthat::expect_true(fit0$converged)
  testthat::expect_equal(sort(fit0$ind), c(3L, 5L))
  testthat::expect_equal(fit0$card, 2L)

  fit1 <- Varsel_pc_r(
    S = pr$S,
    si = pr$si,
    totvexp = pr$totvexp,
    pvexp = 0,
    method = 1L,
    force_in = c(3L, 5L)
  )

  testthat::expect_true(fit1$converged)
  testthat::expect_equal(sort(fit1$ind), c(3L, 5L))
  testthat::expect_equal(fit1$card, 2L)
})
