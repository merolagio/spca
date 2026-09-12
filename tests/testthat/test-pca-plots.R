#screeplot==========
test_that("screeplot_pca() returns a ggplot object", {
  fit = pca(make_tall_data(), n_comps = 3, qq_plot = FALSE)

  pl = screeplot_pca(fit, n_plot = 4, show_plot = FALSE,
                      return_plot = TRUE)

  expect_s3_class(pl, "ggplot")
})

test_that("screeplot_pca() accepts pca objects", {
  fit = pca(make_tall_data(), n_comps = 3, qq_plot = FALSE)

  expect_no_error(
    screeplot_pca(fit, show_plot = FALSE)
  )
})

test_that("screeplot_pca() rejects unsupported objects", {
  expect_error(
    screeplot_pca(list(values = 1:3), show_plot = FALSE),
    "no applicable method"
  )
})

test_that("screeplot_pca() rejects spca() fits", {
  fit = spca(make_tall_data(), n_comps = 2, method = "cspca",
             var_selection = "fwd", objective = "cvexp", fat_matrix = FALSE)

  expect_error(
    screeplot_pca(fit, show_plot = FALSE),
    "no applicable method")
})


#qq-plot==========
test_that("mp_qqplot() returns a ggplot object", {
  fit = pca(make_tall_data(), n_comps = 3, qq_plot = FALSE)

  pl = mp_qqplot(fit, n_vars = nrow(fit$weights),
                   n_obs = nrow(make_tall_data()), n_plot = 4,
                   show_plot = FALSE, return_plot = TRUE)

  expect_s3_class(pl, "ggplot")
})

test_that("mp_qqplot() accepts pca objects", {
  X = make_tall_data()
  fit = pca(X, n_comps = 3, qq_plot = FALSE)

  expect_no_error(
    mp_qqplot(
      fit,
      n_vars = ncol(X),
      n_obs = nrow(X),
      show_plot = FALSE))
})

test_that("mp_qqplot() rejects unsupported objects", {
  expect_error(
    mp_qqplot(
      list(values = 1:3),
      n_vars = 3,
      n_obs = 10,
      show_plot = FALSE),
    "no applicable method"
  )
})

test_that("mp_qqplot() rejects spca() fits", {
  fit = spca(make_tall_data(), n_comps = 2, method = "cspca",
             var_selection = "fwd", objective = "cvexp", fat_matrix = FALSE)

  expect_error(
    mp_qqplot(fit, n_vars = 3, n_obs = 10, show_plot = FALSE),
    "no applicable method"
  )
})


#obsolete plot functions==========
test_that("spca_screeplot() preserves the eigenvalue-vector interface", {
  pl = NULL
  expect_warning(
    {
      pl = spca_screeplot(
        eigenvalues = c(4, 3, 2, 1),
        n_plot = 3,
        show_plot = FALSE,
        return_plot = TRUE
      )
    },
    "screeplot_pca"
  )

  expect_s3_class(pl, "ggplot")
  expect_equal(nrow(pl$data), 3)
})

test_that("spca_screeplot() accepts pca objects", {
  fit = pca(make_tall_data(), n_comps = 3, qq_plot = FALSE)

  pl = NULL
  expect_warning(
    {
      pl = spca_screeplot(
        fit,
        n_plot = 3,
        show_plot = FALSE,
        return_plot = TRUE
      )
    },
    "screeplot_pca"
  )

  expect_s3_class(pl, "ggplot")
})

test_that("wachter_qqplot() preserves the eigenvalue-vector interface", {
  pl = NULL
  expect_warning(
    {
      pl = wachter_qqplot(
        eigenvalues = c(4, 3, 2, 1),
        p = 4,
        n = 20,
        n_plot = 3,
        show_plot = FALSE,
        return_plot = TRUE
      )
    },
    "mp_qqplot"
  )

  expect_s3_class(pl, "ggplot")
  expect_equal(nrow(pl$data), 3)
})

test_that("wachter_qqplot() accepts obsolete pca objects with loadings", {
  X = make_tall_data()
  legacy = pca(X, n_comps = 3, qq_plot = FALSE)
  legacy$loadings = legacy$weights
  legacy$weights = NULL
  legacy$loadings_list = legacy$weights_list
  legacy$weights_list = NULL

  pl = NULL
  expect_warning(
    {
      pl = wachter_qqplot(
        legacy,
        n = nrow(X),
        n_plot = 3,
        show_plot = FALSE,
        return_plot = TRUE
      )
    },
    "'wachter_qqplot'"
  )

  expect_s3_class(pl, "ggplot")
})
