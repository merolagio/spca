#screeplot==========
test_that("screeplot_spca() returns a ggplot object", {
  fit = pca(make_tall_data(), n_comps = 3, qq_plot = FALSE)

  pl = screeplot_spca(fit, nplot = 4, show_plot = FALSE,
                      return_plot = TRUE)

  expect_s3_class(pl, "ggplot")
})

test_that("screeplot_spca() accepts pca objects", {
  fit = pca(make_tall_data(), n_comps = 3, qq_plot = FALSE)
  
  expect_no_error(
    screeplot_spca(fit, show_plot = FALSE)
  )
})

test_that("screeplot_spca() rejects unsupported objects", {
  expect_error(
    screeplot_spca(list(values = 1:3), show_plot = FALSE),
    "no applicable method"
  )
})


#qq-plot==========
test_that("qqplot_spca() returns a ggplot object", {
  fit = pca(make_tall_data(), n_comps = 3, qq_plot = FALSE)

  pl = qqplot_spca(fit, n_vars = nrow(fit$weights),
                   n_obs = nrow(make_tall_data()), nplot = 4,
                   show_plot = FALSE, return_plot = TRUE)

  expect_s3_class(pl, "ggplot")
})

test_that("qqplot_spca() accepts pca objects", {
  X = make_tall_data()
  fit = pca(X, n_comps = 3, qq_plot = FALSE)
  
  expect_no_error(
    qqplot_spca(
      fit,
      n_vars = ncol(X),
      n_obs = nrow(X),
      show_plot = FALSE
    )
  )
})

test_that("qqplot_spca() rejects unsupported objects", {
  expect_error(
    qqplot_spca(
      list(values = 1:3),
      n_vars = 3,
      n_obs = 10,
      show_plot = FALSE
    ),
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
        nplot = 3,
        show_plot = FALSE,
        return_plot = TRUE
      )
    },
    "screeplot_spca"
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
        nplot = 3,
        show_plot = FALSE,
        return_plot = TRUE
      )
    },
    "screeplot_spca"
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
        nplot = 3,
        show_plot = FALSE,
        return_plot = TRUE
      )
    },
    "qqplot_spca"
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
        nplot = 3,
        show_plot = FALSE,
        return_plot = TRUE
      )
    },
    "qqplot_spca"
  )

  expect_s3_class(pl, "ggplot")
})
