test_that("spca_screeplot() returns a ggplot object", {
  fit = pca(make_tall_data(), n_comps = 3, qq_plot = FALSE)

  pl = spca_screeplot(fit$eigenvalues, nplot = 4, show_plot = FALSE,
                      return_plot = TRUE)

  expect_s3_class(pl, "ggplot")
})

test_that("wachter_qqplot() returns a ggplot object", {
  fit = pca(make_tall_data(), n_comps = 3, qq_plot = FALSE)

  pl = wachter_qqplot(fit$eigenvalues, p = nrow(fit$loadings),
                      n = nrow(make_tall_data()), nplot = 4,
                      show_plot = FALSE, return_plot = TRUE)

  expect_s3_class(pl, "ggplot")
})
