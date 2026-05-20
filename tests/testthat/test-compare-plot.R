test_that("plot.spca() returns ggplot objects for main plot types", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)

  p1 = plot(fit, n_plot = 2, plot_type = "bars", show_plot = FALSE,
            return_plot = TRUE)
  p2 = plot(fit, n_plot = 2, plot_type = "heatmap", show_plot = FALSE,
            return_plot = TRUE)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("compare_spca() returns tables and plot on request", {
  X = make_tall_data()
  fit1 = spca(X, n_comps = 2, method = "cspca", fat_matrix = FALSE)
  fit2 = spca(X, n_comps = 2, method = "uspca", fat_matrix = FALSE)

  out = compare_spca(
    list(fit1, fit2),
    n_comps = 2,
    plot_loadings = TRUE,
    show_plot = FALSE,
    return_plot = TRUE,
    return_tables = TRUE,
    print_tables = FALSE,
    print_loadings = FALSE
  )

  expect_type(out, "list")
  expect_true(length(out) >= 1)
})

test_that("aggregate_by_group() returns a table", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)
  groups = rep(c("A", "B"), length.out = nrow(fit$loadings))

  tab = aggregate_by_group(fit, groups = groups, print_table = FALSE,
                           return_table = TRUE)

  expect_true(is.matrix(tab) || is.data.frame(tab))
  expect_equal(nrow(tab), length(unique(groups)))
})
