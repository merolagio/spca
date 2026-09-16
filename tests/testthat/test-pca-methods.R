# Copy into spca/tests/testthat/test-pca-methods.R.
# Uses make_tall_data() from the existing helper-data.R.
# Run from the package project: devtools::test(filter = "pca-methods")

make_pca_method_fit = function() {
  pca(make_tall_data(), n_comps = 3, fat_matrix = FALSE,
      screeplot = FALSE, qq_plot = FALSE)
}

test_that("PCA fits are recognized and wrapper methods are registered", {
  fit = make_pca_method_fit()
  expect_true(is.pca(fit))
  expect_false(is.pca(list()))
  expect_false(is.pca(NULL))
  for (generic in c("print", "plot", "summary", "show_weights",
                    "change_sign", "aggregate_by_group")) {
    expect_true(is.function(getS3method(generic, "pca", optional = TRUE)),
                info = generic)
  }
})

test_that("print.pca forwards selected columns and custom names", {
  fit = make_pca_method_fit()
  tab = NULL
  capture.output({
    tab = print(fit, cols = c(1, 3), contributions = FALSE,
                only_nonzero = FALSE, component_names = c("First", "Third"),
                return_table = TRUE)
  })
  expect_true(is.matrix(tab))
  expect_identical(colnames(tab), c("First", "Third"))
  expect_equal(nrow(tab), nrow(fit$weights) + 2L)
})

test_that("summary.pca forwards options and returns selected metrics", {
  fit = make_pca_method_fit()
  tab = summary(object = fit, cols = 2, min_weight = TRUE,
                print_table = FALSE, return_table = TRUE)
  expect_true(is.matrix(tab))
  expect_equal(ncol(tab), 2L)
  expect_true("Min cont" %in% rownames(tab))
  expect_equal(as.numeric(tab["Vexp", ]), fit$vexp[1:2])
})

test_that("plot.pca returns renderable bars, circular plots and heatmaps", {
  fit = make_pca_method_fit()
  for (kind in c("bars", "circular", "heatmap")) {
    pl = plot(fit, n_plot = 2, plot_type = kind,
              only_nonzero = FALSE, show_plot = FALSE, return_plot = TRUE)
    expect_s3_class(pl, "ggplot")
    expect_no_error(ggplot2::ggplotGrob(pl))
  }
  bars = plot(fit, n_plot = 2, plot_type = "b", contributions = FALSE,
              only_nonzero = FALSE, show_plot = FALSE, return_plot = TRUE)
  expect_equal(bars$data$value, c(fit$weights[, 1:2]))
})

test_that("plot.pca accepts comparison data frames and variable names", {
  fit = make_pca_method_fit()
  labels = paste0("Variable", seq_len(nrow(fit$weights)))
  pl = plot(fit, n_plot = 2, contributions = FALSE,
            pc_weights = as.data.frame(fit$weights[, 1:2]),
            show_plot = FALSE, return_plot = TRUE,
            controls = list(variable_names = labels))
  expect_identical(levels(pl$data$variable), labels)
  expect_equal(pl$data$value, rep(c(fit$weights[, 1:2]), 2))
  expect_no_error(ggplot2::ggplotGrob(pl))
})

test_that("show_weights.pca forwards selected weights and contributions", {
  fit = make_pca_method_fit()
  values = show_weights(fit, cols = c(1, 3), contribution = FALSE,
                        print_list = FALSE, return_list = TRUE)
  expect_equal(values, fit$weights_list[c(1, 3)])
  contributions = show_weights(fit, cols = 1, print_list = FALSE,
                               return_list = TRUE)
  expect_equal(contributions,
               fit$weights_list[[1]] / sum(abs(fit$weights_list[[1]])))
})

test_that("show_weights.pca accepts the documented object argument", {
  fit = make_pca_method_fit()
  expect_equal(
    show_weights(object = fit, print_list = FALSE, return_list = TRUE),
    show_weights(fit, print_list = FALSE, return_list = TRUE)
  )
})

test_that("change_sign.pca updates weights and scores and preserves class", {
  fit = make_pca_method_fit()
  changed = change_sign(object = fit, index_to_change = 1)
  expect_identical(class(changed), class(fit))
  expect_true(is.pca(changed))
  expect_equal(changed$weights[, 1], -fit$weights[, 1])
  expect_equal(changed$weights[, 2:3], fit$weights[, 2:3])
  expect_equal(changed$contributions[, 1], -fit$contributions[, 1])
  expect_equal(changed$weights_list[[1]], -fit$weights_list[[1]])
  expect_equal(changed$scores[, 1], -fit$scores[, 1])
  expect_equal(change_sign(changed, 1), fit)
})

test_that("aggregate_by_group.pca returns group sums", {
  fit = make_pca_method_fit()
  groups = rep(c("A", "B"), length.out = nrow(fit$weights))
  tab = aggregate_by_group(fit, groups = groups, contributions = FALSE,
                           only_nonzero = FALSE, print_table = FALSE,
                           return_table = TRUE)
  expect_equal(tab, rowsum(fit$weights, groups, reorder = FALSE))
})

test_that("aggregate_by_group.pca accepts the documented object argument", {
  fit = make_pca_method_fit()
  groups = rep(c("A", "B"), length.out = nrow(fit$weights))
  tab = aggregate_by_group(object = fit, groups = groups,
                           only_nonzero = FALSE, print_table = FALSE,
                           return_table = TRUE)
  expect_equal(tab, rowsum(fit$contributions, groups, reorder = FALSE))
})

test_that("PCA wrappers preserve the deprecated spca_obj argument", {
  fit = make_pca_method_fit()
  changed = NULL
  expect_warning({
    changed = change_sign(spca_obj = fit, index_to_change = 1)
  }, "deprecated")
  expect_equal(changed$weights[, 1], -fit$weights[, 1])
  values = NULL
  expect_warning({
    values = show_weights(spca_obj = fit, contribution = FALSE,
                          print_list = FALSE, return_list = TRUE)
  }, "deprecated")
  expect_equal(values, fit$weights_list)
})
