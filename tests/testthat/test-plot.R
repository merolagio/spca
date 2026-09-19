#TEST plot()===================================
test_that("plot.spca returns plots that can be rendered", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)
  
  bars = plot(fit, plot_type = "bars", contributions = FALSE,
              only_nonzero = FALSE,
              show_plot = FALSE, return_plot = TRUE)
  
  circular = plot(fit, plot_type = "circular",
                  show_plot = FALSE, return_plot = TRUE)
  
  heatmap = plot(fit, plot_type = "heatmap",
                 show_plot = FALSE, return_plot = TRUE)
  
  expect_equal(bars$data$value, c(fit$weights))
  
  for (pl in list(bars, circular, heatmap)) {
    expect_s3_class(pl, "ggplot")
    expect_no_error(ggplot2::ggplotGrob(pl))
  }
})

test_that("plot.spca supports automatic and custom variable names", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)
  labels = paste0("Variable", seq_len(nrow(fit$weights)))
  rownames(fit$weights) = labels
  
  automatic = plot(
    fit, only_nonzero = FALSE,
    show_plot = FALSE, return_plot = TRUE,
    controls = list(variable_names = "auto")
  )
  
  custom = plot(
    fit, only_nonzero = FALSE,
    show_plot = FALSE, return_plot = TRUE,
    controls = list(variable_names = rev(labels))
  )
  
  expect_identical(levels(automatic$data$variable), labels)
  expect_identical(levels(custom$data$variable), rev(labels))
  expect_s3_class(automatic$theme$axis.text.x, "element_text")
  expect_s3_class(custom$theme$axis.text.x, "element_text")
})

test_that("bar labels can be suppressed and auto handles missing names", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)
  
  hidden = plot(
    fit, show_plot = FALSE, return_plot = TRUE,
    controls = list(variable_names = "none")
  )
  expect_s3_class(hidden$theme$axis.text.x, "element_blank")
  
  rownames(fit$weights) = NULL
  
  automatic = plot(
    fit, only_nonzero = FALSE,
    show_plot = FALSE, return_plot = TRUE,
    controls = list(variable_names = "auto")
  )
  
  expect_identical(
    levels(automatic$data$variable),
    paste0("V", seq_len(nrow(fit$weights)))
  )
  expect_s3_class(automatic$theme$axis.text.x, "element_blank")
})

test_that("printsafe supports comparison weights", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)
  
  pl = plot(
    fit, pc_weights = diag(8)[, 1:2], contributions = FALSE,
    show_plot = FALSE, return_plot = TRUE,
    controls = list(color_scale = "printsafe")
  )
  
  expect_s3_class(pl, "ggplot")
  expect_equal(
    pl$data$value,
    c(fit$weights, diag(8)[, 1:2])
  )
  expect_no_error(ggplot2::ggplotGrob(pl))
})
test_that("plot.spca() returns ggplot objects for main plot types", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)
  
  p1 = plot(fit, n_plot = 2, plot_type = "bars", show_plot = FALSE,
            return_plot = TRUE)
  p2 = plot(fit, n_plot = 2, plot_type = "heatmap", show_plot = FALSE,
            return_plot = TRUE)
  
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

