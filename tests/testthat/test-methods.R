test_that("is.spca() detects valid and invalid objects", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)

  expect_true(is.spca(fit))
  expect_false(is.spca(list()))
  expect_false(is.spca(NULL))
})

test_that("legacy spca fields remain supported", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)
  legacy = fit
  legacy$loadings = legacy$weights
  legacy$weights = NULL
  legacy$loadings_list = legacy$weights_list
  legacy$weights_list = NULL

  expect_true(is.spca(legacy))
  expect_true(validate_spca(legacy, quiet = TRUE))
  expect_equal(.get_spca_weights(legacy), fit$weights)
  expect_equal(.get_spca_weights_list(legacy), fit$weights_list)
  expect_equal(.get_spca_loadings(legacy), fit$weights)
  expect_equal(.get_spca_loadings_list(legacy), fit$weights_list)

  changed = NULL
  expect_warning(
    {
      changed = change_weights_sign_spca(legacy, 1)
    },
    "change_sign"
  )
  expect_equal(changed$loadings[, 1], -legacy$loadings[, 1])
  expect_equal(changed$loadings_list[[1]], -legacy$loadings_list[[1]])
})
#########################
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

##################

test_that("print.spca() can return a table", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)

  tab = print(fit, return_table = TRUE)

  expect_true(is.matrix(tab) || is.data.frame(tab))
  expect_true(ncol(tab) >= 1)
})

test_that("summary.spca() can return a table", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)

  tab = summary(fit, cols = 2, min_weight = TRUE,
                print_table = FALSE, return_table = TRUE)

  expect_true(is.matrix(tab) || is.data.frame(tab))
  expect_true(ncol(tab) >= 1)
  expect_true("Min cont" %in% rownames(tab))
})

test_that("change_sign() changes the requested component sign", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)
  old_weights = fit$weights[, 1]

  changed = change_sign(
    object = fit,
    index_to_change = 1
  )

  expect_spca_object(changed, n_comps = 2, has_scores = TRUE)
  expect_equal(changed$weights[, 1], -old_weights)
  expect_equal(changed$weights[, 2], fit$weights[, 2])
})

test_that("legacy change_loadings_sign_spca() alias remains supported", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)

  changed = NULL
  expect_warning(
    {
      changed = change_loadings_sign_spca(fit, 1)
    },
    "change_sign"
  )

  expect_equal(changed$weights[, 1], -fit$weights[, 1])
})

test_that("show_weights() returns a list on request", {
  fit = spca(make_tall_data(), n_comps = 2, fat_matrix = FALSE)

  cont = show_weights(
    object = fit,
    print_list = FALSE,
    return_list = TRUE
  )

  expect_type(cont, "list")
  expect_equal(length(cont), fit$n_comps)
})

test_that("show_correlations() prints and returns both correlation matrices", {
  fit = spca(make_tall_data(), n_comps = 3, fat_matrix = FALSE)
  result = NULL
  
  expect_output(
    {
      result = show_correlations(fit, return_matrices = TRUE)
    },
    "sPC-PC"
  )
  
  expect_type(result, "list")
  expect_named(
    result,
    c("spc_correlations", "spc_pc_correlations")
  )
  expect_true(is.matrix(result$spc_correlations))
  expect_true(is.numeric(result$spc_correlations))
  expect_true(is.matrix(result$spc_pc_correlations))
  expect_true(is.numeric(result$spc_pc_correlations))
  expect_equal(result$spc_correlations, fit$spc_cor)
  expect_equal(
    as.numeric(result$spc_pc_correlations),
    as.numeric(fit$cor_with_pc)
  )
  expect_identical(rownames(result$spc_pc_correlations), "sPC-PC")
  expect_identical(
    colnames(result$spc_pc_correlations),
    paste0("PC", seq_len(fit$n_comps))
  )
})

test_that("show_correlations() handles abbreviated types and missing fields", {
  fit = spca(make_tall_data(), n_comps = 3, fat_matrix = FALSE)
  
  spc_result = show_correlations(
    fit, type = "s", print_matrices = FALSE, return_matrices = TRUE
  )
  pc_result = show_correlations(
    fit, type = "p", print_matrices = FALSE, return_matrices = TRUE
  )
  
  expect_equal(spc_result, fit$spc_cor)
  expect_equal(as.numeric(pc_result), as.numeric(fit$cor_with_pc))
  
  without_spc_cor = fit
  without_spc_cor$spc_cor = NULL
  reconstructed = show_correlations(
    without_spc_cor, type = "s", print_matrices = FALSE,
    return_matrices = TRUE
  )
  expect_equal(reconstructed, stats::cor(fit$scores))
  
  without_pc_cor = fit
  without_pc_cor$cor_with_pc = NULL
  expect_error(
    show_correlations(
      without_pc_cor, type = "p", print_matrices = FALSE
    ),
    "not available"
  )
})
