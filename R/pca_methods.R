# S3 methods for PCA diagnostic plots
#
# Required changes in pca():
#   out$n_obs = nrow_data
#   class(out) = c("spca_pca", "spca", "list")
#
# Call these methods after assigning the class:
#   screeplot_spca(pca_fit = out, n_plot = neigen_toplot,
#                  ylab = "eigenvalues")
#   qqplot_spca(pca_fit = out, common_var = common_var,
#               n_plot = neigen_toplot, n_fitline = NULL)

#' Wachter QQ Plot for PCA Eigenvalues
#'
#' Produce a QQ plot comparing the eigenvalues of a fitted PCA with
#' Marchenko--Pastur (Wachter) theoretical quantiles.
#'
#' @param pca_fit An object returned by [pca()].
#' @param n_vars An integer scalar or `NULL`. Number of variables. If `NULL`,
#'   obtain it from the number of rows of `pca_fit$weights`.
#' @param n_obs An integer scalar or `NULL`. Number of observations. If `NULL`,
#'   obtain it from `pca_fit$n_obs`.
#' @param gamma A positive numeric scalar or `NULL`. Aspect ratio. If `NULL`,
#'   use `n_obs / n_vars`.
#' @param cor A logical scalar retained for compatibility.
#' @param common_var A positive numeric scalar. Common variance used for the
#'   Marchenko--Pastur quantiles.
#' @param n_plot An integer scalar or `NULL`. Number of leading eigenvalues.
#' @param n_fitline An integer scalar or `NULL`. If positive, fit a line using
#'   the last `n_fitline` points. If negative, exclude the largest
#'   `abs(n_fitline)` points.
#' @param addtitle A logical scalar indicating whether to add a title.
#' @param show_plot A logical scalar indicating whether to print the plot.
#' @param return_plot A logical scalar indicating whether to return the plot.
#'
#' @return If `return_plot = TRUE`, a `ggplot` object; otherwise `NULL`
#'   invisibly.
#' @family pca
#' @export
qqplot_spca = function(
    pca_fit, n_vars = NULL, n_obs = NULL, gamma = NULL, cor = TRUE,
    common_var = 1, n_plot = NULL, n_fitline = NULL, addtitle = TRUE,
    show_plot = TRUE, return_plot = FALSE) {
  UseMethod("qqplot_spca")
}

#' @exportS3Method
#' @noRd
qqplot_spca.spca_pca = function(
    pca_fit, n_vars = NULL, n_obs = NULL, gamma = NULL, cor = TRUE,
    common_var = 1, n_plot = NULL, n_fitline = NULL, addtitle = TRUE,
    show_plot = TRUE, return_plot = FALSE) {

  eigenvalues = pca_fit$eigenvalues

  if (!is.numeric(eigenvalues) || !is.null(dim(eigenvalues)) ||
      length(eigenvalues) < 1L || anyNA(eigenvalues)) {
    stop("`pca_fit$eigenvalues` must be a numeric vector without missing values.",
         call. = FALSE)
  }

  if (is.null(n_vars))
    n_vars = nrow(.get_spca_weights(pca_fit))

  if (is.null(n_vars) || length(n_vars) != 1L || !is.numeric(n_vars) ||
      is.na(n_vars) || n_vars < 1) {
    stop("The number of variables is unavailable; supply a positive `n_vars`.",
         call. = FALSE)
  }

  if (is.null(n_obs))
    n_obs = pca_fit$n_obs

  if (is.null(n_obs)) {
    stop(
      paste0("The number of observations is unavailable in `pca_fit`. ",
             "Supply `n_obs` to produce a Wachter QQ plot."),
      call. = FALSE
    )
  }

  if (length(n_obs) != 1L || !is.numeric(n_obs) || is.na(n_obs) ||
      n_obs < 1) {
    stop("`n_obs` must be a positive numeric scalar.", call. = FALSE)
  }

  if (is.null(gamma))
    gamma = n_obs / n_vars

  if (length(gamma) != 1L || !is.numeric(gamma) || is.na(gamma) ||
      gamma <= 0) {
    stop("`gamma` must be a positive numeric scalar.", call. = FALSE)
  }

  if (length(common_var) != 1L || !is.numeric(common_var) ||
      is.na(common_var) || common_var <= 0) {
    stop("`common_var` must be a positive numeric scalar.", call. = FALSE)
  }

  if (is.null(n_plot))
    n_plot = length(eigenvalues)

  if (length(n_plot) != 1L || !is.numeric(n_plot) || is.na(n_plot) ||
      n_plot < 1L || n_plot > length(eigenvalues)) {
    stop("`n_plot` must be between 1 and the number of eigenvalues.",
         call. = FALSE)
  }
  n_plot = as.integer(n_plot)

  probs = ((n_vars - seq_len(n_vars) + 1) - 0.5) / n_vars
  mp_quantiles = RMTstat::qmp(p = probs, svr = gamma, var = common_var)
  mp_quantiles = n_vars * mp_quantiles / sum(mp_quantiles)

  df = data.frame(
    expected = mp_quantiles[seq_len(n_plot)],
    observed = eigenvalues[seq_len(n_plot)]
  )

  pl = ggplot2::ggplot(df, ggplot2::aes(x = expected, y = observed)) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    theme_pca()

  if (is.numeric(n_fitline) && length(n_fitline) == 1L &&
      !is.na(n_fitline) && n_fitline != 0) {
    if (n_fitline < 0)
      n_fitline = n_plot + n_fitline

    if (n_fitline < 2L || n_fitline > n_plot) {
      stop("`n_fitline` selects fewer than 2 or more than `n_plot` points.",
           call. = FALSE)
    }

    fit_rows = seq.int(n_plot - n_fitline + 1L, n_plot)
    lmcoef = stats::coef(
      stats::lm(observed ~ expected, data = df[fit_rows, ])
    )
    pl = pl + ggplot2::geom_abline(
      intercept = lmcoef[[1L]], slope = lmcoef[[2L]], color = "blue",
      linewidth = 1.15, na.rm = TRUE
    )
  }

  if (addtitle) {
    pl = pl + ggplot2::labs(title = "Wachter QQ-plot") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }

  if (show_plot)
    print(pl)
  if (return_plot)
    return(pl)

  invisible(NULL)
}

#' @exportS3Method
#' @noRd
qqplot_spca.spca = function(
    pca_fit, n_vars = NULL, n_obs = NULL, gamma = NULL, cor = TRUE,
    common_var = 1, n_plot = NULL, n_fitline = NULL, addtitle = TRUE,
    show_plot = TRUE, return_plot = FALSE) {
  stop("`qqplot_spca()` applies only to objects returned by `pca()`.",
       call. = FALSE)
}

#' Plot PCA Eigenvalues in a Screeplot
#'
#' Plot the leading eigenvalues of a fitted PCA against component order.
#'
#' @param pca_fit An object returned by [pca()].
#' @param n_plot An integer scalar or `NULL`. Number of leading eigenvalues.
#' @param ylab A character scalar used as the y-axis label.
#' @param addtitle A logical scalar indicating whether to add a title.
#' @param show_plot A logical scalar indicating whether to print the plot.
#' @param return_plot A logical scalar indicating whether to return the plot.
#'
#' @return If `return_plot = TRUE`, a `ggplot` object; otherwise `NULL`
#'   invisibly.
#' @family pca
#' @export
screeplot_spca = function(
    pca_fit, n_plot = NULL, ylab = "eigenvalues", addtitle = TRUE,
    show_plot = TRUE, return_plot = FALSE) {
  UseMethod("screeplot_spca")
}

#' @exportS3Method
#' @noRd
screeplot_spca.spca_pca = function(
    pca_fit, n_plot = NULL, ylab = "eigenvalues", addtitle = TRUE,
    show_plot = TRUE, return_plot = FALSE) {

  eigenvalues = pca_fit$eigenvalues

  if (!is.numeric(eigenvalues) || !is.null(dim(eigenvalues)) ||
      length(eigenvalues) < 1L || anyNA(eigenvalues)) {
    stop("`pca_fit$eigenvalues` must be a numeric vector without missing values.",
         call. = FALSE)
  }

  if (is.null(n_plot))
    n_plot = length(eigenvalues)

  if (length(n_plot) != 1L || !is.numeric(n_plot) || is.na(n_plot) ||
      n_plot < 1L || n_plot > length(eigenvalues)) {
    stop("`n_plot` must be between 1 and the number of eigenvalues.",
         call. = FALSE)
  }
  n_plot = as.integer(n_plot)

  df = data.frame(
    order = seq_len(n_plot),
    eigenvalue = eigenvalues[seq_len(n_plot)]
  )

  scree_pl = ggplot2::ggplot(
    df,
    ggplot2::aes(x = order, y = eigenvalue)
  ) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(y = ylab) +
    theme_pca()

  if (addtitle) {
    scree_pl = scree_pl + ggplot2::labs(title = "Screeplot") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }

  if (show_plot)
    print(scree_pl)
  if (return_plot)
    return(scree_pl)

  invisible(NULL)
}

#' @exportS3Method
#' @noRd
screeplot_spca.spca = function(
    pca_fit, n_plot = NULL, ylab = "eigenvalues", addtitle = TRUE,
    show_plot = TRUE, return_plot = FALSE) {
  stop("`screeplot_spca()` applies only to objects returned by `pca()`.",
       call. = FALSE)
}

# Obsolete function interfaces retained for backward compatibility ============

#' Wachter QQ Plot for Eigenvalues (Deprecated)
#'
#' `wachter_qqplot()` is retained for backward compatibility. Use
#' [qqplot_spca()] with objects returned by [pca()] in new code.
#'
#' @param eigenvalues A numeric vector of eigenvalues in decreasing order, or
#'   an object returned by [pca()].
#' @param p An integer scalar or `NULL`. Number of variables.
#' @param n An integer scalar. Number of observations.
#' @param gamma A positive numeric scalar. Aspect ratio. If omitted, use
#'   `n / p`.
#' @param cor A logical scalar retained for compatibility.
#' @param common_var A positive numeric scalar. Common variance used for the
#'   Marchenko--Pastur quantiles.
#' @param n_plot An integer scalar or `NULL`. Number of leading eigenvalues.
#' @param n_fitline An integer scalar or `NULL`. If positive, fit a line
#'   using the last `n_fitline` points. If negative, exclude the largest
#'   `abs(n_fitline)` points.
#' @param addtitle A logical scalar indicating whether to add a title.
#' @param show_plot A logical scalar indicating whether to print the plot.
#' @param return_plot A logical scalar indicating whether to return the plot.
#'
#' @return If `return_plot = TRUE`, a `ggplot` object; otherwise `NULL`
#'   invisibly.
#' @family pca
#' @export
wachter_qqplot = function(
    eigenvalues, p = NULL, n, gamma, cor = TRUE, common_var = 1,
    n_plot = NULL, n_fitline = NULL, addtitle = TRUE, show_plot = TRUE,
    return_plot = FALSE) {
  .Deprecated("qqplot_spca")

  if (inherits(eigenvalues, "spca_pca")) {
    if (is.null(p))
      p = nrow(.get_spca_weights(eigenvalues))
    n_value = if (missing(n)) NULL else n
    gamma_value = if (missing(gamma)) NULL else gamma

    return(
      qqplot_spca(
        pca_fit = eigenvalues,
        n_vars = p,
        n_obs = n_value,
        gamma = gamma_value,
        cor = cor,
        common_var = common_var,
        n_plot = n_plot,
        n_fitline = n_fitline,
        addtitle = addtitle,
        show_plot = show_plot,
        return_plot = return_plot
      )
    )
  }

  if (!is.numeric(eigenvalues) || !is.null(dim(eigenvalues)) ||
      length(eigenvalues) < 1L || anyNA(eigenvalues)) {
    stop("eigenvalues must be a numeric vector without missing values",
         call. = FALSE)
  }

  if (is.null(p))
    p = length(eigenvalues)
  if (length(p) != 1L || !is.numeric(p) || is.na(p) || p < 1)
    stop("p must be a positive numeric scalar", call. = FALSE)

  if (missing(gamma)) {
    if (missing(n) || length(n) != 1L || !is.numeric(n) ||
        is.na(n) || n < 1) {
      stop("n must be a positive numeric scalar when gamma is omitted",
           call. = FALSE)
    }
    gamma = n / p
  }
  if (length(gamma) != 1L || !is.numeric(gamma) || is.na(gamma) ||
      gamma <= 0)
    stop("gamma must be a positive numeric scalar", call. = FALSE)

  if (length(common_var) != 1L || !is.numeric(common_var) ||
      is.na(common_var) || common_var <= 0)
    stop("common_var must be a positive numeric scalar", call. = FALSE)

  if (is.null(n_plot))
    n_plot = length(eigenvalues)
  if (length(n_plot) != 1L || !is.numeric(n_plot) || is.na(n_plot) ||
      n_plot < 1L || n_plot > length(eigenvalues)) {
    stop("n_plot must be between 1 and the number of eigenvalues",
         call. = FALSE)
  }
  n_plot = as.integer(n_plot)

  probs = ((p - seq_len(p) + 1) - 0.5) / p
  mp_quantiles = RMTstat::qmp(p = probs, svr = gamma, var = common_var)
  mp_quantiles = p * mp_quantiles / sum(mp_quantiles)

  df = data.frame(
    expected = mp_quantiles[seq_len(n_plot)],
    observed = eigenvalues[seq_len(n_plot)]
  )
  pl = ggplot2::ggplot(df, ggplot2::aes(x = expected, y = observed)) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    theme_pca()

  if (is.numeric(n_fitline) && length(n_fitline) == 1L &&
      !is.na(n_fitline) && n_fitline != 0) {
    if (n_fitline < 0)
      n_fitline = n_plot + n_fitline
    if (n_fitline < 2L || n_fitline > n_plot) {
      stop("n_fitline selects fewer than 2 or more than n_plot points",
           call. = FALSE)
    }
    fit_rows = seq.int(n_plot - n_fitline + 1L, n_plot)
    lmcoef = stats::coef(
      stats::lm(observed ~ expected, data = df[fit_rows, ])
    )
    pl = pl + ggplot2::geom_abline(
      intercept = lmcoef[[1L]], slope = lmcoef[[2L]], color = "blue",
      linewidth = 1.15, na.rm = TRUE
    )
  }

  if (addtitle) {
    pl = pl + ggplot2::labs(title = "Wachter QQ-plot") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  if (show_plot)
    print(pl)
  if (return_plot)
    return(pl)
  invisible(NULL)
}

#' Plot Eigenvalues in a Scree Plot (Deprecated)
#'
#' `spca_screeplot()` is retained for backward compatibility. Use
#' [screeplot_spca()] with objects returned by [pca()] in new code.
#'
#' @param eigenvalues A numeric vector of eigenvalues, or an object returned
#'   by [pca()].
#' @param n_plot An integer scalar or `NULL`. Number of leading eigenvalues.
#' @param ylab A character scalar used as the y-axis label.
#' @param addtitle A logical scalar indicating whether to add a title.
#' @param show_plot A logical scalar indicating whether to print the plot.
#' @param return_plot A logical scalar indicating whether to return the plot.
#'
#' @return If `return_plot = TRUE`, a `ggplot` object; otherwise `NULL`
#'   invisibly.
#' @family pca
#' @export
spca_screeplot = function(
    eigenvalues, n_plot = NULL, ylab = "eigenvalues", addtitle = TRUE,
    show_plot = TRUE, return_plot = FALSE) {
  .Deprecated("screeplot_spca")

  if (inherits(eigenvalues, "spca_pca")) {
    return(
      screeplot_spca(
        pca_fit = eigenvalues,
        n_plot = n_plot,
        ylab = ylab,
        addtitle = addtitle,
        show_plot = show_plot,
        return_plot = return_plot
      )
    )
  }

  if (!is.numeric(eigenvalues) || !is.null(dim(eigenvalues)) ||
      length(eigenvalues) < 1L || anyNA(eigenvalues)) {
    stop("eigenvalues must be a numeric vector without missing values",
         call. = FALSE)
  }

  if (is.null(n_plot))
    n_plot = length(eigenvalues)
  if (length(n_plot) != 1L || !is.numeric(n_plot) || is.na(n_plot) ||
      n_plot < 1L || n_plot > length(eigenvalues)) {
    stop("n_plot must be between 1 and the number of eigenvalues",
         call. = FALSE)
  }
  n_plot = as.integer(n_plot)

  df = data.frame(
    order = seq_len(n_plot),
    eigenvalue = eigenvalues[seq_len(n_plot)]
  )
  scree_pl = ggplot2::ggplot(
    df,
    ggplot2::aes(x = order, y = eigenvalue)
  ) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(y = ylab) +
    theme_pca()

  if (addtitle) {
    scree_pl = scree_pl + ggplot2::labs(title = "Screeplot") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  if (show_plot)
    print(scree_pl)
  if (return_plot)
    return(scree_pl)
  invisible(NULL)
}
