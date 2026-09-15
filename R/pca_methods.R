
#' Test for PCA Objects
#'
#' Check whether an object has class \code{pca} and contains the core
#' elements produced by \code{pca()}.
#'
#' @param x An object to test.
#'
#' @details Performs a lightweight check of the class and the presence of
#' core elements. It does not validate their values or dimensions.
#' Scores and the number of observations are not required because they
#' may be unavailable for covariance-matrix input.
#'
#' @return A logical value: \code{TRUE} if the required class and core
#' elements are present, and \code{FALSE} otherwise.
#'
#' @examples
#' data(holzinger)
#' ho_pca = pca(holzinger, n_comps = 2, screeplot = FALSE, qq_plot = FALSE)
#' is.pca(ho_pca)
#'
#' @family pca
#' @export
is.pca = function(x) {
  inherits(x, "pca") &&
    is.list(x) &&
    !is.null(x$weights) &&
    !is.null(x$contributions) &&
    !is.null(x$vexp) &&
    !is.null(x$vexp_pc) &&
    !is.null(x$cvexp) &&
    !is.null(x$rvexp) &&
    !is.null(x$rcvexp) &&
    !is.null(x$n_comps) &&
    !is.null(x$cardinality) &&
    !is.null(x$weights_list) &&
    !is.null(x$indices) &&
    !is.null(x$eigenvalues) &&
    !is.null(x$cor_with_pc) &&
    !is.null(x$tot_var) &&
    !is.null(x$spc_cor)
}

#' S3 Generic for PCA Diagnostic qqplot
#' @export
#' @noRd
mp_qqplot = function(
    pca_fit, n_vars = NULL, n_obs = NULL, gamma = NULL, cor = TRUE,
    common_var = 1, n_plot = NULL, n_fitline = NULL, addtitle = TRUE,
    show_plot = TRUE, return_plot = FALSE) {
  UseMethod("mp_qqplot")
}

#' S3 Method for PCA Diagnostic qqplot
#'
#' Produce a qq-plot comparing the eigenvalues of a fitted PCA with
#' Marchenko--Pastur theoretical quantiles.
#'
#' This diagnostic relies on Marchenko--Pastur theory for the null
#' eigenvalue distribution of a (possibly high-dimensional) sample
#' covariance matrix, and is therefore only meaningful for objects of
#' class `"pca"` (i.e. objects returned by [pca()]). It does not apply
#' to sparse fits returned by [spca()] (class `"spca"`), for which the
#' reference distribution does not hold; no method is defined for that
#' class, so calling it on such an object raises the standard
#' "no applicable method" error rather than producing a plot.
#'
#' @param pca_fit An object of class `"pca"`, as returned by [pca()].
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
#' @details
#' The Marchenko-Pastur distribution depends on the data aspect ratio $p/n$.
#'  Therefore both `n_vars` and `n_obs` must be available to produce the plot.
#'  The distribution is applicable to the eigenvalues of the sample covariance 
#'  matrix of a set of variables with equal variance. For sample correlation
#'  matrix, the quantiles are scaled to have sum equal to `p`.
#' @examples
#' data(holzinger)
#' ho_pca = pca(holzinger)
#' # from the screeplot we may choose to retain 4 components
#' # produce a Wachter qqplot fitting a line to all but the largest 
#' # 4 eigenvalues. 
#' # Since pca was fitted using the data matrix, the fit carries the number 
#' # of observations. Otherwise we would need to pass `n_obs` .
#' mp_qqplot(ho_pca, n_fitline = -4)
#' # The qq-plot indicates that the 4th eigenvalue is compatible 
#' # with that of a random matrix. 
#' 
#' @return If `return_plot = TRUE`, a `ggplot` object; otherwise `NULL`
#'   invisibly.
#' @family pca
#' @exportS3Method
mp_qqplot.pca = function(
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
             "Supply `n_obs` to produce a Wachter qq-plot."),
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

#' S3 Generic for Plotting PCA Eigenvalues in a Screeplot
#' @export
#' @noRd
scree_plot = function(
    pca_fit, n_plot = NULL, ylab = "eigenvalues", addtitle = TRUE,
    show_plot = TRUE, return_plot = FALSE) {
  UseMethod("scree_plot")
}

#' S3 Method for Plotting PCA Eigenvalues in a Screeplot
#'
#' Plot the leading eigenvalues of a fitted PCA against component order.
#'
#' This diagnostic applies only to objects of class `"pca"` (i.e. objects
#' returned by [pca()]). It does not apply to sparse fits returned by
#' [spca()] (class `"spca"`); no method is defined for that class, so
#' calling it on such an object raises the standard "no applicable
#' method" error rather than producing a plot.
#'
#' @param pca_fit An object of class `"pca"`, as returned by [pca()].
#' @param n_plot An integer scalar or `NULL`. Number of leading eigenvalues.
#' @param ylab A character scalar used as the y-axis label.
#' @param addtitle A logical scalar indicating whether to add a title.
#' @param show_plot A logical scalar indicating whether to print the plot.
#' @param return_plot A logical scalar indicating whether to return the plot.
#'
#' @examples
#' data(holzinger)
#' ho_pca = pca(holzinger, screeplot = FALSE)
#' # the screeplot can be produced from `pca` directly.
#' # It can be customized by saving it as a `ggplot` object 
#' myscreeplot = scree_plot(ho_pca, return_plot = TRUE)
#' myscreeplot  + ggplot2::geom_point(color = "red")
#' @return If `return_plot = TRUE`, a `ggplot` object; otherwise `NULL`
#'   invisibly.
#' @family pca 
#' @exportS3Method
scree_plot.pca = function(
    pca_fit, n_plot = NULL, ylab = "eigenvalues", addtitle = TRUE,
    show_plot = TRUE, return_plot = FALSE) {

  eigenvalues = pca_fit$eigenvalues

  if (!is.numeric(eigenvalues) || !is.null(dim(eigenvalues)) ||
      length(eigenvalues) < 1L || anyNA(eigenvalues)) {
    stop("`pca_fit$eigenvalues` must be a numeric vector without missing 
         values.",
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
    ggplot2::aes(x = order, y = eigenvalue)) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(y = ylab) +
    theme_pca() +
    scale_x_continuous(
      breaks = function(limits) {
        b <- scales::breaks_pretty(n = 6)(limits)
        b[abs(b - round(b)) < 1e-8]
      },
      minor_breaks = NULL
    )

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

# Obsolete function interfaces retained for backward compatibility ============

#' Wachter QQ Plot for Eigenvalues (Deprecated)
#'
#' `wachter_qqplot()` is retained for backward compatibility. Use
#' [mp_qqplot()] with objects returned by [pca()] in new code.
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
#' @keywords internal
#' @export
wachter_qqplot = function(
    eigenvalues, p = NULL, n, gamma, cor = TRUE, common_var = 1,
    n_plot = NULL, n_fitline = NULL, addtitle = TRUE, show_plot = TRUE,
    return_plot = FALSE) {
  .Deprecated("mp_qqplot")

  if (inherits(eigenvalues, "pca")) {
    if (is.null(p))
      p = nrow(.get_spca_weights(eigenvalues))
    n_value = if (missing(n)) NULL else n
    gamma_value = if (missing(gamma)) NULL else gamma

    return(
      mp_qqplot(
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
#' [scree_plot()] with objects returned by [pca()] in new code.
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
#' @keywords internal
#' @export
spca_screeplot = function(
    eigenvalues, n_plot = NULL, ylab = "eigenvalues", addtitle = TRUE,
    show_plot = TRUE, return_plot = FALSE) {
  .Deprecated("scree_plot")

  if (inherits(eigenvalues, "pca")) {
    return(
      scree_plot(
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
