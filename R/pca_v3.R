#' Compute Principal Components
#'
#' Creates PCA output as an `spca` object, so the result can be used with
#' `spca` methods.
#'
#' @param M A data matrix, correlation matrix, or covariance matrix.
#' @param ncomps Integer. Number of components to retain. If `NULL`, all
#'   components are retained up to the maximum allowed by the selected backend.
#' @param center_data Logical. If `TRUE`, center variables to zero mean. If `M`
#'   is detected as a data matrix and any column mean is nonzero, centering is
#'   performed automatically.
#' @param scale_data Logical. If `TRUE`, scale variables.
#' @param fat_matrix Logical or `NULL`. If `NULL`, the backend is selected
#'   automatically: data matrices with \eqn{n < p} use the fat backend and all
#'   other inputs use the tall backend. If `TRUE`, request the fat backend. If
#'   `FALSE`, use the tall backend. Covariance/correlation matrices always use
#'   the tall backend.
#' @param screeplot Logical. If `TRUE`, produce a scree plot.
#' @param qq_plot Logical. If `TRUE`, produce a Wachter QQ-plot with
#'   \link{wachter_qqplot}. The fitted line is based on `ncomps`.
#' @param nrow_data Integer. Number of rows in the original data set.
#'   Required when `qq_plot = TRUE` and `M` is a covariance or correlation
#'   matrix. If not available, the Wachter QQ-plot cannot be produced.
#' @param neigen_toplot Integer. Number of eigenvalues to show in diagnostic
#'   plots. If `NULL`, all available eigenvalues are shown.
#' @param PM Logical. If `TRUE`, compute the requested eigenpairs by power
#'   method and rank-one deflation.
#' @param epsPM Numeric. Convergence tolerance for the power method.
#' @param maxiterPM Integer. Maximum number of power-method iterations.
#'
#' @return An \link{spca_object} with the addition of the vector `eigenvalues`
#'   containing the eigenvalues up to the rank used by the selected backend.
#'
#' @details
#' `ncomps` controls how many components are retained in the returned object.
#' The tall backend computes PCA from the covariance/correlation matrix. The fat
#' backend computes PCA in row space and converts the retained eigenvectors back
#' to variable loadings.
#'
#' @examples
#' \dontrun{
#' data(holzinger)
#' ho_pca = pca(holzinger, ncomps = 4, screeplot = TRUE,
#'              nrow_data = 144, qq_plot = TRUE)
#' summary(ho_pca)
#' }
#'
#' @family pca
#' @export
pca = function(M, ncomps = NULL, center_data = FALSE, scale_data = FALSE,
               fat_matrix = NULL,
               screeplot = FALSE, qq_plot = TRUE, nrow_data = NULL,
               neigen_toplot = NULL,
               PM = FALSE, epsPM = 1e-5, maxiterPM = 100){

# validation ==========

  vbool = validate_booleans(center_data = center_data,
                            scale_data = scale_data,
                            screeplot = screeplot,
                            qq_plot = qq_plot,
                            PM = PM)

  if(!vbool){
    stop("wrong input for boolean argument")
  }

  if (!is.null(fat_matrix) &&
      (!is.logical(fat_matrix) || length(fat_matrix) != 1 || is.na(fat_matrix)))
    stop("fat_matrix must be TRUE, FALSE, or NULL")

  if (!is.numeric(epsPM) || length(epsPM) != 1 || is.na(epsPM) || epsPM <= 0)
    stop("epsPM must be a positive scalar")
  if (!is.numeric(maxiterPM) || length(maxiterPM) != 1 || is.na(maxiterPM) || maxiterPM < 1)
    stop("maxiterPM must be a positive scalar")

  if(any(is.na(M)))
    stop("The data matrix cannot contain missing values")

  if (is.data.frame(M))
    M = as.matrix(M)
  if (!is.matrix(M))
    stop("M must be a matrix")

  n = nrow(M)
  p = ncol(M)

  is_datamatrix_M = FALSE
  if ((nrow(M) != p) || !isSymmetric(M)){
    is_datamatrix_M = TRUE
    nrow_data = nrow(M)
  }

# maximum data rank
  if (is_datamatrix_M)
    rank_M = min((nrow(M) - 1), ncol(M))
  else
    rank_M = nrow(M)

  if (is.null(fat_matrix)) {
    fat_matrix = (is_datamatrix_M && (n < p))
    if (fat_matrix)
      warning("fat_matrix = NULL selected the fat backend because M is a data matrix with n < p")
  }

  if(is_datamatrix_M){
    if (!is.null(colnames(M)))
      var_names = colnames(M)
    else
      var_names = paste0("V", seq_len(ncol(M)))
  }
  else {
    if (!is.null(colnames(M)))
      var_names = colnames(M)
    else
      if (!is.null(rownames(M)))
        var_names = rownames(M)
      else
        var_names = paste0("V", seq_len(ncol(M)))
  }

  if (fat_matrix) {
    if (!is_datamatrix_M) {
      warning("fat_matrix = TRUE ignored because the input is a covariance/correlation matrix; using the tall backend")
      use_fat_backend = FALSE
    } else if (n < p) {
      use_fat_backend = TRUE
    } else {
      warning("fat_matrix = TRUE ignored because the data matrix is not fat; using the tall backend")
      use_fat_backend = FALSE
    }
  } else {
    use_fat_backend = FALSE
    if (is_datamatrix_M && (n < p))
      warning("fat_matrix = FALSE forces the tall backend on a fat data matrix; X'X may be singular")
  }

  max_comps = if (use_fat_backend) n else p
  if(is.null(ncomps))
    ncomps = max_comps
  if((ncomps > max_comps) || (ncomps <= 0)){
    warning(paste("incorrect value for ncomps in pca, set to", max_comps))
    ncomps = max_comps
  }

  if (!is.numeric(nrow_data)){
    warning("qq-plot cannot be produced because nrow_data is not available")
    qq_plot = FALSE
  }

  if (is_datamatrix_M) {
# data matrix must be column mean centered
    if (any(abs(colMeans(M)) > 1e-6))
      center_data = TRUE
    if (center_data || scale_data){
      M = scaleC(M, center_data, scale_data)
    }
  }

# computation========

  pcout = pcaC(M = M,
               ncomps = as.integer(ncomps),
               data_matrix = is_datamatrix_M,
               fat_matrix = use_fat_backend,
               PM = PM,
               epsPM = epsPM,
               maxiterPM = as.integer(maxiterPM))

# output   ====
  loadings = pcout$loadings
  rownames(loadings) = var_names
  colnames(loadings) = paste0("PC", seq_len(ncol(loadings)))

  contributions = scaleColsC(loadings[, 1:ncomps, drop = FALSE],
                             1, rep(1, ncomps))
  rownames(contributions) = var_names
  colnames(contributions) = paste0("PC", seq_len(ncol(contributions)))

  loadings_list = lapply(1:ncomps, function(i, x) x[, i],
                         x = loadings[, 1:ncomps, drop = FALSE])

  vexp = pcout$vexpPC

  out = list(loadings = loadings[, 1:ncomps, drop = FALSE],
             contributions = contributions,
             ncomps = ncomps,
             cardinality = rep(p, ncomps),
             vexp = vexp,
             vexpPC = vexp,
             cvexp = cumsum(vexp),
             rvexp = rep(1, ncomps),
             rcvexp = rep(1, ncomps),
             cor_with_pc = rep(1, ncomps),
             tot_var = pcout$totvar,
             loadings_list = loadings_list,
             indices = as.list(rep(list(1:p), ncomps)),
             eigenvalues = pcout$eigenvalues[seq_len(min(rank_M, length(pcout$eigenvalues)))]
  )

  if (is_datamatrix_M){
    out$scores = pcout$scores
    colnames(out$scores) = paste0("Comp", 1:ncomps)
  }
  out$spc_cor = diag(out$ncomps)

  class(out) = c("list", "spca")

# plots =============

  if (screeplot || qq_plot){

    if (is.null(neigen_toplot))
      neigen_toplot = length(pcout$eigenvalues)
    if (neigen_toplot < 2){
      warning(paste("neigen_toplot must be greater than 1, setting it to",
                    length(pcout$eigenvalues)))
      neigen_toplot = length(pcout$eigenvalues)
    }
    if (neigen_toplot > length(pcout$eigenvalues)) {
      warning(paste("neigen_toplot exceeds the number of available eigenvalues; setting it to",
                    length(pcout$eigenvalues)))
      neigen_toplot = length(pcout$eigenvalues)
    }

    if (screeplot == TRUE){
      pl = spca_screeplot(pcout$eigenvalues, n_plot = neigen_toplot,
                          ylab = "eigenvalues")
    }

    if (qq_plot == TRUE){
      pl = wachter_qqplot(pcout$eigenvalues, p = p, n = nrow_data,
                          n_plot = neigen_toplot, n_fitline = -ncomps)
    }
  }
  out$method = "PCA"
  out$Call = match.call()
  return(out)
}

theme_pca = function(base_size = 12, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size,
                         base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      legend.position = "bottom",
      panel.background = element_rect(colour = "black")
    )
}


#' Wachter (Marchenko--Pastur) qq-plot for eigenvalues
#'
#' Produces a QQ-plot comparing observed eigenvalues to Marchenko--Pastur
#' (Wachter) theoretical quantiles for aspect ratio `gamma` = n/p.
#'
#' @param eigvals Numeric vector of eigenvalues (assumed sorted decreasing).
#' @param p Integer. Number of variables.
#' @param n Integer. Sample size.
#' @param gamma Numeric. Aspect ratio; defaults to `n/p` if missing.
#' @param cor Logical. If `TRUE`, rescales MP quantiles to match a correlation
#'   matrix trace (sum to `p`).
#' @param n_plot Integer. Number of leading eigenvalues to include; defaults to
#'   length(eigvals).
#' @param n_fitline Integer or `NULL`. If positive, fits an `lm` line using the
#'   last `n_fitline` points; if negative, excludes the nfit largest values.
#' @param add_title Logical. If `TRUE`, adds a plot title.
#' @param prn Logical. If `TRUE`, prints the plot.
#' @param rtn Logical. If `TRUE`, returns the ggplot object.
#'
#' @return If `rtn = TRUE`, a `ggplot` object; otherwise `NULL` (invisibly).
#'
#'
#' @examples
#' # wachter_qqplot(eigvals, p = ncol(X), n = nrow(X), cor = TRUE, n_fitline = 5, rtn = TRUE)
#'
#' @family pca
#' @export
wachter_qqplot = function(eigvals, p = NULL, n, gamma, cor = T, n_plot = NULL, n_fitline = NULL, add_title = TRUE, prn = TRUE, rtn = FALSE){

  if(is.null(p)) p = length(eigvals)
  if(missing(gamma)) gamma = n/p

  if(is.null(n_plot)) n_plot = length(eigvals)

  probs = ((p - (1:p) + 1)- 0.5)/p
  mp_quantiles = RMTstat::qmp(p = probs, svr = gamma)

  if(cor) mp_quantiles = p * mp_quantiles/sum(mp_quantiles)
#  browser()
  df = data.frame(expected = mp_quantiles[1:n_plot], observed = eigvals[1:n_plot])
  pl = ggplot(df, aes(x = expected, y = observed)) + geom_point(size = 2) + theme_pca()

  if ((is.numeric(n_fitline)) && (n_fitline != 0)){
    if (n_fitline < 0) n_fitline = n_plot + n_fitline

    lmcoef = lm(observed ~ expected, data = df[(n_plot - n_fitline + 1):n_plot, ])$coefficient

    pl = pl + geom_abline(intercept = lmcoef[1], slope = lmcoef[2] ,
                          color = "blue")
    # pl = pl + geom_smooth(data = df[(n_plot - n_fitline + 1):n_plot, ], se = F, method = "lm")
  }
  if(add_title)
    pl = pl + labs(title = "wachter qq-plot") +
    theme(plot.title = element_text(hjust = 0.5))

  if(prn)
    print(pl)
  if(rtn)
    return(pl)
}

#' Scree plot of eigenvalues
#'
#' Plots the first `n_plot` eigenvalues (or their proportions) against component order.
#'
#' @param x A numeric vector of eigenvalues, or a list containing a numeric element named `values`.
#' @param n_plot Integer. Number of leading eigenvalues to plot; defaults to length(x) (or length(x$values) if `x` is a list).
#' @param ylab Character. Y-axis label.
#' @param add_title Logical. If `TRUE`, adds a plot title.
#' @param prn Logical. If `TRUE`, prints the plot.
#' @param rtn Logical. If `TRUE`, returns the ggplot object.
#'
#' @return If `rtn = TRUE`, a `ggplot` object; otherwise `NULL` (invisibly).
#' @family pca
#' @export
spca_screeplot = function(eigvals, n_plot = NULL, ylab = "eigenvalues",
                     add_title = TRUE, prn = TRUE, rtn = FALSE){
  if (!is.vector(eigvals) || any(is.na(eigvals)))
      stop("eigvals must be a vector of eigenvalues and missing values are not allowed")

  if(is.null(n_plot))
    n_plot = length(eigvals)

  df = data.frame(order = 1:n_plot,
                  eigenvalue = eigvals[1:n_plot])
  scree_pl = ggplot(df, aes(x = order, y = eigenvalue)) + geom_point(size = 2) +
    geom_line() + labs(y = ylab) + theme_pca()

  if(add_title) scree_pl = scree_pl + labs(title = "screeplot") +
    theme(plot.title = element_text(hjust = 0.5))

  if(prn)
    print(scree_pl)
  if(rtn)
    return(scree_pl)
}
