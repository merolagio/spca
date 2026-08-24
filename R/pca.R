#' Computes Principal Components
#'
#' Compute a principal component analysis (PCA) and return the result as an
#' \code{spca} object, so that it can be used with \code{spca} methods.
#'
#' @param M A data matrix, correlation matrix, or covariance matrix.
#' @param n_comps An integer scalar or \code{NULL} (default \code{NULL}).
#'   Number of components to retain. If \code{NULL}, all components are retained
#'   up to the maximum allowed by the selected backend.
#' @param center_data A logical value (default \code{FALSE}). If \code{TRUE},
#'   center variables to zero mean. If \code{M} is detected as a data matrix and
#'   any column mean is nonzero, centering is performed automatically.
#' @param scale_data A logical value (default \code{FALSE}). If \code{TRUE},
#'   scale variables.
#' @param fat_matrix A logical value or \code{NULL} (default \code{NULL}). If
#'   \code{NULL}, the backend is selected automatically: data matrices with
#'   \eqn{n < p} use the fat backend and all other inputs use the tall backend.
#'   If \code{TRUE}, request the fat backend. If \code{FALSE}, use the tall
#'   backend. Covariance and correlation matrices always use the tall backend.
#' @param screeplot A logical value (default \code{FALSE}). If \code{TRUE},
#'   produce a scree plot.
#' @param qq_plot A logical value (default \code{TRUE}). If \code{TRUE},
#'   produce a Wachter QQ plot with \code{\link{qqplot_spca}}.
#' @param nrow_data An integer scalar or \code{NULL} (default \code{NULL}).
#'   Number of rows in the original data set. Required when
#'   \code{qq_plot = TRUE} and \code{M} is a covariance or correlation
#'   matrix. If not available, the Wachter QQ-plot cannot be produced.
#' @param neigen_toplot An integer scalar or \code{NULL} (default
#'   \code{NULL}). Number of eigenvalues to show in diagnostic plots. If
#'   \code{NULL}, all available eigenvalues are shown.
#' @param cor A logical value (default \code{TRUE}). Currently accepted for
#'   compatibility; the diagnostic plot uses \code{common_var} for the
#'   Marchenko--Pastur quantiles.
#' @param common_var A numeric scalar (default \code{1}). Common variance of
#'   the variables used for the Marchenko--Pastur quantiles in the Wachter QQ
#'   plot.
#' @param pm A logical value (default \code{FALSE}). If \code{TRUE}, compute
#'   the requested eigenpairs by power method and rank-one deflation.
#' @param eps_pm A positive numeric scalar (default \code{1e-4}). Convergence
#'   tolerance for the power method.
#' @param maxiter_pm A positive integer scalar (default \code{1000}). Maximum
#'   number of power-method iterations.
#'
#' @return An \code{\link{spca_object}} with an additional
#'   \code{eigenvalues} vector containing the eigenvalues up to the rank used by
#'   the selected backend and \code{n_obs} stores the number of observations,  
#'   if a data matrix is passed, or NULL.
#'
#' @details \code{n_comps} controls how many components are retained in the
#' returned object. The tall backend computes PCA from the covariance or
#' correlation matrix. The fat backend computes PCA in row space and converts
#' the retained eigenvectors back to variable weights.
#'
#' @examples
#' data(holzinger)
#' ho_pca = pca(holzinger, n_comps = 4, screeplot = TRUE,
#'              nrow_data = 144, qq_plot = TRUE)
#' summary(ho_pca)
#' @family pca
#' @export
pca = function(M, n_comps = NULL, center_data = FALSE, scale_data = FALSE,
               fat_matrix = NULL, screeplot = FALSE, qq_plot = TRUE,
               nrow_data = NULL, neigen_toplot = NULL, cor = TRUE, 
               common_var = 1, pm = FALSE, eps_pm = 1e-4, maxiter_pm = 1000) {
  
  # validation ==========
  
  vbool = validate_booleans(center_data = center_data,
                            scale_data = scale_data,
                            screeplot = screeplot,
                            qq_plot = qq_plot,
                            pm = pm)
  
  if (!vbool) {
    stop("wrong input for boolean argument")
  }
  
  if (!is.null(fat_matrix) &&
      (!is.logical(fat_matrix) || length(fat_matrix) != 1 || is.na(fat_matrix)))
    stop("fat_matrix must be TRUE, FALSE, or NULL")
  
  if (!is.numeric(eps_pm) || length(eps_pm) != 1 || 
      is.na(eps_pm) || eps_pm <= 0)
    stop("eps_pm must be a positive scalar")
  if (!is.numeric(maxiter_pm) || length(maxiter_pm) != 1 || 
      is.na(maxiter_pm) || maxiter_pm < 1)
    stop("maxiter_pm must be a positive scalar")
  
  if (any(is.na(M)))
    stop("The data matrix cannot contain missing values")
  
  if (is.data.frame(M))
    M = as.matrix(M)
  if (!is.matrix(M))
    stop("M must be a matrix")
  
  n = nrow(M)
  p = ncol(M)
  
  is_datamatrix_M = FALSE
  if ((nrow(M) != p) || !isSymmetric(M)) {
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
      warning("fat_matrix = NULL selected the fat backend because M is 
              a data matrix with n < p")
  }
  
  if (is_datamatrix_M) {
    if (!is.null(colnames(M)))
      var_names = colnames(M)
    else
      var_names = paste0("V", seq_len(ncol(M)))
  } else {
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
      warning("fat_matrix = TRUE ignored because the input is a covariance/
              correlation matrix; using the tall backend")
      use_fat_backend = FALSE
    } else if (n < p) {
      use_fat_backend = TRUE
    } else {
      warning("fat_matrix = TRUE ignored because the data matrix is not fat;
              using the tall backend")
      use_fat_backend = FALSE
    }
  } else {
    use_fat_backend = FALSE
    if (is_datamatrix_M && (n < p))
      warning("fat_matrix = FALSE forces the tall backend on a fat data matrix;
              X'X may be singular")
  }
  
  max_comps = if (use_fat_backend) n else p
  if (is.null(n_comps))
    n_comps = max_comps
  if ((n_comps > max_comps) || (n_comps <= 0)) {
    warning(paste("incorrect value for n_comps in pca, set to", max_comps))
    n_comps = max_comps
  }
  
  if (!is.numeric(nrow_data) && qq_plot) {
    warning("qq-plot cannot be produced because nrow_data is not available")
    qq_plot = FALSE
  }
  
  if (is_datamatrix_M) {
    # data matrix must be column mean centered
    if (any(abs(colMeans(M)) > 1e-6))
      center_data = TRUE
    if (center_data || scale_data) {
      M = standardize_data(M, center_data, scale_data)
    }
  }
  
  # computation========
  
  pcout = pcaC(M = M,
               ncomps = as.integer(n_comps),
               data_matrix = is_datamatrix_M,
               fat_matrix = use_fat_backend,
               PM = pm,
               epsPM = eps_pm,
               maxiterPM = as.integer(maxiter_pm))
  
  # output   ====
  weights = pcout$weights
  rownames(weights) = var_names
  colnames(weights) = paste0("PC", seq_len(ncol(weights)))
  
  contributions = scale_columns(weights[, 1:n_comps, drop = FALSE],
                                1, rep(1, n_comps))
  rownames(contributions) = var_names
  colnames(contributions) = paste0("PC", seq_len(ncol(contributions)))
  
  weights_list = lapply(1:n_comps, function(i, x) x[, i],
                         x = weights[, 1:n_comps, drop = FALSE])
  
  vexp = pcout$vexpPC
  
  out = list(weights = weights[, 1:n_comps, drop = FALSE],
             contributions = contributions,
             n_comps = n_comps,
             cardinality = rep(p, n_comps),
             vexp = vexp,
             vexp_pc = vexp,
             cvexp = cumsum(vexp),
             rvexp = rep(1, n_comps),
             rcvexp = rep(1, n_comps),
             cor_with_pc= rep(1, n_comps),
             tot_var = pcout$totvar,
             weights_list = weights_list,
             indices = as.list(rep(list(1:p), n_comps)),
             eigenvalues = pcout$eigenvalues[
               seq_len(min(rank_M, length(pcout$eigenvalues)))],
             n_obs = if (is_datamatrix_M) nrow_data else NULL
  )
  if (is_datamatrix_M) {
    out$scores = pcout$scores
    colnames(out$scores) = paste0("Comp", 1:n_comps)
  }
  out$spc_cor = diag(out$n_comps)
  
  class(out) = c("spca_pca", "spca", "list")
  
  # plots =============
  
  if (screeplot || qq_plot) {
    
    if (is.null(neigen_toplot))
      neigen_toplot = length(out$eigenvalues)
    if (neigen_toplot < 2) {
      warning(paste("neigen_toplot must be greater than 1, setting it to",
                    length(out$eigenvalues)))
      neigen_toplot = length(out$eigenvalues)
    }
    if (neigen_toplot > length(out$eigenvalues)) {
      warning(paste("neigen_toplot exceeds the number of available eigenvalues;
                    setting it to",
                    length(out$eigenvalues)))
      neigen_toplot = length(out$eigenvalues)
    }
    
    if (screeplot == TRUE) {
      
      pl = screeplot_spca(out, nplot = neigen_toplot, 
                          ylab = "eigenvalues")
    }
    if (qq_plot == TRUE) {
      pl = qqplot_spca(out, n_vars = p, n_obs = nrow_data, 
                          common_var = common_var, nplot = neigen_toplot,
                          n_fitline = NULL
      )
    }
  }
  out$method = "PCA"
  out$call = match.call()
  out
}

#' Theme for PCA Diagnostic Plots
#'
#' Return the ggplot2 theme used by the PCA diagnostic plots.
#'
#' @param base_size A numeric scalar (default \code{12}). Base font size.
#' @param base_family A character scalar (default \code{""}). Base font family.
#'
#' @return A ggplot2 theme object.
#' @noRd
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

