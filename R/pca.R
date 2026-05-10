#' Compute Principal Components
#'
#' Creates PCA output as an `spca` object, so the result can be used with
#' `spca` methods.
#'
#' @param M A data matrix, correlation matrix, or covariance matrix.
#' @param ncomps Integer. Number of components to retain. If `NULL`, all
#'   components are retained.
#' @param centerdata Logical. If `TRUE`, center variables to zero mean. If `M`
#'   is detected as a data matrix and any column mean is nonzero, centering is
#'   performed automatically.
#' @param scaledata Logical. If `TRUE`, scale variables.
#' @param screeplot Logical. If `TRUE`, produce a scree plot.
#' @param wachter Logical. If `TRUE`, produce a Wachter QQ-plot with
#'   \link{wachter_qqplot}. The fitted line is based on `ncomps`.
#' @param nrow_data Integer. Number of rows in the original data set. 
#'   Required when `wachter = TRUE` and `M` is a covariance or correlation
#'    matrix. If not available, the Wachter QQ-plot cannot be produced.
#' @param neigen_toplot Integer. Number of eigenvalues to show in diagnostic
#'   plots. If `NULL`, all eigenvalues are shown.
#'
#' @return An  \link{spca_object} with the addition of the vector `eigenvalues` which contains all the eigenvalues of $S$, useful for future plots or computations. 
#' 
#' @details
#' `ncomps` controls how many components are retained in the returned object.
#' Setting it below the maximum does not reduce the cost of the eigendecomposition.
#'
#' @examples
#' \dontrun{
#' data(holzinger)
#' ho_pca <- pca(hom, ncomps = 4, screeplot = TRUE, nrow_data = 144, wachter = TRUE)
#' summary(ho_pca)
#' }
#'
#' @family pca
#' @export
pca = function(M, ncomps = NULL, centerdata = FALSE, scaledata = FALSE,
                screeplot= FALSE,  wachter = TRUE, nrow_data = NULL,
               neigen_toplot = NULL){
  
# validation ==========  


  vbool = validate_booleans(centerdata = centerdata, scaledata = scaledata, screeplot = screeplot, wachter =  wachter)
  
  if(!vbool){
    stop("wrong input for boolean argument")
}
    if(any(is.na(M)))
    stop("The data matrix cannot contain missing values")
  
  if (is.data.frame(M))
    M = as.matrix(M)
  if (!is.matrix(M))
    stop("M must be a matrix")
  
  p = ncol(M)
  
  if(is.null(ncomps))
    ncomps = p
  if((ncomps > p) || (ncomps <= 0)){
    warning(paste("incorrect value for ncomps in pca, set to), p"))
    ncomps = p
    }
  

  is_datamatrix_M = FALSE
  if ((nrow(M) != p) || !isSymmetric(M)){
    is_datamatrix_M = TRUE
    nrow_data = nrow(M)

# data matrix must be column mean centered  
  if (any(abs(colMeans(M)) > 1e-6))
    centerdata = TRUE
  if (centerdata || scaledata){
      M = scaleC(M, centerdata, scaledata)
    }
    S = ataC(M)
  } 
  else{
    S = M
  }

  if(is.null(rownames(M)))
    nam = paste0("V", seq(p))
  else
    nam = rownames(M)
  
  if (is.null(neigen_toplot))
    neigen_toplot = p
  if (neigen_toplot < 2){
    warning(paste("neigen_toplot must be greater than 1, setting it to", p))
      neigen_toplot = p
  }

  if (!is.numeric(nrow_data)){
      warning("qq-wachter plot cannot be produced beacuse nrow_data is not available")
      wachter = FALSE
    }

# computation========
  
    ee = EigenC(as.matrix(S))
    totvar = sum(ee$val)
  

# output   ==== 
    rownames(ee$vec) = nam
    colnames(ee$vec) = paste0("PC", 1:p)
    
    contributions = scaleColsC(ee$vec[,1:ncomps, drop = FALSE], 
                               1, rep(1, ncomps))
    rownames(contributions) = nam
    
    loadlist = lapply(1:ncomps, function(i, x) x[, i], x = ee$vec[, 1:ncomps])
    
    vexp = ee$val[1:ncomps]/totvar
  
  out = list(loadings = ee$vec[, 1:ncomps, drop = FALSE], 
             contributions = contributions,
             ncomps = ncomps, cardinality = rep(p, ncomps), 
             cardinality = rep(p, ncomps),
             vexp = vexp, 
             vexpPC = vexp, 
             cvexp = cumsum(vexp),
             rvexp = rep(1, ncomps), 
             rcvexp = rep(1, ncomps),
             cor_with_pc = rep(1, ncomps),
             total_variance = totvar,
             loadlist = loadlist, 
             indices = as.list(rep(list(1:p), ncomps)), 
             eigenvalues = ee$val[1:ncomps]
  )
  if (is_datamatrix_M){
    out$scores = M %*% out$loadings[, 1:ncomps, drop = FALSE]
    colnames(out$scores) = paste0("Comp", 1:ncomps)
  }
    out$corComp = diag(out$ncomps)

  class(out) = c("list", "spca")

# plots =============
  if (screeplot == TRUE){
    pl = spca_screeplot(ee$val, nplot = neigen_toplot, ylab = "eigenvalues")
    
  }
  #browser()
  if (wachter == TRUE){
    pl = wachter_qqplot(ee$val, p = p, n = nrow_data, nplot = neigen_toplot, n_fitline = -ncomps
    )
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
#' @param cor Logical. If `TRUE`,rescales MP quantiles to match a
#' correlation
#'   matrix trace (sum to `p`).
#' @param nplot Integer. Number of leading eigenvalues to include; defaults to
#'   length(eigvals)`.
#' @param n_fitline Integer or `NULL`. If positive, fits an `lm` line using the
#'   last `n_fitline` points; if negative, excludes the nfit largest values.
#' @param addtitle Logical. If `TRUE`, adds a plot title.
#' @param prn Logical. If `TRUE`, prints the plot.
#' @param rtn Logical. If `TRUE`, returns the ggplot object.
#'
#' @return If `rtn = TRUE', a `ggplot` object; otherwise `NULL` (invisibly).
#'
#'
#' @examples
#' # wachter_qqplot(eigvals, p = ncol(X), n = nrow(X), cor = TRUE, n_fitline = 5, rtn = TRUE)
#'
#' @family pca 
#' @export
wachter_qqplot = function(eigvals, p = NULL, n, gamma, cor = T, nplot = NULL, n_fitline = NULL, addtitle = TRUE, prn = TRUE, rtn = FALSE){
  
  if(is.null(p)) p =length(eigvals)
  if(missing(gamma)) gamma = n/p
  
  if(is.null(nplot)) nplot = length(eigvals)
  
  probs = ((p - (1:p) + 1)- 0.5)/p
  mp_quantiles = RMTstat::qmp(p = probs, svr = gamma)
  
  if(cor) mp_quantiles = p * mp_quantiles/sum(mp_quantiles)
#  browser()
  df = data.frame(expected = mp_quantiles[1:nplot], observed = eigvals[1:nplot])
  pl = ggplot(df, aes(x = expected, y = observed)) + geom_point(size = 2) + theme_pca()
  
  if ((is.numeric(n_fitline)) && (n_fitline != 0)){
    if (n_fitline < 0) n_fitline = nplot + n_fitline 
    
    lmcoef = lm(observed ~ expected, data = df[(nplot - n_fitline + 1):nplot, ])$coefficient
    
    pl = pl + geom_abline(intercept = lmcoef[1], slope = lmcoef[2] , 
                          color = "blue")
    # pl = pl + geom_smooth(data = df[(nplot - n_fitline + 1):nplot, ], se = F, method = "lm")
  }
  if(addtitle)
    pl = pl + labs(title = "wachter qq-plot") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  if(prn)
    print(pl)
  if(rtn)
    return(pl)
}

#' Scree plot of eigenvalues
#'
#' Plots the first `nplot` eigenvalues (or their proportions) against component order.
#'
#' @param x A numeric vector of eigenvalues, or a list containing a numeric element named `values`.
#' @param nplot Integer. Number of leading eigenvalues to plot; defaults to length(x)` (or length(x$values)` if `x` is a list).
#' @param ylab Character. Y-axis label.
#' @param addtitle Logical. If `TRUE`, adds a plot title.
#' @param prn Logical. If `TRUE`, prints the plot.
#' @param rtn Logical. If `TRUE`, returns the ggplot object.
#'
#' @return If `rtn = TRUE`, a `ggplot` object; otherwise `NULL` (invisibly).
#' @family pca
#' @export
spca_screeplot = function(eigvals, nplot = NULL, ylab = "eigenvalues", 
                     addtitle = TRUE, prn = TRUE, rtn = FALSE){
  if (!is.vector(eigvals) || any(is.na(eigvals)))     
      stop("eigvals must be a vector of eigenvalues and missing values are not allowed")
  
  if(is.null(nplot))
    nplot = length(eigvals)
  
  df = data.frame(order = 1:nplot,
                  eigenvalue = eigvals[1:nplot])
  scree_pl = ggplot(df, aes(x = order, y = eigenvalue)) + geom_point(size = 2) +
    geom_line() + labs(y = ylab) + theme_pca()

  if(addtitle) scree_pl = scree_pl + labs(title = "screeplot") +
    theme(plot.title = element_text(hjust = 0.5))
  
  if(prn)
    print(scree_pl)
  if(rtn)
    return(scree_pl)
}
