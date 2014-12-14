#' Computes principal components solutions
#' 
#' Computes PCA components loadings.
#' 
#' \emph{nd} is just the number of components retained from the full eigen
#' decomposition, doesn't speed up the function. \emph{only.values} does not
#' compute the loadings and is more efficient. Kaiser rule determines the
#' number of components as the number of eigenvalues larger than one. It should
#' be used only for correlation matrices, if called on a covariance matrix a
#' warning is generated.
#' 
#' @param S A correlation or covariance matrix.
#' @param nd Integer: number of loadings to retain. If missing all loadings are
#' retained.
#' @param only.values Logical: should only the eigenvalues be computed?
#' @param screeplot Logical: should the screeplot be plotted?
#' @param kaiser.print Logical: should the kaiser rule be computed, printed and
#' returned?.
#' @return An object of class \emph{spca} is returned, which contains:
#' \item{loadings}{The matrix of loadings (if only.values = TRUE it is equal to
#' NULL).} \item{vexpv}{a vector of variances explained by each PC}
#' \item{vexp}{a vector of variances explained by each PC} \item{cvexp}{a
#' vector of cumulative variances explained by the PCs} In addition, if
#' \code{kaiser.print = TRUE}: \item{kaiser}{The number of eigenvalues larger
#' than one.}
#' @seealso See also \code{\link{print.spca}, \link{summary.spca}}
#' @examples
#'  \dontrun{ 
#' 	 data(anthrop, package = "spca")
#' 	 # computes 4 PCs loadings plotting the screeplot and printing the kaiser rule
#' 	 mypca <- pca(anthrop, nd = 4, screeplot = TRUE, kaiser.print = TRUE)
#' 	 ## print loadings
#' 	 mypca
#' 	 summary(mypca)
#'  }
#'  
#' @export pca
pca = function(S, nd, only.values = FALSE, screeplot= FALSE, kaiser.print = FALSE){
  #######=============================================================  
  ## computes the PCA loadings with vexp, cumulative vexp and eigenvalues
  #######=============================================================  
  if (!is.matrix(S) & !is.data.frame(S))
    stop("S must be a matrix")
  p = ncol(S)
  if ((nrow(S) != p) | t(S[p,p]) != S[p,p])
    stop("S must be a symmetric ")
  
  ee = eigen(S, symmetric = TRUE, only.values = only.values)
  if (any(ee$val < -10^(-5)))
    stop("S must be a covariance or correlation matrix ")
  
  if(missing(nd))
    nd = p
  if (only.values == FALSE){
    ee$vec = t(t(ee$vec) * sign(ee$vec[1,]))
    rownames(ee$vec) = colnames(S)
  }
  else
    ee$vec = NULL
  vexp = ee$val[1:nd]/sum(ee$val)
  out = list(loadings = ee$vec[,1:nd], vexp = vexp, cvexp = cumsum(ee$val[1:nd])/sum(ee$val)
             , vexpPC = vexp, val = ee$val, ind = as.list(rep(list(1:p), nd)))
  class(out) = "spca"
  old.par <- par(no.readonly = TRUE) 

  if (screeplot == TRUE){
    par(mar = c(4, 4, 1, 1))
    plot(1:p, ee$val, xlab = "component", ylab = "eigenvalue", type = "b", pch = 16, xaxt = "n")
    axis(side = 1, at = 1:p)
  }
  if (kaiser.print){
    if (!all(diag(S) == 1))
      warning("kaiser rule should be used only for correlation matrices")
    kaiser =   sum(ee$val >1.0)
    out$kaiser = kaiser
    print(paste("number of eigenvalues larger than 1 is", kaiser ))
      
  }
  return(out)
  on.exit(par(old.par))
  
}
