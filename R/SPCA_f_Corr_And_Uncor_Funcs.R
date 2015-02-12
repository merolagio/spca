fcspca = function(SS, S, nc = 1, vexp = FALSE){
  ## returns the lsspca for PP = SZ[ind,]* SZ[,ind], and D = S[ind, ind]
  ## SZ deflated matrix, S undeflated
  ## way faster with chol
  ## funzeca anche con singolo indice
  ## ADDED geninv Fast algo for  GENeralid INVerse COMP
  #####---------------------------------------------------------
  
  #   no = sample(10:30,1)
  #   ind = sort(sample(1:50, no))
  #   S = S[ind, ind]
  #   SS = SS[ind, ind]
  
  if (is.null(dim(S))){
    loadings = as.matrix(1)
    nc = 1
  }
  else{    
    p = ncol(S)
    G = chol(S, pivot = TRUE)
    rank =  attr(G, "rank")
    pivot <- attr(G, "pivot")
    #    G = as(G, "dtrMatrix")
    ##   =========================== INVERSE ========
    if (rank < p)
      R = geninv(G, isChol = TRUE)
    else{ # use triangular for fast solve then sort rows(!) 
      R = backsolve(G, diag(p))
      R = R[order(pivot), ]
    }
    ##  ==============================================
    
    ##   =========================== COMPUTES ========
    aa = eigen(crossprod(R, SS)  %*%  R, symm = TRUE)
    loadings = as.matrix(R %*% aa$vec[,1:nc]  )
    csm <- colSums(loadings^2)
    #csm = colSums((G %*% aa$vec[,1:nc])^2)
    if (nc == 1)
      sig = sign(loadings[1]) 
    else 
      sig = sign(loadings[1, ]) 
    loadings = t(t(loadings)/ (sig * sqrt(csm)))
  }  
  ### the  
  if (vexp == FALSE)
    return(loadings)  
  else{
    if (is.null(dim(S)))
      eval = SS/S 
    else
      eval = aa$val[1:nc]/sum(diag(S))
  }
  #     vexpns = vexp/drop(sqrt(diag(crossprod(R %*% loadings))))
  #     vexpns = vexp/drop((diag(crossprod(R %*% loadings))))
  
  return( list(loadings = loadings, eval = eval) )#, csm = csm) )#, vexpns = vexpns) ) 
}

fuspca = function(SS, S, R, nc = 1, vexp = FALSE){
  ## returns the correlated lsspca for 
  ## PP = S[ind,]* S[,ind], and D = S[ind, ind], R = A'S[,ind]
  ## SZ deflated matrix, S undeflated
  ## way faster with chol
  ## funzeca anche con singolo indice
  ## ADDED geninv Fast algo for  GENeralid INVerse COMP
  #####---------------------------------------------------------
  
  
  if(missing(R)){
    fcspca(SS, S, nc = nc, vexp = vexp)
  }
  else{
    if (is.null(dim(S))){
      loadings = as.matrix(1)
      nc = 1
    }
    else{    
      p = ncol(S)
      Dp = geninv(S, isChol = FALSE)
      #Dp = MASS::ginv(S)
      K = tcrossprod(Dp, R)
      M = R %*% K
      if (ncol(M) == 1){        
        M = Dp - tcrossprod(K, K)/drop(M)
      }
      else{
        #        P = geninv(M, isChol = FALSE)
        P = solve(M)
        # M = MASS::ginv(M)        
        M = Dp - tcrossprod(K %*% P, K)
      }
      
      ## geigen computes generalied eigenvecs     
      aa =  geigen(M, SS, nc, vexp = vexp)
      loadings = aa$vec[, 1:nc, drop = FALSE]
      csm <- colSums(loadings^2)
      #csm = colSums((G %*% aa$vec[,1:nc])^2)
      if (nc == 1)
        sig = sign(loadings[1]) 
      else 
        sig = sign(loadings[1, ]) 
      loadings = t(t(loadings)/ (sig * sqrt(csm)))
    }  
    ### the  
    if (vexp == FALSE)
      return(loadings)  
    else{
      if (is.null(dim(S)))
        eval = SS/(S)
      else
        eval = aa$val[1:nc]
      
    }
    #     vexpns = vexp/drop(sqrt(diag(crossprod(R %*% loadings))))
    #     vexpns = vexp/drop((diag(crossprod(R %*% loadings))))
    
    return( list(loadings = loadings, eval = eval) )#, csm = csm) )#, vexpns = vexpns) ) 
  }
}

#' Computes sparse principal components solutions
#' 
#' Computes LS SPCA sparse principal components loadings for a given set of
#' indices. the fspca is faster than spca, which does the same thing.\cr See the package vignettes for details.
#' 
#' The number of components to compute is determind from the length of
#' \emph{ind}.  If \emph{unc} has fewer elements than the number of indices
#' passed, the remaining elements are set equal to the last one.
#' 
#' @param S A correlation or covariance matrix.
#' @param ind A list of indices for each dimension. The number of dimensions to
#' compute is determined by its length. If only the first dimension is
#' required, it can be a vector.
#' @param unc A logical vector indicating which components should be should be
#' computed uncorrelated to the preceeding ones. Can be shorter than the number
#' of dimensions to compute. See details.
#' @return An object of class \emph{spca} is returned. It is the smallest
#' instance of an spca object, which contains: \item{loadings}{The matrix of
#' loadings} \item{contributions}{Matrix of loadings scaled to unit \eqn{L_1}
#' norm.} \item{vexpv}{a vector of variances explained by each component}
#' \item{vexp}{a vector of variances explained by each PC} In addition, if
#' \code{any unc[j] = FALSE}: \item{corComp}{The matrix with correlations among
#' components.} \item{loadingsUnc}{Loadings of the components made
#' uncorrelated.}
#' @details The fspca function uses more efficient Choleski decomposition and have been 
#' optimised following \emph{R Inferno} and \emph{Advanced R}. These should give an 
#' improvement of more than 20% in speed. Generalised inverses are computed with Couret algorithm. 
#' @seealso \code{\link{spca}}, \code{\link{spcabb}}, \code{\link{spcabe}}, \code{\link{summary.spca}}
#' @keywords LS SPCA
#' @examples
#'    \dontrun{ 
#'    data(anthrop, package = "spca")
#'    # for uncorrelated components
#' 	 myspca <- fspca(anthrop, ind = list(1:2, 3:7))
#' 	 ## print loadings
#' 	 myspca
#' 	 ## print summaries
#' 	 summary(myspca)
#' 	 # for correlated components
#' 	 myspcac <- fspca(anthrop, ind = list(1:2, 3:7), unc = FALSE)
#' 	 myspcac
#' 	 summary(myspcac)
#' 	 ## print correlation between components
#' 	 myspcac$corComp
#' 	 ## print loadings of components made uncorrelated
#' 	 myspcac$loadingsUnc
#' 	 ## compare the two results numerically and graphically
#' 	compare(myspca, myspcac, methodsnames = c("Unc", "Cor"), shortnamescomp = FALSE) 
#'    }
#'  
#' @export fspca
fspca = function(S, ind, unc = TRUE){
  
  ##==============================================================  
  ## runs SPCA-LS for a given set of indices
  ##==============================================================
  
  if(missing(ind))
    stop("need to give a list of indices in spca. if not known, use spcabe or spcabb")
  namess = colnames(S)
  S = unname(S)
  p = ncol(S)
  SS = S %*% S
  totv = sum(diag(S))
  if (!is.list(ind)){
    nd = 1
    cr = fcspca(SS[ind, ind], S[ind, ind], vexp = TRUE)
    A = matrix(rep(0, p), ncol = 1)
    A[ind,] = cr$loadings
    fvexp = cr$eval/totv
  }
  else{
    nd = length(ind)
    if ( length(unc) < nd ){
      le = length(unc)
      nm = nd - le
      unc = c(unc, rep(unc[le], nm))
    }
    A = matrix(0, p, nd)
    fvexp = rep(0, p)
    if (any(unc == FALSE))
      Z = diag(p)
    
    for (j in 1:nd){    
      if (j == 1){#j = 1
        if (length(ind[[j]]) == 1){
          fvexp[1] = SS[ind[[1]], ind[[1]]]/(S[ind[[1]], ind[[1]]] * totv)
          A[ind[[j]],1] = 1
        }
        else{            
          # old          cr = fcspca(S,ind[[j]], vexpn = TRUE)
          cr = fcspca(SS[ind[[j]], ind[[j]]], S[ind[[j]], ind[[j]]], vexp = TRUE)
          A[ind[[j]],j] = cr$loadings
          fvexp[j] = cr$eval/totv
        }    
      }# end j = 1
      else{## j > 1
        #j = 2
        if (any(unc[j:nd] == FALSE))
          Z = spca:::makez(A[,j-1, drop = FALSE], S, Z)
        if (unc[j] == TRUE){
          R = crossprod(A[, 1:(j-1)], S[, ind[[j]] ])
          cr = fuspca(SS = SS[ind[[j]], ind[[j]]], S = S[ind[[j]], ind[[j]]], 
                      nc = 1,   R = R, vexp = TRUE)
        }
        else{
          SSZ = crossprod(Z[, ind[[j]]],  SS %*% Z[, ind[[j]]])
          cr = fcspca(SSZ, S[ind[[j]], ind[[j]]],  vexp = TRUE)
        }          
        fvexp[j] = cr$eval/totv
        A[ind[[j]], j] = cr$loadings
        
      }
      
    }
  }## end j > 1
  e = eigen(S, only.values = TRUE, symmetric = TRUE)$val
  vexp = e[1:nd] / totv
  vexpv = fmakevexp(A, S)
  rownames(A) = namess
  contributions = sweep(A, 2, colSums(abs(A)), "/") 
  if (all(unc==FALSE))
    unc = FALSE
  if (all(unc==TRUE))
    unc = TRUE  
  out = list(loadings = A, contributions = contributions, vexp = vexpv, vexpPC = vexp, fvexp= fvexp, 
             unc = unc, ind = ind)
  if (nd > 1){
    if (any(unc == FALSE)){
      out$corComp = spca:::make.cor(S, A)
      out$Uncloadings = make.uncLoad2(A, S)
    }
  }
  class(out) = "spca"
  return(out)
}
