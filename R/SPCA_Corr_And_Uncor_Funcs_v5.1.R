cspca = function(S, ind, Z = NULL, vexpn = FALSE){
  ## computes CSPCA for one component
  ## V2 uses mult eigen
  #calling ginv directly from MASS, so won't load it
  p = ncol(S)
  if (!is.null(Z))
    Sz = S %*% Z
  else
    Sz = S
  D = S[ind, ind] 
  sv = svd(D, nu = 0)
  nv = sum(sv$d > 1E-6)
  ## Dm = D^1/2. don't use prod.eigen this is already efficient
  Dm = t(t(sv$v[,1:nv]) / sqrt(sv$d[1:nv])) %*% t(sv$v[,1:nv])

  ## eigen J D^1/2 (eigvec(D^1/2 Sz Sz D^1/2)) like in paper
  ee = eigen(crossprod(Sz[,ind] %*% Dm), symmetric = TRUE)
  av = Dm  %*% ee$vec[,1]
  
  av = av/ sqrt(sum(av^2))
  av = sign(av[1]) * av
  a = rep(0,p)
  a[ind] = av
  if (vexpn == TRUE)
    vexp = drop(crossprod(Sz[,ind] %*% av))/ (drop(t(av) %*% S[ind, ind] %*% av) * sum(diag(S)))
  else
    vexp = drop(crossprod(Sz[,ind] %*% av))/ (drop(t(av) %*% Sz[ind, ind] %*% av) * sum(diag(S)))
  return(list(a = a, vexp = vexp))
}

uspca = function(S, ind, A = NULL, onlyone = FALSE){  
  ## v 2.0 new version more efficient
  ## this version computes uncorrelated spca-ls loadings as from paper 
  ##  from matrix  (CD)^1/2 (JSSJ) (CD)^1/2 seems better than the version using Z. 
  ## uses mult.eigen for V D V'
  ## Note: removed argument Z from previous version. book was an argument no longer used
  
  #  computes one uspca component
  # onlyone = TRUE means cardinality = 1
  ## was constrRegOpt

  p = ncol(S)
  if(length(ind) == 1)
     onlyone = TRUE
  if (onlyone == TRUE){
    a = rep(0, p)
    a[ind] = 1
    vexp = sum((S[,ind])^2)/ (S[ind,ind] * sum(diag(S)))
  }
  else{    
    D = S[ind,ind]
    #require(MASS)
    sv = eigen(D, symmetric = TRUE)
    sv$val[ sv$val < 0 ] = 0 
    nv = sum(sv$val > 1E-6)
    
    if (rcond(D) > 1E-5)  
      Dp = solve(S[ind, ind]) 
    else
      Dp = MASS::ginv(S[ind, ind]) 
    
    
    if (is.null(A)){
      
      ## this is the solution on paper
      Dm = mult.eigen(sv$vec, abs(sv$val), ind = 1:nv, power = -0.5)
      #      Dm = sv$vec[,1:nv] %*% diag(sqrt(sv$val[1:nv])^-1) %*% t(sv$vec[,1:nv])#t(t(sv$v[,1:nv]) / sqrt(sv$d[1:nv])) %*% t(sv$v[,1:nv])
      ee = eigen(crossprod( S[ ,ind] %*% Dm), symmetric = TRUE)
      
      av = Dm %*% ee$ve[, 1]
      av = av / sqrt(sum(av^2))
    }
    else{    
      
      A = as.matrix(A)
      R = crossprod(A, S[,ind])
      
      H = R %*% Dp %*% t(R)
      if (rcond(H) > 1E-4){        
        C = (diag(length(ind)) -  Dp %*% t(R) %*% solve(H) %*% R)            
      }
      else{
        C = (diag(length(ind)) -  Dp %*% t(R) %*% MASS::ginv(H) %*%  R)  
      }
      if (sum(abs(C)) < 1E-03){
        print("cannot find uncorrelated solution computing correlated")
        C = diag(length(ind))# - Dm)
      }
      
      ## put this in paper and use this
      CDp = C %*% Dp
      cdp.ee = eigen(CDp, symmetric = T)
      #        indcd = which(cdp.ee$val > 1E-10)
      ## cacca
      ## use this, way more efficient
      cdph = t(cdp.ee$vec %*% (t(cdp.ee$vec) * abs(cdp.ee$val)^0.5))
      
      #         if (length(indcd) == 1)
      #           cdph = outer(cdp.ee$vec[,indcd], cdp.ee$vec[,indcd])*sqrt(cdp.ee$val[indcd])
      #         else  
      #           cdph = cdp.ee$vec[,indcd] %*% diag(cdp.ee$val[indcd])^0.5 %*% t(cdp.ee$vec[,indcd])
      
      b = eigen(cdph %*% S[ind,] %*% S[ ,ind] %*% cdph, symmetric = TRUE)$ve[,1]
       av = cdph %*% b        
      av = av / sqrt(sum(av^2))
    }    
    
  }  
  av = sign(av[1]) * av
  
  a = rep(0,p)
  a[ind] = av
  vexp = makevexp(a, S)
  
  return(list(a = a, vexp = vexp))
}  

####### spca v2


#' Computes sparse principal components solutions
#' 
#' Computes LS SPCA sparse principal components loadings for a given set of
#' indices.\cr See the package vignettes for details.
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
#' @seealso \code{\link{spcabb}}, \code{\link{spcabe}}, \code{\link{summary.spca}}
#' @keywords LS SPCA
#' @examples
#'    \dontrun{ 
#' 	 data(anthrop, package = "spca")
#' 	 # for uncorrelated components
#' 	 myspca <- spca(anthrop, ind = list(1:2, 3:7))
#' 	 ## print loadings
#' 	 myspca
#' 	 ## print summaries
#' 	 summary(myspca)
#' 	 # for correlated components
#' 	 myspcac <- spca(anthrop, ind = list(1:2, 3:7), unc = FALSE)
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
#' @export spca
spca = function(S, ind, unc = TRUE){### runs SPCA-LS for a given set of indices
  if(missing(ind))
    stop("need to give a list of indices in spca. if not known, use spcabe or spcabb")
  p = ncol(S)
  if (!is.list(ind)){
    nd = 1
    cr = cspca(S,ind)
    A = matrix(cr$a, ncol = 1)
  }
  else{
    nd = length(ind)
    if ( length(unc) < nd ){
      le = length(unc)
      nm = nd - le
      unc = c(unc, rep(unc[le], nm))
    }
    A = matrix(0, p, nd)
    if (all(unc == FALSE))
      Z = diag(p)
    #  vexpv = rep(NA,nd)
    for (j in 1:nd){    
      if (j == 1){#j = 1
        if (length(ind[[j]]) == 1){
          vv = sum(S[ind[[1]],]^2)/diag(S)
          A[ind[[j]],1] = 1
        }
        else{            
          cr = cspca(S,ind[[j]], vexpn = TRUE)
          A[,j] = cr$a
        }    
      }# end j = 1
      else{## j > 1
        #j = 2
        if (any(unc[j:nd] != TRUE))
          Z = makez(A[,j-1], S, Z)
        if (unc[j] == TRUE){
          cr = uspca(S,ind[[j]], A[, 1:(j-1)])
        }
        else
           cr = cspca(S,ind[[j]], Z = Z, vexpn = FALSE)
          A[,j] = cr$a
      }
      
    }
  }## end j > 1
  e = eigen(S, only.values = TRUE, symmetric = TRUE)$val
  vexp = e[1:nd] / sum(e)
  vexpv = makevexpNO(A, S)
  rownames(A) = colnames(S)
  contributions = sweep(A, 2, apply(abs(A),2, sum), "/")  
  if (all(unc==FALSE))
    unc = FALSE
  if (all(unc==TRUE))
    unc = TRUE  
  out = list(loadings = A, contributions = contributions, vexp = vexpv, vexpPC = vexp, unc = unc, ind = ind)
  if (any(unc == FALSE)){
    out$corComp = make.cor(S, A)
    out$Uncloadings = make.uncLoad(A, S)
  }
  class(out) = "spca"
  return(out)
}
