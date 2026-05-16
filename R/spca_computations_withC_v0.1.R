# returns the non-zero loading with the smallest absolute value for each column
# input a matrix of loadings or contributions
get_minload = function(smpc, eps = 1e-4){
  if(!is.matrix(smpc))
    stop("get_minload: a matrix of loadings or contributions is needed") 
  gl = function(x)
    min(abs(x[abs(x)> eps]))
  apply(smpc, 2, gl)
}

# returns the cardinality of the columns of a matrix of loadings  
get_card = function(A, thresh = 1e-4){
  if (is.spca(A) )
    A = A$loadings
  if(is.vector(A))
    sum(abs(A) > thresh)
  else
    if(is.matrix(A))
      colSums(abs(A)> thresh)
  else
    stop("A must be an spca object or a matrix or a vector")
  
}


# computes the unit L1 norm contributions from a matrix of loadings
# x can be either the matrix or vector of loadings or list containing a 
# matrix called loadings
make_contributions = function(x){
  
  if (is.spca(x) && validate_spca(x))
    {
    return(scaleColsR(x$loadings, normtype = 1, sig = NULL))
  }
  if (is.data.frame(x))
    x = as.matrix(x)
  if (is.matrix(x))
    return(scaleColsR(x, normtype = 1, sig = NULL))
  
  if (is.vector(x))
    return(x/sum(abs(x)))
  
  stop("x must be an spca object, or a matrix or vector of loadings")
  }



#transforms a variance matrix to a correlation matrix S variance matrix
var2cor = function(S){
  if ((!is.matrix(S)) || (!isSymmetric(S)) || any(diag(S) < 1e-4))
    stop(("S must be a symmetric matrix with non zero diagonal values"))
    return(var2corC(S))
}

## computes correlation between pairs of sPCs
# wrapper for the C++ function
# inputs
# @param Matrix of loadings
# @param Covariance matrix
make_spc_cor_S = function(A, S){
 if ((is.vector(A)) || (ncol(A) == 1)){
    warning("A must be a matrix with at least two columns")
    return(NULL)
 } 
  if (is.data.frame(A))
    A = as.matrix(A)
    if (nrow(A) != nrow(S))
      stop("A must have the same number of columns as the number of rows of S")
  
  if ((!is.matrix(S)) || (!isSymmetric(S)) || any(diag(S) < 1e-4)){
    warning(("S must be a symmetric matrix with non zero diagonal values"))
    return(NULL)
  }  else 
      return(makeCorCompC(A, S))
  invisible()
}


# #unused
# extra_variance_explained = function(A, S, tol = 1e-12) {
#   k = ncol(A)
#   trS = sum(diag(S))
#   SA =  abC(S, A)
#   
#   cv = numeric(k)
#   L = matrix(0, k, k)
#   
#   for (j in seq_len(k)) {
#     
#     aj = A[, j]
#     
#     if (j == 1L) {
#       d = drop(vtuC(aj, SA[, j, drop = TRUE]))
#       if (!is.finite(d) || (d <= tol))
#         stop("Non-finite or zero denominator for first component")
#       
#       L[1, 1] = sqrt(d)
#       
#     } else {
#       b = drop( vtaC(aj, SA[, 1:(j - 1), drop = FALSE]))
#       d = drop(vtuC(aj, SA[, j, drop = TRUE]))
#       
#       v = forwardsolve(L[1:(j - 1), 1:(j - 1), drop = FALSE], b)
#       
#       piv = d - sum(v^2)
#       if (!is.finite(piv) || (piv <= tol)) {
#         stop(paste(
#           "Non-finite or non-positive pivot at component", j
#         ))
#       }
#       
#       L[j, 1:(j - 1)] = v
#       L[j, j] = sqrt(piv)
#     }
#     
#     Lj = L[1:j, 1:j, drop = FALSE]
#     
#     ## trace(G_j^{-1} H_j) = || L_j^{-1} (SA_j)' ||_F^2
#     Z = forwardsolve(Lj, t(SA[, 1:j, drop = FALSE]))
#     cv[j] = sum(Z^2) / trS
#   }
#   
#   list(
#     vexp = c(cv[1], diff(cv)),
#     cvexp = cv
#   )
# }

# wrapper for C++ function
# computes the extra variance explained by difference
make_vexp =  function (A, S) {
{ ## should add a flag for unc 
  ## new improved using ind
  ## revised to allow one component only
  ## computes vexp for uncorrelated components
}
if ((!is.matrix(A)) || (!is.matrix(S)))
  stop("A and S must be matrices")
  
  if((!isSymmetric(S)))
    stop("S must be a covariance or correlation matrix")
  
  if (nrow(A) != nrow(S))
    stop("A must have the same number of rows as S")
  
    return(make_vexpSC(A, S))

}

scaleColsR = function(M, normtype = 2, sig = NULL){
  if(is.data.frame(M))
    M = as.matrix(M)
  if(!is.matrix(M))
    stop("M must be a matrix or a data.frame")
  if (!(normtype[1] %in% (1:2)))
    stop("normtype must be 1 or 2")
  if (is.null(sig))
    sig = rep(1, ncol(M))
  
  scaleColsC(M, normtype, sig)
}


scaleR = function(M, center = FALSE, scale = TRUE){
  if(is.data.frame(M))
    M = as.matrix(M)
  if(!is.matrix(M))
    stop("M must be a matrix or a data.frame")
  scaleC(M, center, scale)
}

