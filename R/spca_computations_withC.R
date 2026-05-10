# returns the non-zero loading with the smallest absolute value for each column
# input a matrix of loadings or contributions
get_minload = function(smpc, eps = 1e-4){
  if(!is.matrix(smpc))
    stop("get.minload: a matrix of loadings or an spca object is needed") 
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
  
  if(is.list(x) && (!is.null(x$loadings)) && (is.matrix(x$loadings))){
    return(scaleColsC(A = x$loadings, normtype = 1, 
                      sig = rep(1, ncol(x$loadings))))
  }
  else{
      return(scaleColsC(A = x, normtype = 1, 
                        sig = rep(1, ncol(x))))
  }
  if (is.vector(x))
    return(x/sum(abs(x)))
  
  stop("X must be a matrix or vector of loadings or list
         containing the sam with the name loadings")
  }



#transforms a variance matrix to a correlation marix S variance matrix
var2cor = function(S){
  if ((!is.matrix(S)) || (!isSymmetric(S)) || any(diag(S) < 1e-4))
    stop(("S must be a symmetric matrix with non zero diagonal values"))
  if (exists("var2corC", mode = "function"))
    return(var2corC(M))
  else {
    d = as.vector(diag(diag(S)^-0.5))
    return(vtauC(d, S, d))
  }
  invisible()
}

## computes correlation between pairs of sPCs
# wrapper for the C++ function
# inputs
# @param Matrix of loadings
# @param Covariance matrix
make_corComp_S = function(A, S){
 if ((is.vector(A)) || (ncol(A) == 1)){
    warning("A must be a matrix with at least two columns")
    return(NULL)
  } 
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
  stop("A ns S must be matrices")
  
  if((!isSymmetric(S)))
    stop("S must be a covariance or correlation matrix")
  
  if (nrow(A) != nrow(S))
    stop("A must have the same nummber of rows as S")
  
    return(make_vexpSC(A, S))

}

