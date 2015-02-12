tryCatch.W.E = function(expr)
{
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}


### backsolve if full rank
geninv = function(M, isChol = TRUE, rank){
  ## implements geninv by Courrieu 
  ## if isChol == TRUE M must be output from function chol
  ## not permuted
  if(isChol == FALSE){
    M = chol(M, pivot = TRUE)
    rank = attr(M, "rank")
    pivot = order(attr(M, "pivot"))
  }
  else{
    stopifnot(Matrix::isTriangular(M) )
    if (!is.null(attr(M, "pivot")))
      pivot = order(attr(M, "pivot"))
    else
      pivot = NULL
    if(missing(rank))
      message("no rank passed, I do for you, finger crossed")
    rank = sum(rowSums(abs(M)) > 0.0001)
  }
  p = ncol(M)
  if (rank == p){
    L = backsolve(M, diag(p))
    L = L[pivot,]
    tcrossprod(L, L)
  }
  else{ ## Courrieu  formula for MP g.inverse
    
#     L = t(M[1:rank, pivot])     
#     tcrossprod(L %*% solve(crossprod(L)))    
    L = (M[1:rank, pivot])     
    crossprod(solve(tcrossprod(L)) %*% L)     
  }
  
}


geigen = function(B, S, nc = 1, vexp = FALSE){
  ## gets  generalised  eigenvectors with choleski, even if D is singular )
  Q = chol(B, pivot = TRUE)
  Q = Q[1:attr(Q, "rank"), order(attr(Q, "pivot"))]
  a = eigen(tcrossprod(Q %*% S, Q))
  
  aa = crossprod(Q, a$vec[, 1:nc, drop = FALSE])
  if (vexp == TRUE)
    return(list(vec = aa, val = a$val[1:nc]))
  else 
    return(list(vec = aa))
}

make.uncLoad2 = function(A, S, scaled = TRUE){
  ## ortogonalises loadings
  p = ncol(S)
  if (is.vector(A))
    d = 1
  else
    d = ncol(A)
  if (d > 1){
    B = A   
    Z = diag(1, p)
    for (i in 2:d){
      Z = spca:::makez(B[,i-1], S, Z)
      B[,i] = Z %*% A[,i]
    }  
    if (scaled == TRUE)
      B = t(t(B)/ sqrt(colSums(B^2)))
    return(B)
  }
  else
    return(A)
}

fmakevexp = function(A, S, eigen = FALSE , unc = FALSE, scaled = FALSE){
  ##  use this it's slightly slower than makevexp2 but doesnt break small vars
  ## scaled does like QR, i.e. scales uncorrelated comps, which should be wrong
  if (is.vector(A))
    A = matrix(A, ncol = 1)
  ind = (colSums(abs(A) > 0.001) > 0)
  if (any(unc == FALSE)){
    A = make.uncLoad2(A, S, scaled = scaled)
  }
  E = crossprod(A, S)
  M = E %*% A
  vexp = diag(tcrossprod(E))/(diag(M)*sum(diag(S)))
  ## scaled are like QR unscaled keep sparse load unit L2
  if (eigen == TRUE){
    eigenvalues = drop(diag(M))#
    if (scaled == TRUE)
      eigenvalues = eigenvalues/colSums(A^2)
    return(list(vexp = vexp, eigenvalues = eigenvalues))
  }
  else
    return(vexp)
}

