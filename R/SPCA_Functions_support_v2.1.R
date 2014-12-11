
makevexp =  function (A,D) {
{  ## new improved using ind
  ## revised to allow one component only
  ## computes vexp for uncorrelated components
}
if (is.matrix(A)){
    A = sweep(A, 2, sqrt(apply(A^2,2,sum)), "/")  
    nd = ncol(A)
    vt = sum(diag(D))
    vexp = rep(0, nd)
    for (i in 1:nd) {
      ind = which(abs(A[,i])> 0.001)
      if (length(ind) == 1){
        B = D[,ind]
        b = D[ind,ind]
        vexp[i] = sum(B^2)/(vt * b)
      }
      else{
        B = D[,ind] %*%  A[ind, i]
        b = drop(crossprod(B[ind,], A[ind, i]))
        vexp[i] = crossprod(B, B)/(vt * b)
      }
      D = D - B %*% t(B)/b
    }
  }
  else{
    ind = which(abs(A) > 0.001)
    A = A[ind]/sqrt(sum(A[ind]^2))
    vt = sum(diag(D))
    B = D[,ind] %*% as.matrix(A)
    b = drop(crossprod(B[ind,], as.matrix(A)))
    vexp = crossprod(B, B)/(vt * b)
  }
  return(vexp)
}

makevexpNO = function(A, D){
  ##  computes vexp for correlated components
  A = as.matrix(A)
  nd = ncol(A)
  vexp = rep(0, nd)
  for (i in 1:nd) {
    if (i == 1)
      Dz = D
    else{
      Z = makezM(A[,1:(i-1)], D)
      Dz = D %*% Z
    }
    ind = which(abs(A[,i]) > 0.001)
    if (length(ind) > 1){
      B = drop(crossprod(Dz[,ind] %*% A[ind, i]))
      b = drop(t(A[ind,i]) %*% Dz[ind, ind] %*% A[ind, i])
    } else{### uses * because simple number
      B = drop(crossprod(Dz[,ind]   * A[ind, i]))
      b = drop(t(A[ind,i]) * Dz[ind, ind] * A[ind, i])
      
    }
    vexp[i] = B/(b*sum(diag(D)))
  }
  return(vexp)
}

makevexpNOn = function(A, D){
  ## computes the approximated vexp maximised by the uncorrelated components not vexp
  A = as.matrix(A)
  nd = ncol(A)
  vexp = rep(0, nd)
  for (i in 1:nd) {
    if (i == 1)
      Dz = D
    else{
      Z = makezM(A[,1:(i-1)], D)
      Dz = D %*% Z
    }
    A = as.matrix(A)
    B = drop(crossprod(Dz %*% A[, i]))
    b = drop(t(A[,i]) %*% D %*% A[, i])
    vexp[i] = B/(b*sum(diag(D)))
  }
  return(vexp)
}

eig = function(D, nd = 4, prn = FALSE){
## does eigendecomposition returning also vexp and cvexp
## returns object spca  
  ee = eigen(D, symmetric = TRUE)
  ee$vexp = ee$val/sum(ee$val)
  ee$cvexp = cumsum(ee$val)/sum(ee$val)
  if (prn == TRUE){
    out = list(A = ee$vec[,1:nd], vexpv = ee$vexp[1:nd])#, vexp = ee$vexp[1:nd])
    class(out) = "spca"
    print(out)
  }
  return(ee)  
}

make.uncLoad = function(A, S){
## ortogonalises loadings
  p = ncol(S)
  d = ncol(A)
  B = A   
  Z = diag(1, p)
  for (i in 2:d){
    Z = makez(A[,i-1], S, Z)
    B[,i] = Z %*% A[,i]
  }  
  B = sweep(B,2, sqrt(apply(B^2,2,sum)), "/")
  return(B)
}

make.corx = function(S, A){
  ## computes the cor between the components and each x-variable  
  M = S %*% A
  top = t(t(M)/diag(sqrt(crossprod(A,M))))
  top = top/sqrt(diag(S))
  return(top)  
} 

make.cor = function(D, A, dgt = 4){
## computes correlation among components
  if (is.vector(A))
    out = (NULL)
  else 
    if (ncol(A) == 1)
      out = (NULL)
    else 
      if (is.matrix(A) & ncol(A) > 1 ){  
          out = crossprod(A, D) %*% A
          o = sqrt(diag(out))^-1
          out = round(diag(o) %*% out %*% diag(o), dgt)
      }
  return(out)
}

makez = function(a, S, Z){
### updates the Z matrix with next loadings 
  ## only for unc components
  p = ncol(S)
  if (missing(Z))
    Z = diag(1,p)
  a = as.matrix(a)
  Z - (a %*% crossprod(a,S))/(drop( crossprod(a,S) %*% a))  
}

makezM = function(A,D){
  ## makes Z matrix from a full set of loadings, 
  ## must use this with correlated components
  diag(ncol(D)) - A %*% solve(crossprod(A,D) %*% A) %*% crossprod(A,D) 
}


make.vexpeig= function(ee, ndim, cum = TRUE, dgt = 3){
## computes the proportional variance explained by eigenvectors
  if(missing(ndim))
    ndim = length(ee$val)
  if (cum == TRUE)
    out = round(cumsum(ee$val[1:ndim])/sum(ee$val), dgt)
  else
    out = round((ee$val[1:ndim])/sum(ee$val), dgt)
  return(out)  
}

make.cont = function(smpc){
  ## standardise a matrix of loadings to unity l1 norm
  ## scales loadings to unit L1 contributions
  ## input either a spca object or matrix of loadings
  ## v2 modified, doesnt return vexp anymore
  if (any(class(smpc) == "spca")){
    if (any(names(smpc) == "contributions"))
      Ac = smpc$contributions
    else{
      smpc = smpc$loadings
      Ac = round(sweep(smpc, 2, apply(abs(smpc),2,sum), "/"), 5)
    }
  }  
  else{ 
    if (is.list(smpc) & length(smpc) == 1)
      smpc = smpc[[1]]
    if ( is.matrix(smpc))
      Ac = round(sweep(smpc, 2, apply(abs(smpc),2,sum), "/"), 5)
    else
      if (is.vector(smpc))
        Ac = smpc/sum(abs(smpc))
      else
        stop("a matrix of loadings or an spca object is needed")      
  }
  return(Ac)
}

get.minload = function(smpc, perc = FALSE, eps = 0.001){
## returns the non-zero loading with the smallest absolute value for each column
## input a matrix of loadings or an spca object
## if perc == TRUE will compute on the percent contribution  
   if (any(class(smpc) == "spca"))
    smpc = smpc$loadings
   else 
    if (! is.matrix(smpc))
    stop("get.minload: a matrix of loadings or an spca object is needed")  
   if (perc == TRUE)
     smpc = make.cont(smpc)
   d = ncol(smpc)
   gl = function(x)
   min(abs(x[abs(x)> eps]))
   apply(smpc, 2, gl)
}

get.card = function(A, eps = 0.01){
## returns the cardinality of the columns of a matrix of loadings  
if (any(class(A) == "spca"))
  A = A$loadings
  if(is.null(dim(A)))
    sum(abs(A) > eps)
  else
    apply(abs(A)> eps, 2, sum)
}

mult.eigen = function(A, b, ind, power = 1L){
{#multiplies A %*% diag(b) %*% t(A)
  # used for fitted variance matrix
}
  if (missing(ind))
    ind = 1:ncol(A)
  if (length(b[ind]) != ncol(A[,ind]))
    stop("matrix A and vector b must be of compatible dimension")
  if (power == 1)
    t(A[,ind] %*% (t(A[,ind]) * b[ind]))
  if (power != 1)
    if (power == 0.5 & any(b < 0))
      stop("mult.eigen: cannot take sqrt of negative b")
    t(A[,ind] %*% (t(A[,ind]) * (b[ind])^power))
}

myprintspca = function(smpc, cols, digits = 3, rows, noprint = 1E-03, 
                       rtn = FALSE, perc = FALSE, namescomp = NULL){
  ### generic print for spca objects 
  ## used by spca.comp
  ## v2.1 fixed problem with only 1 component
  if (any(class(smpc) == "spca")){  
    if (perc == TRUE){
      if(any(names(smpc) == "contributions"))
        A = smpc$contributions
      else
        A = make.cont(smpc$loadings)
    }
    else{
      if (is.null(smpc$As))
        A = smpc$loadings
      else 
        A = smpc$As
    }
  }
  else{#
    if (is.matrix(smpc) | is.vector(smpc)){
      A = make.cont(smpc)
    }
    else
      stop(paste("The argument must be either a smpc object or a matrix, not a", class(smpc)))
  }
  
  if (is.vector(A))
    A = as.matrix(A)
  ## end if
  if (missing(cols))
    cols = 1:ncol(A)
  else
    if (length(cols) == 1L)
      cols = 1:cols
  
  if (missing(rows))
    rows = 1:nrow(A)
  else
    if (length(rows) == 1L)
      rows = 1:rows
  
  A = as.matrix(A[rows,cols])
  ## assigns names to laodings
  if (!is.null(namescomp) & length(namescomp) == ncol(A)){
    colnames(A) = namescomp
  }
  else{
    if (!is.null(namescomp) & length(namescomp) != ncol(A))
      warning("the length of namescomp is incorrect, automatic names assigned")
    colnames(A) = paste("Comp",1: ncol(A), sep = "")
  }
  
  
  
  if (perc == TRUE)
    fx <- format(round(A*100, max(digits-2,0)),drop0trailing = TRUE, justify = "centre")
  else
    fx <- format(round(A, digits),drop0trailing = TRUE, justify = "centre")
  names(fx) <- NULL
  nc <- nchar(fx[1L], type = "c")
  fx[abs(A) < noprint] <- paste(rep(" ", nc), collapse = "")
  #  ind = (abs(A)> noprint & abs(A) < noprint)
  #  fx[ind] = "--"
  fx = format(fx, justify = "right" )
  if (any(class(smpc) == "spca")){
## chv
    vexp = smpc$vexp[cols]
    doo = rep("-----", ifelse(is.null(ncol(fx)), 1, ncol(fx)))
    fx = rbind(fx, doo, round(100*vexp,1))
    rownames(fx)[nrow(fx)-1] = ""    
    rownames(fx)[nrow(fx)] = "PCVE"
  }  
  if (perc == TRUE)
    print("Percentage Contributions")
  else
    print("Loadings")
  if (ncol(A) == 1L){
    #    fx = t(fx)
    print(t(fx), quote = FALSE)#, ...)
    
    
    #     rownames(fx)[1] = "Loadings"
  }
  else
    print(fx, quote = FALSE)#, ...)
  
  if(rtn == TRUE){   
    return(fx)    
  }  
  else 
    invisible()
}
