#' SPCA by Branch-and-Bound 
#' 
#' Finds the LS SPCA loadings with given cardinalities using Branch-and-Bound
#' 
#' If unc = FALSE, when nvexp = TRUE the objective function is the true
#' variance explained, otherwise the approximated one (see references or Vignettes for details). unc and excludeload can
#' be vectors of length less than nd (hence also a single value), in this case,
#' the last element is assigned to the missing ones. The BB search is
#' computationally demanding, for large problems (n > 50) consider using
#' \code{\link{spcabe}} Just in case the functions takes too long and it is
#' interrupted, a minimal output of the loadings computed is returned. 
#' This is a minimal spca
#' object with elements A, the loadings so far computed, vexp and vexpv.
#' 
#' @param S A correlation or covariance matrix.
#' @param card A vector of cardinalities for each component. the number of
#' dimensions to compute is determined from its length.
#' @param unc A logical vector indicating if each component should be should be
#' uncorrelated to the preceeding ones or not.
#' @param startind A list of indices from which the sparse loadings will be
#' computed. If missing all combinations of indices are searched.
#' @param excludeload Logical: vector (length nd or shorter) should indices of
#' non-zero loadings in previous components be excluded from future searches?
#' @param nvexp Logical. If TRUE the real variance estimated is used as
#' objective function. Otherwise an approximated form of the variance
#' explainedis used.
#' @param msg Logical: should messages be printed after each component is
#' computed
#' @return An object of class \emph{spca} is returned. It contains:
#' \item{loadings}{The matrix of loadings} \item{vexp}{A vector of variances
#' explained by each component} \item{vexpPC}{A vector of variances explained
#' by each PC} \item{ind}{A list containing the indices of the non-sparse
#' loadings} \item{niter}{Number of iterations to compute each component} Call
#' arguments \item{unc}{the argument unc passed} \item{nvexp}{The argument
#' nvexp passed} If \code{any unc[j] = FALSE} \item{corComp}{Matrix with
#' correlations among components.} \item{Aunc}{Loadings of components made
#' uncorrelated.}
#' @note Thanks to Dr Alessio Farcomeni for making avilable his R code for the
#' BB algorithm. This version is a slight variation of it.
#' @author Giovanni Merola
#' @seealso  \code{\link{spcabe}}
#' @keywords spca
#' @examples
#'  \dontrun{ 
#' data(anthrop)
#' ## 3 uncorrelated components with sparse loadings each of cardinality 
#' ## 3, 3 and 2 (the last value will be set to 3 because uncorrelated components 
#' ## must have cardinality at least equal to their order)
#' myspca1 <- spcabb(anthrop, card = c(3,3, 2) )
#' # print the results
#' myspca1
#' # print summary results
#' summary(myspca1)
#' # show how many iterations each compnent took
#' myspca1$niter
#' # plot loadings and cumulative variance explained (4 plots)
#' plot(myspca1, plotload = TRUE, onlynonzero = FALSE, variablesnames = TRUE)
#' 
#' ## 3 correlated components with sparse loadings also each of cardinality 3
#' myspca2 <- spcabb(anthrop, card = rep(3, 3), unc = FALSE, nvexp = FALSE )
#' # print results
#' myspca2
#' # print summary results
#' summary(myspca2)
#' # show how many iterations each compnent took
#' myspca2$niter
#' ## compare the correlated with the uncorrelated solutions 
#' compare(myspca1, myspca2, methodsnames = c("unc", "cor"))
#' # print the correlations among the correlated components
#' myspca2$corComp
#' # print the loadings of components made uncorrelated
#' myspca2$loadingsUnc
#' 
#' ## 3 correlated components with sparse loadings of cardinality 2, 1 and 3
#' myspca3 <- spcabb(anthrop, card = c(2, 1, 1), unc = FALSE )
#' # print the results
#' myspca3
#' # print summary results
#' summary(myspca3)
#' ## print correlation between components
#' myspca3$corComp
#' ## print loadings of components made uncorrelated
#' myspca3$loadingsUnc
#'    }
#'  
#' @export spcabb
spcabb = function(S, card, unc = TRUE, startind, excludeload = FALSE, nvexp = FALSE, 
                  msg = TRUE){
##===========================================================
## computes B&B for spca for all cardinalities
## calls branchSpca, below
##===========================================================
  tryCatch({
  p = ncol(S)
  nd = length(card)
  if (missing(startind)){
    startind = as.list(1:nd)
    for (i in 1:nd)
      startind[[i]] = c(1:p)
  }
  else{
    li = length(startind)
    if (li < nd){
      startind = append(startind, lapply(1:(nd-li), function(i, hh) 1:hh, hh = p))
    }
    if (any(vapply(startind, length) < card))
      stop(paste("length of startind cannot be less than the cardinality"))
  }
  if (length(unc) == 1){
    unc = rep(unc, nd)
  } else
    if (length(unc) < nd){
      m = length(unc)
      unc[(m+1):nd] = unc[m]
    }    
 # checks excludeload
 if (length(excludeload) == 1)
   excludeload = rep(excludeload, nd)
 else
   if (length(excludeload) < nd){
     m = length(excludeload)
     excludeload[(m+1):nd] = excludeload[m]
   }    
  A = matrix(0, p, nd)
  
  Z = diag(p)
  ind = as.list(rep(0, nd))
  indused = rep(0, nd)
  e = eigen(S, symmetric = TRUE)
  vexp = e$val[1:nd] / sum(e$val)
#  spca.out = NULL
  finish = 1
  for (j in 1:nd){### starts looping over components#j = 2  
    if (j > 1){
      indused = unique(c(indused, ind[[(j-1)]]))
      if (excludeload[j] == TRUE){  
        nind = startind[[j]][!sapply(startind[[j]], function(a,b) any(a == b), b = indused)]
      }
      else
        nind = sort(startind[[j]])
    }#end if j > 1
    else{
      nind = sort(startind[[j]])
    }
    lnind = length(nind)
    if (j == 1){  #j = 1
      if (card[j] == 1){
        
        aa = c(diag(crossprod(S))/diag(S))[nind]
        vv = nind[order(aa, decreasing = TRUE)][1]        
        A[vv,j] = 1
        ind[[j]]= vv 
        vexpv = aa[vv]/sum(diag(S))
        niter = 1
        outo = list(loadings = A[,1:j], vexp = vexpv[1:j], vexpPC = vexp[1:j], ind = ind[1:j])
        class(outo) = "spca"
      }
      else{  ## j = 1 but card[1] >1
        vv = abs(uspca(S, ind = nind)$a)
        vv = vv[nind]
        ra = (lnind + 1) - rank(vv)
        or = order(vv, decreasing = TRUE)
        inds = c(nind[or], (1:p)[-nind])
        S2 = S[inds, inds]
        ## here startind is replaced by 1:lnind because matrix sorted with startind first
        tmp = branchSpca(S2, k = card[j], ind = 1:lnind, mod = "corr")
        niter = tmp$niter
        A[nind, j] = tmp$a[1:length(nind)][ra]
        indo = which(abs(A[,j]) > 0.001)
        vexpv = tmp$vexp
        ind[[1]] = sort(indo)#sort(inds[indo])
       outo = list(loadings = A[,1:j], vexp = vexpv[1:j], vexpPC = vexp[1:j], ind = ind[1:j])
       class(outo) = "spca"
      }## end card[1] > 1    
    }## end j == 1
### ---------------------------------------
    else{ ### start j >2
      if (unc[j] == TRUE)
        Z = makez(A[,j-1], S, Z)
      else
        Z = makezM(A[,1:(j-1)], S)
      Sz = S %*% Z
      if (unc[j] == TRUE & card[j] < j){
        card[j] = j
        warning(paste("cardinality for uncorr comp", j, "set to", j))
      }
      if (card[j] == 1){

        aa = c(diag(crossprod(Sz))/(diag(S)* sum(diag(S))))
        vv = order(aa[nind], decreasing = TRUE)[1]
        
        ind[[j]]= nind[vv]
        A[ind[[j]],j] = 1
        vexpv = c(vexpv, aa[vv])
        niter = c(niter, 1)
        outo = list(loadings = A[,1:j], vexp = vexpv[1:j], vexpPC = vexp[1:j], ind = ind[1:j])
        class(outo) = "spca"
      }
      else{# j>2, card >1
        #vv = abs(eigen(Sz, symm = TRUE)$vec[,1])
        vv = abs(uspca(S, ind = nind, A = A[,1:(j-1)])$a)
        
        vv = vv[nind]
        ra = (lnind + 1) - rank(vv)
        or = order(vv, decreasing = TRUE)
        inds = c(nind[or], (1:p)[-nind])
        
        S2 = S[inds, inds]
## runs B&B on ordered variables
## dob is whether to use uncorr, corr optimising approximate vexp or corr using real vexp
        if (unc[j] == TRUE )
          dob = "uncorr"
        else{
          if (nvexp == FALSE)
            dob = "corr"
          else
            dob = "ncorr"
        }  
        tmp = branchSpca(S2, k = card[j], ind = (1:lnind), A[inds,1:(j-1)], Z = Z[inds,inds], mod = dob)
        
         niter = c(niter, tmp$niter)

        A[nind, j] = tmp$a[1:length(nind)][ra]
        indo = which(abs(A[,j]) > 0.001)
#        print(c(paste("comp",j, "ind are"), indo))
        vexpv = c(vexpv, tmp$vexp)
        ind[[j]] = sort(indo) 
ind = ind[1:nd]
indused = indused[1:nd]

        outo = list(loadings = A[,1:j], vexp = vexpv[1:j], vexpPC = vexp[1:j], ind = ind[1:j])
        class(outo) = "spca"
           }## end j>1 and card > 1
    }### end j >1
      if (msg == TRUE)
        message(paste("done comp",j))
####Sys.sleep(5)
    }
  if (length(vexpv) == length(card))
    finish = 0
  si = sign(apply(A,2, function(x) sign(x[which(abs(x)> 0.01)][1])))
  A = t(t(A)*si)
  e = eigen(S, symmetric = TRUE)
  vexp = e$val[1:nd] / sum(e$val)
  vexpv = makevexp(A, S)
  rownames(A) = colnames(S)
  out = list(loadings = A, vexp = vexpv, vexpPC = vexp, ind = ind, niter = niter,  
             unc=unc, nvexp = nvexp)
  if (any(unc == FALSE) & ncol(A) > 1){
    out$corComp = make.cor(S, A)
    out$Uncloadings = make.uncLoad(A, S)
  }
  class(out) = "spca"
 return(out) 
},
### if error 
interrupt = function(c){
  return(outo)
}, error = function(c) 
{
  message("error in spcabb")
  message(paste("returning solution with", j-1, "components"))
  return(outo)
  stop(c)
}
  )
}

branchSpca = function(D, k, A = NULL, Z = NULL, ind = FALSE, mod = c("uncorr","corr", "ncorr")) {
  ## v2 added optional ind
  ## added ncorr to use real extra sum of squares
  ## modified to max vexp  
  # funzione obiettivo del nodo 
  ## branch and bound algorithm for SPCA (U and C)
  #D covariance matrix
  # k caedinality
  # A previous loadings
  # Z matrix to make covariance of residuals as D Z
  # mod if uncorr or corr SPCA DONT USE nvexp, only for testing!!
  
  ## output:
  ## w optimal indices
  ## a full loadings
  ## vexp cariance explained
  
  ## evalsplit returns variance explained and loadings for a set of indices
#   if (mod == "ncorr"){
#     message("you chose to use nvexp, this is highly unstable")
#     d <- readline("enter Y to continue or N to change to the correct variance: ")
#     if (substring(d, 1, 1)  != "Y")
#       mod == "corr"
#   }
  if (mod[1] == "uncorr")
  {
    evalsplit = function(D, ind, A = NULL, Z = NULL ) {
      if (is.null(A)){
        cr = uspca(D, ind)
        at = cr$a 
        vexp = cr$vexp
      }
      else{          
        A = as.matrix(A)
        ee = eigen(crossprod(t(A) %*% D[,ind]), only.values = TRUE, symmetric = TRUE)$val
        if (sum(abs(ee)> 0.001)>= length(ind))
          vexp = 0
        else{  
          cr = uspca(D, ind, A) 
          at = cr$a
          vexp = cr$vexp
        }
      }
      return(list(at = at, vexp = vexp))
    }  
  } 
  else{ 
    if (mod[1] == "corr"){
      evalsplit =   function(D, ind, A = NULL, Z = NULL ) {
        cr = cspca(D, ind, Z)
        a = cr$a 
        vexp = cr$vexp        
        return(list(at = a, vexp = vexp))    
      }  
    }
    else{
      evalsplit =   function(D, ind, A = NULL, Z = NULL ) {
        cr = cspca(D, ind, Z, vexpn = TRUE)
        a = cr$a 
        vexp = cr$vexp        
        return(list(at = a, vexp = vexp))    
      }  
    }
  }
  ## e per l'appunto:
  
  if (missing(A) | is.null(A)){
    A = NULL
    Z = NULL
  }
  else{
    A = as.matrix(A)
  }  
  ## initialising indices
  if (length(ind) == 1)
    if ((ind == FALSE))
      ind = 1:ncol(D)
  else
    if ( k > length(ind))
      stop("need give cardinality lower than length ind")
  ni = length(ind)
  ### starting solution 
  ev = evalsplit(D, ind[1:k], A, Z) 
  lb = ev$vexp
  a = ev$at
  inr = ind[1:k]
  
  w = function(x,lev) {
    any(x == lev)
  }
  
  split=function(current,lev,k) {
    
    n=length(current)
    l=sum(lev)
    
    if (l==k) {
      ch1=NULL
      ch2=list(current[lev==1],c(lev,1))}
    
    if (l<k) {
      l1=lev
      l2=lev
      l1[l+1]=1
      l2[l+1]=0
      c1=current
      c2=current[-sum(l1)]
      l2=l2[-sum(l1)]
      ch1=list(c1,l1)
      ch2=list(c2,l2)}      
    
    return(list(ch1,ch2))
    
  }
  ## ni is length indices
  activeset = list(matrix(1:ni,ncol=1),rep(0,ni))
  i=1
  nit = 0
  while(length(activeset)>=2*i) {
    
    l = length(activeset[[2*i-1]]) 
    ev = evalsplit(D = D, ind = ind[activeset[[2*i-1]]], A = A, Z = Z )
    tmp = ev$vexp
    a = ev$at
    lev=activeset[[2*i]]
    #   i = 1
    if (tmp>lb) {
      
      children=split(activeset[[2*i-1]],lev,k)
      
      if (!is.null(children[[1]])) {
        activeset[[length(activeset)+1]]=children[[1]][[1]]
        activeset[[length(activeset)+1]]=children[[1]][[2]]
      }
      
      if (!is.null(children[[2]])) {
        ev = evalsplit(D = D, ind = ind[children[[2]][[1]]], A = A, Z = Z)
        tmp = ev$vexp
        #print(tmp)       
        if (tmp > lb && length(children[[2]][[1]]) == k) {
          lb=tmp
          inr = ind[children[[2]][[1]]]
          a = ev$at
        }
        
        if (tmp > lb && length(children[[2]][[1]])>k) {
          activeset[[length(activeset)+1]]=children[[2]][[1]]
          activeset[[length(activeset)+1]]=children[[2]][[2]]
          
        } 
      }        
    }
    i=i+1
    nit = nit +1
    #     if ( nit/100- floor(nit/100) == 0)
    #       print(nit)
  }  
  #  print(nit)
  
  out = evalsplit(D, ind[inr], A, Z)
  if (is.null(A))
    vexp = makevexp( out$a, D)
  else
    if (mod[1] == "ncorr"){
      vexp = makevexpNOn( cbind(A, out$a), D)[ncol(A)+1]
    }
  else
    vexp = makevexpNO( cbind(A, out$a), D)[ncol(A)+1]
  #  print(vexp)
  
  return(list(w=inr, a = out$a, vexp = vexp, niter = nit))
  
}
