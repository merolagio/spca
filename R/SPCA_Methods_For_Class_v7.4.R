
#' Verifies if an object is of class spca
#' 
#' @param x Any object suspected of being of class spca.
#' @return Logical: TRUE if object is of class spca, FALSE otherwise.
#' @export is.spca
is.spca = function(x){
  inherits(x, "spca")
}

#' Prints the sparse loadings from an spca object
#' 
#' Prints sparse loadings omitting the zero ones and giving the cumulative
#' variance explained.
#' 
#' 
#' @param x An spca object.
#' @param cols A vector indicating which components should be printed. Default
#' all. If an iteger is passed, it is set to 1:cols.
#' @param only.nonzero  Logical: if = TRUE only the nonzero loadings are printed.
#' otherwise all loadings are printed.
#' @param perc Logical: should the loadings be standardised to unit \eqn{L_1}
#' norm (and printed as percentage contributions)?
#' @param digits Integer: number of decimal figures.
#' @param thresh Value below which loadings are considered zero and not
#' printed.
#' @param rtn Logical: should the formatted (text) table be returned?
#' @param namescomp A vector of names for the components. If NULL assigned as
#' "Comp j"
#' @param  ...  Additonal arguments for generic print, additional arguments will
#'  generate an error.
#' @return If rtn = TRUE, it returns a text table formatted as specified by the
#' arguments.
#' @note This is a wrapper for the main function in which the "dots" are disabled
#' so that only exact (or partial) prescribed arguments can be entered. 
#' @seealso Examples in \link{spcabb} and \link{spcabe}.
#' @export print.spca
print.spca = function(x, cols, only.nonzero = TRUE, perc = TRUE, digits = 3, thresh = 1E-03, 
                    rtn = FALSE, namescomp = NULL, ...){#
  goodarg = as.list(environment())
  badarg = eval(substitute(alist(...)))
  if (length(badarg) > 0){
    stop(paste0("\nUnused arguments: ", paste(names(badarg), collapse=", ")))
  }
## -------------------------------------------------------------------------
      smpc = x
  if (is.spca(smpc) == TRUE){ ## should use is.spca 
    if (perc == TRUE){
      if(any(names(smpc) == "contributions"))
        A = smpc$contributions
      else
        A = make.cont(smpc)
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
      if (perc == TRUE)
        A = make.cont(list(smpc = smpc))
    }
    else
      stop(paste("The argument must be either a smpc object or a matrix, not a", 
                 class(smpc), "object"))
  }
  
  if (is.vector(A))
    A = as.matrix(A)
  ## end if
  if (missing(cols))
    cols = 1:ncol(A)
  else
    if (length(cols) == 1L)
      cols = 1:cols
  if (only.nonzero == FALSE)
    rows = 1:nrow(A)
  else{
    rows = which(apply(abs(A) > 0.0001, 1, any))
}  
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
  
# -----  formatting -------------------  
  
  if (perc == TRUE)
    fx <- format(round(A*100, max(digits-2,0)),drop0trailing = TRUE, justify = "centre")
  else
    fx <- format(round(A, digits),drop0trailing = TRUE, justify = "centre")
  names(fx) <- NULL
  nc <- nchar(fx[1L], type = "c")
  fx[abs(A) < thresh] <- paste(rep(" ", nc), collapse = "")
  #  ind = (abs(A)> thresh & abs(A) < thresh)
  #  fx[ind] = "--"
  fx = format(fx, justify = "right" )
  if (any(class(smpc) == "spca")){
    vexp = cumsum(smpc$vexp[cols])
    doo = rep("-----", ifelse(is.null(ncol(fx)), 1, ncol(fx)))
    fx = rbind(fx, doo, round(100*vexp,1))
    rownames(fx)[nrow(fx)-1] = ""    
    rownames(fx)[nrow(fx)] = "PCVE"
  }  
  if (perc == TRUE)
    message("Percentage Contributions")
  else
    message("Loadings")
  if (ncol(A) == 1L){
    print(t(fx), quote = FALSE)#, ...)
  }
  else{
    print(fx, quote = FALSE)#, ...)
    cat(paste(" "))
  }
  if(rtn == TRUE){   
    return(fx)    
  }  
  else 
    invisible()
}


#' Prints summaries from an spca object
#' 
#' Prints summaries and comparisons with the full PCA solutions for a set of LS
#' SPCA loadings.
#' 
#' The summaries are printed as formatted text, if rtn = TRUE, the value
#' returned is a numerical matrix.
#' 
#' For each component the following summaries are computed: \tabular{ll}{ 
#' PVE\tab The percentage variance explained\cr
#' PCVE \tab The percentage cumulative variance explained\cr
#' PRCVE \tab The percentage cumulative variance explained relative to that of the corresponding principal components\cr
#' Card \tab The cardinality, that is the number of non zero loadings\cr
#' Ccard \tab The cumulative cardinality.\cr
#' PVE/Card \tab The percentage variance explained over the cardinality.\cr
#' PCVE/Ccard \tab The percentage cumulative variance explained over the cumulative cardinality.\cr
#' Converged \tab If the object was computed with \emph{spcabe}, type of convergence: 0 if all loadings bigger than \emph{thresh}, 1 if minimal cardinality reached or 2 if the maximal variance loss in trimming was reached.\cr
#' MinLoad \tab Minimum absolute value of the non-zero loadings. }
#' If perc = TRUE, the last row gives the minimum absolute percentage contribution, MinPContr.
#'
#' 
#' @param object An spca object.
#' @param cols A vector indicating which components should be included. Default
#' all.  If an iteger is passed, it is set to 1:cols.
#' @param perc Logical: should the laodings be standardised to unit L1 norm
#' (and printed as percentage contributions)
#' @param rtn Logical: should the summary matrix of summaries be returneded?
#' @param prn Logical: should anything be printed? Takes priority on prnload.
#' @param thrsehcard Value below which loadings are considered zero and not
#' counted in the cardinality
#' @param ...  Additonal arguments for generic summary, additional arguments will
#' generate an error.
#' @return If rtn = TRUE, a numerical matrix with the summaries.
#' @note This is a wrapper for the main function in which the "dots" are disabled
#' so that only exact (or partial) prescribed arguments can be entered. 
#' @seealso Examples in \link{spcabe} and \link{spcabb}
#' @export summary.spca
summary.spca = function(object, cols, perc = TRUE, rtn = FALSE, prn = TRUE, 
                        thrsehcard = 0.001, ...){#
  goodarg = as.list(environment())
  badarg = eval(substitute(alist(...)))
  if (length(badarg) > 0){
    stop(paste0("\nUnused arguments: ", paste(names(badarg), collapse=", ")))
  }
# ---------------------------------------------------------------------------------  
  ## generic S3 method creates summaries from spca object printing for contributions
    smpc = object
  if (!any(class(smpc) == "spca")){
    stop("summary.spca works only for spca objects")
  }  
  if( is.vector(smpc$loadings) )
    smpc$loadings = as.matrix(smpc$loadings)
  if (missing(cols)){
    cols = 1:length(smpc$vexpPC)  
  }
  else
    if (length(cols) == 1L)
      cols = 1:cols
  
  out = rbind(round(smpc$vexp*100,1), 
              round(cumsum(smpc$vexp)*100,1),
              round(cumsum(smpc$vexp)/ cumsum(smpc$vexpPC)*100,1),
              round(apply(abs(smpc$loadings)> thrsehcard,2,sum)),
              round(cumsum(apply(abs(smpc$loadings)> thrsehcard,2,sum))),
              round(smpc$vexp/apply(abs(smpc$loadings)> 0.01,2,sum)*100,1),
              round(cumsum(smpc$vexp)/cumsum(apply(abs(smpc$loadings)> 0.01,2,sum))*100,1)
  )
  out = as.matrix(out[,cols])
  colnames(out) = paste("Comp", cols, sep = "")  
  rownames(out) = 
    c("PVE", "PCVE", "PRCVE",
      "Card", "Ccard","PVE/Card", "PCVE/Ccard")
  
  nc = 7
  if (!is.null(smpc$conv)){
    out = rbind(out, smpc$conv[cols])
    rownames(out)[nc + 1] = "Converged"
    nc = nc + 1
  }
  out = rbind(out, get.minload(smpc, perc = perc)[cols])
  if (perc == FALSE)
    rownames(out)[nc + 1] = "MinLoad"
  else
    rownames(out)[nc + 1] = "MinCont"
  
  nc = nc + 1            
  if(prn == TRUE){
    fx <- format(out, digits = 1, drop0trailing = FALSE, justify = "right")
    if (ncol(out) > 1L){ 
      fx[c(1:3, 6:nc),] = apply(out[c(1:3, 6:nc),], 2, paste , "%", sep = "")  
      fx[4:5,] = format(round(out[4:5,]), drop0trailing = TRUE, justify = "right",trim = TRUE, nsmall=0)  
      if (!is.null(smpc$conv)){
        fx[nc -1,] = format(round(out[nc - 1,]), drop0trailing = TRUE, justify = "right",trim = TRUE, nsmall=0)  
      }
    }
    else{
      fx[c(1:3, 6:nc),1] = paste(out[c(1:3, 6:nc),1], "%", sep = "")  
      fx[4:5,1] = format(round(out[4:5,1]), drop0trailing = TRUE, justify = "right",trim = TRUE, nsmall=0)  
      if (!is.null(smpc$conv)){
        fx[nc -1,] = format(round(out[nc - 1,]), drop0trailing = TRUE, justify = "right",trim = TRUE, nsmall=0)  
      }
      
    }
    if (perc == FALSE)
      fx[nc,] = format(round(out[nc,], digits = 3), digits = 3, drop0trailing = FALSE, justify = "right")
    else
      fx[nc,] = paste(round(out[nc,] *100,1), "%", sep = "")
    
    print(fx, quote = FALSE, justify = "right")#, ...)
  }
  
  
  if (rtn == TRUE )
    return(out)
  else
    invisible()
}


#' Compares two or more spca solutions
#' 
#' Compares two or more spca solutions by printing the loadings and the summary
#' statistics next to each other. It can plot the cumulative variances
#' explained together with those of PCA and the loadings for each component.
#' 
#' For the meaning of each summary statistic see \code{\link{summary.spca}}.  Plotvar
#' plots \emph{nd} values. if \emph{plotload} or \emph{prnload} are integer,
#' that number of loaidngs will be processed. However, \emph{nd}
#' loadings are always returned if \emph{rtn=TRUE}.
#'
#' @param smpc An spca object 
#' @param compareto A list of spca objects with which smpc is to be compared. Can be givenas 
#' single object
#' @param nd Number of dimensions to compare. If not specified set to the
#' minimum number of loadings in the objects.
#' @param methodsnames Names for each object included. If not specified, labels are
#' created as Met1, Met2, etc.
#' @param perc Logical: should the loadings be standardised to unit \eqn{L_1}
#' norm (and printed as percentage contributions).
#' @param plotvar Logical: should the cumulative variances be plotted?
#' @param plotload Logical or integer (>0): should the loadings be plotted and
#' how many?
#' @param labelload Logical: write variables names loading plots?
#' @param sizelabelsload Real: expansion coefficient for loading plot label. 
#' See \emph{cex} in \code{\link[graphics]{par}}, 
#' @param poslabeload integer: position of the labels of the laodings.
#' 1 = bottom, 2 = left, 3 = top (default), 4 = right.
#' @param prnload Logical or Integer (>0): should the loadings be printed and
#' how many?
#' @param shortnamescomp Logical: should the loadings be printed with short names
#' (Cx.y) or long ones (Cx.methodsnames)?
#' @param rtn Logical: should the text table of loadings and the matrix of
#' summaries be returneded?
#' @param prn Logical: should anything be printed? Takes priority on prnload.
#' @param only.nonzero Logical: should only nonzero contributions be printed?
#' @param bnw Logical: should plots be in blck and white?
#' @param sizelegend Magnification of the legend labels, see \emph{cex} in 
#' \code{\link[graphics]{par}}.
#' @param mfrowload Number of loadings plots per row.
#' @param mfcolload Number of loadings plots per column.
#' @param ... additional arguments for generic compare. Disabled, additional arguments will
#' generate an error.  
#' @return If rtn = TRUE, it returns a formatted text table with the loadings
#' and a matrix with the summaries.
#' @seealso Examples in \code{\link{spcabb}} and \code{\link{spcabe}}.
#'
#' @method compare spca
#' @export compare.spca
compare.spca = function(smpc, compareto, nd, methodsnames, perc = TRUE, 
                        plotvar = TRUE, plotload = FALSE, labelload = TRUE, 
                        sizelabelsload = 0.85, poslabeload = 3, prnload = TRUE,
                        shortnamescomp = TRUE, rtn = FALSE, prn = TRUE, 
                        only.nonzero = TRUE, bnw = FALSE, mfrowload = 1, mfcolload = 1, 
                        sizelegend = 0.85, ... ) {
## -------------------------------------------------------
  ## compares two or more spca solutions
  ## smpc an spca object, compare to a list with one or more spca obj to ocmpare
##==============================================================
if(class(smpc) != "spca")
    stop("must pass an spca object as first argument to compare")
  badarg = eval(substitute(alist(...)))
  if (length(badarg) > 0){
    stop(paste0("\nUnused arguments: ", paste(names(badarg), collapse=", ")))
  }  
  nam = rownames(smpc$loadings)
  if (class(compareto) == "spca")
    smpc = list(smpc, compareto)
  else 
    if (class(compareto) == "list")
      if (all(sapply(c(list(smpc), compareto), is.spca))){
        smpc = list(smpc)
        
        for (k in 1:length(compareto))
          smpc[[k + 1]] = compareto[[k]]
      }
  else 
    stop("must pass a list of spca objects as compareto argument")
  oldpar = par(no.readonly = TRUE)
  p = nrow(smpc[[1]]$loadings)
  chkl = sapply(smpc, function(K) nrow(K$loadings))
  if (any(chkl != p))
    stop("the solutions to compare must refer to the same dataset")
  # n is number of spca objects
  n = length(smpc)
  # checks that all objects have the number of variables
  dimall = sapply(smpc, function(a) nrow(a$loadings))
  nr = dimall[[1]]
  if (any(dimall != dimall[1]))
    stop("spca object to compare must be produced from the same problem")
  # if nd not specified sets equal to minimum dim
  if (missing(nd)){
    ml = min(sapply(smpc,function(x) length(x$vexpPC)))
    nd = ml
  }
  if (prn == FALSE)
    prnload = FALSE
  ## su is list of summaries for all objects
  su = lapply(smpc, summary, prn = FALSE, rtn = TRUE)
  
  ## A is list of loadingss for all objects
  A = lapply(smpc, function(x) return(x$loadings))
  
  # creates vexp   
  vexp = sapply(smpc, function(x, ndd){vx = x$vexp[1:ndd]; return(vx)}, ndd = nd)
  # this makes first column vexp PCA
  vexp = cbind(smpc[[1]]$vexpPC[1:nd], vexp) 
  vxp = vexp
  
  vexp = apply(vexp, 2, cumsum)
{  if (missing(methodsnames)){
  if (!is.null(names(smpc)))
    methodsnames = c("PCA", names(smpc))
  else
    methodsnames = c("PCA", paste("Met", 1:n, sep = ""))
}
else
  methodsnames = c("PCA", methodsnames)
  }
pchlist = c(15:20,7:14)
if (bnw == TRUE)
  colo = rep(1,n+1)
else
  colo = 1:(n+1)
### ploat laodings 
### npl is number of loadings to plot
{
    if (is.logical(plotload) & plotload == TRUE)
      npl = nd
    else 
      if (is.numeric(plotload) & plotload > 0)
        npl = plotload
    else
      npl = FALSE
  }
if(npl > 0){
  cefx = mfrowload
  cefy = mfcolload 
  par(mfrow = c(mfrowload, mfcolload), mar = c(2, 4.1, 1.1, 1.1))
  for (j in 1:npl){
    A1 = sapply(A, function(d, j) return(d[, j]), j = j)#, kk = j)
    if (perc == TRUE)
      A1 = sweep(A1, 2, apply(abs(A1), 2, sum), "/")
    ra = range(A1)*(1.05)
    #ra[1] = min(0, ra[1])
    di = diff(ra)/5
    yLabels <- seq(ra[1], ra[2], di)
    plab = pretty(yLabels)
    plot(c(1,nr), c(min(plab), max(plab)), type = "n", xlab = "",
         ylab = ifelse(perc, "contributions", "loadings"), 
         main = paste("Component",j), xaxt = "n", yaxt = ifelse(perc, "n", "s")) 
    axis(side = 1,at =1:nr, labels = paste(1:nr), cex.axis = 0.5)
    ####
    if( perc == TRUE){

      axis(2, at = pretty(yLabels), labels = sprintf(round(pretty(yLabels) * 100, 0), fmt="%3.0f%%"), 
           cex.axis = 0.75, cex.lab = 0.75, las = 1)   
    }
    #####
    for (i in 1:n){
      ind = smpc[[i]]$ind[[j]]
      points(ind, A1[ind,i], col = colo[i], pch = pchlist[i])       
      
      if(labelload == TRUE){
        text(ind, A1[ind,i], labels = nam[ind], cex = sizelabelsload, pos = poslabeload)
      }
    }
    abline(h = 0)
    ## determine position of legend, finger crossed
    posle = FALSE
    lp = legend("bottomright", legend = methodsnames[-1], col = colo[1:n], pch = pchlist[1:n], 
                cex = sizelegend, xjust = 0, y.intersp = 0.85/cefx, 
                x.intersp = 0.75/cefy, plot = FALSE)#/par()$cex
    
    M =min(A1)
    m = min(A1[floor((3*nr/4)):nr,])  
    if ((m > (1 + sign(lp$rect$top)* (0.05 )) * lp$rect$top) | all(A1[floor((3*nr/4)):nr,] == 0)){
      posle = "bottomright"
    }
    else{
      lp = legend("topright", legend = methodsnames[-1], col = colo[1:n], pch = pchlist[1:n], 
                  cex = sizelegend, xjust = 0, y.intersp = 0.85/cefx, 
                  x.intersp = 0.75/cefy, plot = FALSE)#/par()$cex
      M =max(A1)
      m = max(A1[floor(3*nr/4):nr,])
      
      if ((m < (1 - sign(lp$rect$top) * (0.05  )) * (lp$rect$top - sign(lp$rect$top) * lp$rect$h)) | all(A1[floor(3*nr/4):nr,] == 0)){
        posle = "topright"
      }
      else{
        lp = legend("topleft", legend = methodsnames[-1], col = colo[1:n], pch = pchlist[1:n], 
                    cex = sizelegend, xjust = 0, y.intersp = 0.85/cefx, 
                    x.intersp = 0.75/cefy, plot = FALSE)#/par()$cex
        M =max(A1)
        m = max(A1[1:ceiling(nr/4),])
        if ((m < (1 - sign(lp$rect$top) * (0.05  )) * (lp$rect$top - sign(lp$rect$top) * lp$rect$h)) | all(A1[1:ceiling(nr/4),] == 0)){
          posle = "topleft"
        }
        else{
          lp = legend("bottomleft", legend = methodsnames[-1], col = colo[1:n], pch = pchlist[1:n], 
                      cex = sizelegend, xjust = 0, y.intersp = 0.85/cefx, 
                      x.intersp = 0.75/cefy, plot = FALSE)#/par()$cex
          M =min(A1)
          m = min(A1[(1:ceiling(nr/4)),])  
          if ((m > (1 + sign(lp$rect$top)* (0.05  )) * lp$rect$top) | all( A1[(1:ceiling(nr/4)),] == 0)){
            posle = "bottomleft" ## + ifelse(labelload, 0.1*sizelabelsload, 0)
          }
        }
      }
    }
    #plots legend if posle not = false
    if (!(posle == FALSE))
      legend(posle, legend = methodsnames[-1], col = colo[1:n], pch = pchlist[1:n], 
             cex = sizelegend, xjust = 0, y.intersp = 0.75/cefx, x.intersp = 0.75/cefy)
             #,adj = 1)#/par()$cex, bty = "n"
  }
}  
if(plotvar == TRUE){
  par( mar = c(4.6, 4.1, 1.1, 1.1), mfrow = c(1,1))
  M = max(vexp[1:nd,])
  m = min(vexp[1:nd,])
  ra = c(m, M*(1.05))
  #ra[1] = min(0, ra[1])
  di = diff(ra)/5
  yLabels <- seq(ra[1], ra[2], di)
  plab = pretty(yLabels)
  
  plot(c(1,nd), c(min(plab), max(plab)), type = "n",  xlab = "components", ylab = "Cum. Var. Expl.", 
       xaxt = "n", yaxt = "n")
  axis(side = 1, at = 1:nd)  
  for (i in 1:(n+1))
    lines(1:nd, vexp[1:nd,i], type = "b", col = colo[i], pch = pchlist[i], lty = ((i != 1) + 1) )
  
  
  axis(2, at = pretty(yLabels), labels = sprintf(round(pretty(yLabels) * 100, 0), fmt="%3.0f%%"), 
       cex.axis = 0.75, cex.lab = 0.75, las = 1)          
  
  legend("bottomright", col = colo[1:(n+1)], pch = pchlist[1:(n+1)], lty = 1:(n+1), legend= methodsnames,
         y.intersp = 0.85, cex = 0.85)
}  
## Aall working matrix with ordered laodings for nd components, will retun this  
Aall = matrix(0, nrow = nr, ncol = n *nd) 
donam = rep("A",n*nd)
k = 0
for ( i in 1:nd){
  for (j in 1:n){
    k = k +1
    if (perc == TRUE)
      Aall[,k]  =   smpc[[j]]$loadings[,i]/sum(abs(smpc[[j]]$loadings[,i]))
    else
      Aall[,k] = smpc[[j]]$loadings[,i]
    donam[k] = paste(methodsnames[j+1], paste("C", i, sep = ""), sep = "")
  }
} # perc = FALSE
colnames(Aall) = donam
if (!is.null(rownames(smpc[[1]]$loadings)))
  rownames(Aall) = rownames(smpc[[1]]$loadings)
vxp = c(t(vexp[,-1]))
## A is spca object with  all the loadings order wrt objcet
A = list(loadings = Aall, vexp = vxp)
class(A) = "spca"
nur = (sapply(su, nrow))

if (sum(nur == 9) != n ){
  for (i in which(nur == 9))
    su[[i]] = su[[i]][-8,]
}
nur = min(nur)
Suall = matrix(0, nrow = nur, ncol = n *nd) 
donam = rep("A",n*nd)
k = 0
for ( i in 1:nd)
  for (j in 1:n){
    k = k +1
    Suall[,k] =  su[[j]][1:nur,i]
    if (shortnamescomp == FALSE)
      donam[k] = paste(paste("C", i, sep = ""), methodsnames[j+1],  sep = "-")
    else
      donam[k] = paste(paste("C", i, sep = ""), j, sep = ".") 
  }  
colnames(Suall) = donam
rownames(Suall) = rownames(su[[1]])#[1:nur]    
### if perc
if (perc == TRUE){
  nr = nrow(Suall)
  rownames(Suall)[nr] = "Min %Cont"
  mc = get.minload(Aall) *100
  Suall[nr,] = mc  
}
out = list(loadings = Aall, Summary = Suall, perc = perc)

if(prn == TRUE){
  if (prnload > 0){
    
{
  if(is.numeric(prnload))
    nprl = prnload
  else
    nprl = nd
}
{  
  if (shortnamescomp == FALSE)
    mynam = paste(rep(paste("C",1:nprl, sep = ""), each=n), rep(methodsnames[-1], times = nprl), sep = ".")
  else
    mynam = paste(rep(paste("C",1:nprl, sep = ""), each=n), rep(1:n, times = nprl), sep = ".")      
}
  if (only.nonzero == TRUE)
    only.nonzero = which(apply(abs(A$loadings) > 0, 1, sum)> 0)
  else
    only.nonzero = 1:p


myprintspca(A, cols = 1:(nprl*n), namescomp = mynam, perc = perc, rows = only.nonzero)
writeLines(" ")

  }
fx <- format(Suall, digits = 2, drop0trailing = FALSE, justify = "right", trim = TRUE, scientific = FALSE)
fx[4:5,] = format(round(Suall[4:5,]), digits = 4, drop0trailing = FALSE, justify = "right",
                  trim = TRUE, nsmall=0)  
fx[c(1:3, 6:7),] = format(round(Suall[c(1:3, 6:7),], 5), digits = 4, nsmall = 1, 
                          justify = "right", drop0trailing = FALSE)  
{  if (nur == 9){
  fx[8,] = format(round(Suall[8, ],2), digits = 4, drop0trailing = FALSE, justify = "right")  
  if (perc == FALSE)
    fx[9,] = format(round(Suall[9, ],3), digits = 4, drop0trailing = FALSE, justify = "right", nsmall = 3, scientific = FALSE)
  else
    fx[9,] = paste(round(Suall[9, ],1), "%", sep = "")
}  
else{
  if (perc == FALSE)
    fx[8,] = format(round(Suall[8, ],3), digits = 4, drop0trailing = FALSE, justify = "right", nsmall = 3, scientific = FALSE)
  else
    fx[8,] = paste(round(Suall[8, ],1), "%", sep = "")
}
}
fx = format(fx, justify = "right")
print(paste("Summary statistics"), quote = FALSE)    
print(fx, quote = FALSE, justify = "right")#, ...)
}
on.exit(par(oldpar))
if(rtn)
  return(out)  
else
  invisible()
}
 
#' @return \code{NULL}
#' @rdname compare.spca
#' @export
compare = function(smpc, ...) {
  UseMethod("compare", smpc)
}

compare.default = function(smpc, ...) {
  stop("Nothing to do, compare requires 2 or more spca objects")
  
}

#' Plots loadings and variance explained for an spca object
#' 
#' Plots coefficients and variance explained for spca solutions.
#' 
#' The cumulative variance explained is always plotted together with that
#' explained by the PCs.  The loadings are plotted as barplots. For large
#' matrices it is reccommended to set onlynonzero = TRUE and variablesnames = F. The
#' plots of the sparse loadings versus the PC's ones are marked with the line
#' of equality of the PCs ones.
#' 
#' @param x An spca object.
#' @param cols The number of components to be plotted. Default all.  If an
#' iteger is passed, it is set to 1:cols.
#' @param plotvexp Logical: should the cumulative variance explained be
#' plotted?
#' @param methodname Name of the method. If FALSE set to LS SPCA
#' @param plotload Logical: should the loadings be plotted?
#' @param thresh Real: value below this are considered zero and not plotted. It 
#' \emph{thresh}> 0.001 it is effective regardless of the value of \emph{onlynonzero}.
#' @param perc Logical: should the loading be scaled as percentages?
#' @param variablesnames names of the variables to use in plot of loadings.  If FALSE,
#' names are set to V1, V2,... If TRUE the rownames of the matrix of loadings
#' are used.
#' @param onlynonzero Logical: should only the non-zero loadings be plotted?
#' @param plotloadvsPC Logical: if TRUE the sparse loadings are plotted versus
#' the corresponding PCA ones.
#' @param pcs An spca object containing the PCA loadings, typically obtained
#' with the function pca.
#' @param addlabels Hybrid: if TRUE the nonzero loadings in the plotloadvsPC
#' and plotload plots are labelled with short names V1, V2,..., if equal to
#' "orig" the original variables names are used as labels, if FALSE no labels
#' are added.
#' @param mfrowload Number of loadings plots per row.
#' @param mfcolload Number of loadings plots per column.
#' @param bnw Logical: should the plots be in black and white?
#' @param rotlabels Angle for the rotation of the labels, see \emph{srt} in \code{\link[graphics]{par}}.
#' @param sizelabels Magnification of the labels, see \emph{cex} in \code{\link[graphics]{par}}.
#' @param ... Additonal arguments for generic plot. Disabled, additional arguments will
#' generate an error.
#' @return None
#' @note The value of \emph{thresh} must be chosen according to the value of \emph{perc}.\cr
#' The "dots" are disabled so that only exact (or partial) prescribed arguments 
#' can be entered.\cr 
#' The plots are not very customisable. Personalised plots can be easily
#' produced from the spca object.
#' @seealso Examples in \code{\link{spcabe}} and \code{\link{spcabb}}. For plotting two or
#' more spca solutions together see \code{\link{compare}}.
#' @export plot.spca
plot.spca = function(x, cols, plotvexp = TRUE, methodname= FALSE, plotload = FALSE,  
                     thresh = 0.001, perc = TRUE, variablesnames = FALSE, onlynonzero = TRUE,  
                     plotloadvsPC = FALSE, pcs = NULL, addlabels = TRUE,
                     mfrowload = 1, mfcolload = 1, bnw = FALSE, rotlabels = 0, 
                     sizelabels = 1, ...){
## -----------------------------------------------------------------------
## generic S3 method that plots varexplained and loadings

goodarg = as.list(environment())
badarg = eval(substitute(alist(...)))
if (length(badarg) > 0){
  stop(paste0("\nUnused arguments: ", paste(names(badarg), collapse=", ")))
  #  (call. = FALSE)
}
smpc = x
  oldpar = par(no.readonly = TRUE)
  if (!any(class(smpc) == "spca")){
    stop("plot.spca works only for spca objects")
  } 
  else
    if (perc == FALSE)
      A = smpc$loadings    
  else    
    if(is.null(smpc$contributions))
      A = make.cont(smpc)#sweep(, 2, apply(abs(A),2, sum), "/") *100
  else
    A = smpc$contributions
  p = nrow(A)
  
  if (missing(cols))
    cols = 1:length(smpc$vexpPC)
  else
    if (length(cols) == 1L)
      cols = 1:cols
  if (rev(cols)[1] > ncol(A))
    cols = 1:ncol(A)
  if (missing(methodname))
    methodname = c("PCA", "SPCA")
  else
    methodname = c("PCA", methodname)
  
  ## --------------------- plot loadings ---------------  
  nc = length(cols)
  
  ## variable names for plotting loadings  
  if (is.factor(variablesnames))
    variablesnames = as.character(variablesnames)
  if (length(variablesnames) > 1)
    if (length(variablesnames) < p)
      stop("too few names passed in variablesnames ")
    else
      varnames = variablesnames[1:p]
  else{
    if (variablesnames[1] == FALSE){
    varnames = paste("V", 1:p, sep = "")
  }
  else{
    if (variablesnames[1] == TRUE & !is.null(rownames(A)))
      varnames = rownames(A)
    else {      
      warning("something wrong in variablesnames argument. Set to default")
      varnames = paste("V", 1:p, sep = "")
      
    }

  }  
  }
  ###
  if (plotload == TRUE){    
    nf = layout(matrix(1:(mfrowload * mfcolload), nrow = mfrowload, ncol = mfcolload, byrow = TRUE))
    ####    
    
    for (i in 1:nc){
      if (is.list(smpc$ind)){
        if(thresh > 0.001 | onlynonzero == TRUE)
          indp = which(abs(A[,i]) > thresh ) 
        else
          indp = smpc$ind[[i]]
      }
      else
        indp = 1:p
      par(mar = c(4, 4, 3, 1))
      ra = range(A[indp,i])
      ra[1] = min(0, ra[1])
      
      if (addlabels == TRUE)
        nma = FALSE
      else 
        nma = varnames
      
      bp = barplot(A[indp,i],  density = 25, angle = rotlabels, space = 0.1, ylim = ra + sign(ra)*0.1,
                   main = paste("Comp",i), names.arg = nma[indp],  axes = !perc)
      #      , yaxt="n")
      ## this is vertical adj for lables
      
      if (addlabels == TRUE){
        ad = rep(1.45, length(indp))
        
        ad[A[indp,i] > 0] = -0.45      
        
        if (variablesnames[1] == TRUE) {
          mz = (bp[2] - bp[1])/(2 * 1.2)  
          for(kk in 1:length(indp))
            text(bp[kk] - mz, A[indp[kk],i] , labels = varnames[indp][kk], #
                 srt = rotlabels, adj = c(0, ad[kk]), cex = sizelabels) # + sign(A[indp[kk],i]) * 0.05
        }
        else{
          nn = length(indp)
          
          for(kk in 1:length(indp))
            text(bp[kk], A[indp[kk],i] + sign(A[indp[kk],i]) * 0.002, labels = varnames[indp][kk], #
                 srt = 0, adj = c(0.5, ad[kk]), cex = sizelabels)
          
        }
      }
      ##      done plot and labels, now do axis
      
      ra = range(A[indp,i])
      ra[1] = min(0, ra[1])
      di = diff(ra)/5
      yLabels <- seq(ra[1], ra[2], di)
      #####
      
      if (perc == TRUE)
        axis(2, at = pretty(yLabels), labels = sprintf(round(pretty(yLabels) * 100, 0), fmt="%2.0f%%"), 
             cex.axis = 0.75, cex.lab = 0.75, las = 1)          
      lines(c( 0, bp[length(indp)]), c(0,0), lty = 3)
    }
    
  }
  #### new!! plots loads vs PC's 
  if (plotloadvsPC){
    doplot = TRUE
    if (is.null(pcs) | is.null(pcs$loadings)){
      message("pcs must be the output of pca, the plot of the loadings vs PCs is aborted ")
      doplot = FALSE
    }
    
    ## CAMBIAto PLOTLOAD variablesnames = VARNAMES MAKE ADDLABELS TRUE/FALSE
    if (doplot == TRUE){
       plotload(smpc = smpc, pcs = pcs, cols = cols, perc = perc, variablesnames = varnames, 
                addlabels = addlabels,  
               onlynonzero = FALSE, mfrowload = mfrowload, mfcolload = mfcolload, bnw = bnw)
      
    }
  }
  
  ## plot vexp etc
  if (plotvexp == TRUE){
    vexp = cbind(cumsum(100*smpc$vexpPC[cols]),
                 cumsum(100*smpc$vexp)[cols])
    colnames(vexp) = methodname
    rownames(vexp) = paste("n comps", cols, sep="=")
    M = max(vexp)
    m = min(vexp)
    par(mfrow = c(1,1), mar = c(4.5, 4, 1, 1))

    ## see  http://stackoverflow.com/questions/8736966/r-common-title-and-legend-for-combined-plots
    pchlist = c(1,8)
    if (bnw == TRUE)
      colo = rep(1,2)
    else
      colo = c(1,2)
    yl = range(pretty(m:M, n = 5))
    plot(c(1,max(cols)), c(m,M), type = "n", ylim = yl,
         xlab = "number of components", ylab = "PCVE", xaxt = "n", yaxt = "n")
    axis(side = 1, at = 1:max(cols))
    axis(side = 2, at = pretty(m:M, n = 5), 
         labels = sprintf(pretty(m:M, n = 5), 
                          fmt="%2.0f%%"), cex.axis = 0.75, cex.lab = 0.75, las=1) 
    
    ###
    #,       main = "Cumulated Var. explained")
    for (j in 1:2)
      lines(cols, vexp[,j], col = colo[j], type = "b", pch = pchlist[j], lty = j)
    legend("bottomright", legend = methodname, adj = 0, merge = TRUE, x.intersp = 0.3,
           lty = 1:2, col = colo, pch = pchlist, cex = 0.75, y.intersp= 0.75)  
  }
  
  on.exit(par(oldpar))
  invisible()
}



#' Shows the sparse loadings
#' 
#' Shows the non-zero loadings separately for each component.
#' 
#' Useful for large matrices to see the loadings at the same 
#' time or to assign long descriptive names.
#' 
#' @param smpc A list of spca objects, typically from spcabe and spcabb.  It
#' can also be a simple matrix of loadings.
#' @param cols A vector containg the indices of the loadings to be shown.  Can
#' be a single value. if missing all loadings are shown: If an integer is
#' passed, only that dimension will be returned.
#' @param perc Logical: should the loodings be standardised to unit \eqn{L_1}
#' norm (and printed as percentage contributions).
#' @param digits Number of decimal digits to show.
#' @param  variablesnames Hybrid: if not FALSE, need to pass a vector of varaiable names.
#' @param thresh Loadings with absolute value below this are considered zero.
#' @param rtn Logical: should the text table of loadings and the matrix of
#' summaries be returneded?
#' @details variablesnames must have the names of the p variables in the first p
#' positions.  
#' @return If rtn = TRUE, it returns a list with the loadings.
#' @seealso \link{print.spca}, \link{plot.spca}. Examples in
#' \code{\link{spcabe}}
#' @export showload
showload = function(smpc, cols, perc = TRUE, digits = 3,  variablesnames = FALSE, 
                    thresh = 0.001, rtn = FALSE){
  
## function that prints nonzero loadings one component at the time from an spca object
  
  if(missing(cols)){
    cols = 1:ncol(smpc$loadings)
  }
  if (!any(class(smpc) == "spca")){
    if (is.matrix(smpc) | is.vector(smpc))
      A = as.matrix(smpc)[,cols]
    else
      stop("need an spca object or an array of loadings")
  }
  else{
    A = as.matrix(smpc$loadings[,cols])
    if (length(cols) == 1)
      rownames(A) =  rownames(smpc$loadings)
  }
  if (perc == TRUE)
    A = as.matrix(make.cont(A))
  loads = list()
  if (is.factor(variablesnames))
    variablesnames = as.character(variablesnames)
  if (is.vector( variablesnames) ){ 
    if (length( variablesnames) < nrow(A))
      stop(" variablesnames must be a vector of length equal to number of loadings")
    else
      rownames(A) =  variablesnames[1:nrow(A)]
  }
  for(i in 1:length(cols)){
    loads[[i]] = A[abs(A[,i])> thresh,i]
    print(paste("Component", cols[i]))
    if (perc == TRUE){
      a = paste(round(100 * loads[[i]], max(0,digits-2)), "%", sep = "")
      names(a) = names(loads[[i]])
      #message("Percent Contributions")
      print(a, quote = FALSE, justify = "left")
      writeLines(" ")#paste(, quote = FALSE)
    }
    else{
      #message("Loadings")
      print(loads[[i]], digits = digits, justify = "left")
      writeLines(" ")
    }
  }
  if (rtn == TRUE)
    return(loadings)
  else
    invisible()
  
}

plotload = function(smpc, pcs, cols, perc = TRUE, variablesnames = FALSE, 
                    addlabels = TRUE, onlynonzero = FALSE, mfrowload = 1, 
                    mfcolload = 1, bnw = FALSE){
#### =====================================================================
## called in plot.spca  method for spca objects
## not exported
## plots the sparse loadings versus the PCA ones
#### =====================================================================
  oldpar = par(no.readonly = TRUE)
  if (!any(class(smpc) == "spca")){
    stop("plot.spca works only for spca objects")
  } 
  else
    if (perc == FALSE)
      A = smpc$loadings    
  else    
    if(is.null(smpc$contributions))
      A = make.cont(smpc)#sweep(, 2, apply(abs(A),2, sum), "/") *100
  else
    A = smpc$contributions
  
  p = nrow(A)
  
  if (missing(cols))
    cols = 1:ncol(A)
  else
    if (length(cols) == 1L)
      cols = 1:cols
  if (rev(cols)[1] > ncol(A))
    cols = 1:ncol(A)

  if(perc == TRUE)
    pcs = make.cont(pcs$loadings)
  else
    pcs = pcs$loadings

par(mfrow = c(mfrowload, mfcolload))
  par(mar = c(4.1, 4.1, 2.5, 1))
  if (perc == TRUE)
    na = "contributions"
  else
    na = "loadings"
  if (bnw == TRUE) 
    colo = 1
  else
    colo = "blue"

  for (k in cols){
##    
    if (onlynonzero == TRUE)
      ind = smpc$ind[[k]]
    else
      ind = 1:nrow(A)
    pc = pcs[ind, k]
    sp = A[ind, k]
    sp = sign(cor(pc,sp))*sp
    
    if (addlabels == FALSE)
      plot(pc, sp, pch = 16, col = colo, xlab = "PC", ylab = paste("Sparse", na), 
           xaxt = c("s", "n")[perc+1], yaxt = c("s", "n")[perc+1])
    else{
      indo = smpc$ind[[k]]
      plot(pc, sp, pch = 16, type = "n", col = colo, xlab = "PC", ylab = paste("Sparse", na), 
           xaxt = c("s", "n")[perc+1], yaxt = c("s", "n")[perc+1])
      points(pc[-indo], sp[-indo], pch = 16,  col = colo)
      
      xa = (max(pc) + min(pc))/2
      po = rep(2, length(indo))
      po[pc[indo] < xa] = 4
      nam = variablesnames[indo] 

      points(pc[indo], sp[indo], pch = 16,  cex = 0.75, col = colo)
      text(pc[indo], sp[indo], labels = nam, cex = 0.75, pos = po)#, adj = 0.25)
    }
    
    ###
    if (perc == TRUE){
      M = max(pc*100)
      m = min(pc*100)
      Lab = pretty(m:M, n = 5)/100
      
      axis(side = 1, at = Lab, 
           labels = sprintf(Lab*100, fmt="%2.0f%%"), cex.axis = 0.75, cex.lab = 0.75, las=1) 
      
      M = max(sp*100)
      m = min(sp*100)
      Lab = pretty(m:M, n = 5)/100
      
      axis(side = 2, at = Lab, 
           labels = sprintf(Lab*100, fmt="%2.0f%%"), cex.axis = 0.75, cex.lab = 0.75, las=1) 
    }

    mtext(text = paste("comp", k), line = 0.5, side = 3)
    abline(0,1)
    abline(0,0, lty = 3)
  }
  on.exit(par(oldpar))
  invisible()
}
