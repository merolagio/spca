#' Function for choosing the cardinality of the sparse components
#' 
#' Interactive function that produces and plots various statistics relative to
#' different cardinalities of the sparse components.
#' 
#' This function is interactive produces plots relative to the different
#' cardinalities of a component and then asks which cardinality is preferred
#' and computes the next. The process can be stopped by entering adding a
#' decimal value to cardinality of the last component desired. By default the
#' solutions are computed with the BE algorithm (\code{\link{spcabe}}).
#' \emph{prntrace=TRUE} prints the trace of the last \emph{cardstoprint}
#' trimmings, with the variables orderd in elimination order during cardinality
#' selection.  The order may not be univocal when using the BB algorithm.
#' \emph{rtntrace} returns the full trace for all dimensions.  The default
#' settings produce 4 plots. Farcomeni's index is computed as \eqn{vexp(c_j) -
#' \frac{\log(c_j)\bar\sigma^2}{j+1}}, where \eqn{c_j}{c_j} is the cardinality
#' and \eqn{\bar\sigma^2} is the average variance (=1 for correlation
#' matrices). The values of farcomeni's Index and entropy are not returned if
#' their plot is not required.
#' 
#' @param S A correlation (or covariance) matrix.
#' @param method String. Method used to produce solutions, either BE or BB.
#' @param perc Logical: should the loading be scaled as percentages?
#' @param unc Logical vector. If TRUE the corresponding component is computed
#' uncorrelated, otherwise correlated. Can be shorter than nd. See details for
#' \link{spcabe}.
#' @param trim Number of loadings to trim at each iteration.  See details for
#' \link{spcabe}
#' @param reducetrim Logical. If TRUE and trim > 1 when are left less than trim
#' + mincard[j] loadings, trim is reduced to 1 for these last loadings.
#' @param prntrace Logical: should the trace of the trimming be printed?
#' @param cardstoprint Integer: number of cardinalities to print with the
#' trace. If missing all, otherwise only the last \emph{cardstoprint} solutions
#' will be printed.
#' @param interact Hybrid: if TRUE the cardinalities chosen must be entered
#' intearctively.  If a vector of cardinalities is passed the plots and tracies
#' are produced.
#' @param rtntrace Logical: should the trace of the trimming be returned?
#' @param doplot Logical: should the any plotting be done
#' @param plotminload Logical: should the minimum loading (or contribution if
#' perc = TRUE) for each cardinality be plotted?
#' @param plotcvexp string: if set =\emph{abs} the percentage variance
#' explained for each cardinality is plotted. If set =\emph{rel} the percentage
#' cumulative variance explained relative to that explained by the same number
#' of PCs is plotted against the cardinality. If set =FALSE (or anything else)
#' none is plotted.
#' @param plotlovsvexp Logical: should the variance explained for each
#' cardinality be plotted against the minimal loadings?
#' @param plotentropy Logical: should the entropy of the loadings be plotted?
#' @param plotfarcomeni Logical: should the sparsity index proposed by
#' farcomeni be plotted?
#' @param mfrowplot,mfcolplot Integers. The number of rows and columns on which
#' display the plots
#' @param cardstoplot Integer: number of cardinalities to plot. If missing all
#' are plotted
#' @param ce Real > 0. The expansion factor for the plots labels.
#' @return If rtntrace = TRUE a list of matrices of full traces is retuned.
#' @note The plots are not very customisable. Personalised plots can be easily
#' produced from the spca object.
#' @seealso \code{\link{spcabe}}, \code{\link{spcabb}}.
#' @references Giovanni M. Merola. 2014.  Least Squares Sparse Principal
#' Component Analysis: a backward elimination approach to attain large
#' loadings. To appear in Australian and New Zealand Journal of Statistics.
#' @examples
#' \dontrun{
#' data(bsbl)
#' ## run choosecard in non interactive mode
#' ba <- choosecard(bsbl, prntrace = TRUE, cardstoprint = 6, doplot = FALSE, interact = c(3, 3, 4))
#' 
#' # to run in interactive mode replace the interact and doplot arguments with TRUE 
#' # (or remove them from the call altogether). 
#' }
#' @export choosecard
choosecard <- function(S, method = c("BE", "BB"),
                      perc = TRUE, unc = TRUE, trim = 1, reducetrim = TRUE, prntrace = FALSE, 
                      cardstoprint, interact = TRUE, rtntrace = TRUE, doplot = TRUE,  
                      plotminload = TRUE, plotcvexp = c("rel", "abs", FALSE), 
                      plotlovsvexp = TRUE, plotentropy = TRUE, plotfarcomeni = FALSE,
                      mfrowplot = 2, mfcolplot = 2, cardstoplot, ce = 1){
##==============================================================
  ## iterative function to choose the cardinality
  ## calls do card and plot card
##==============================================================
  
  p = NULL
  p = ncol(S)
  nd = p
  if (interact[1] != TRUE)
    if (all(is.numeric(interact)))
      nd = length(interact)
    else
      stop("interact must be TRUE or a vector of cardinalities")
  if ( length(unc) < nd ){
    le = length(unc)
    nm = nd - le
    unc = c(unc, rep(unc[le], nm))
  }
  if (length(rownames(S)) < p){
    rownames(S) = paste("V",1:p, sep = "")
    colnames(S) = paste("V",1:p, sep = "")
  }
  if(method[1] == "BE" | method[1] == "BB")# | method[1] == "PCA")
    method = method[1]
    else
      stop('method must be one of BE or BB)# or PCA')
  if (missing(cardstoplot))
    cardstoplot = p
  else 
    if (!is.numeric(cardstoplot))
      stop("cardstoplot must be either missing or the maximum number of cardinalities to plot")
    else 
      cardstoplot = floor(cardstoplot)
  if (missing(cardstoprint))
    cardstoprint = p
  plotcvexp = plotcvexp[1]

  if (doplot == FALSE){
    plotminload = FALSE
    plotcvexp = FALSE
    plotlovsvexp = FALSE
    plotentropy = FALSE
    plotfarcomeni = FALSE
  }
    
  enough = FALSE
  prevload = list()
  k = 0
  card = c()
  ind = list()
  vexp = c()
  Trace = list()
  tmp = list()
  while (enough == FALSE){
    k = k + 1
    
    if (k > 1)
      prevload = c(prevload, list(tmp$load[tmp$Val[,1] == card[k-1],]))
    
    if (method == "BE"){ ## do BE
      tmp = docard(S, compno = k, prevload = prevload, perc = perc, unc = unc[1:k], trim = trim , 
                 reducetrim =reducetrim  )
      }####
    else
      if (method == "BB") {     # Do BB
      tmp$Values = c()
      tmp$loadings = c()  
      
      nc = seq(p, ifelse(unc[k], k, 1), -trim)
      if (min(nc) > ifelse(unc[k], k, 1))
        nc = c(nc, ifelse(unc[k], k, 1))
      for (i in nc){
        bb = spcabb(S, card = c(card, i), startind = ind, unc = unc, msg = FALSE)
        indo = bb$ind[[k]]
        a = bb$loadings[,k]
        
        contr = a/sum(abs(a[bb$ind[[k]]]))
        minl = c( min(abs(a[bb$ind[[k]]])), min(abs(contr[bb$ind[[k]]])))
        
        tmp$Values = rbind(tmp$Values, c(i, minl, bb$vexp[k], sum(bb$vexp[1:k])/sum(bb$vexpPC[1:k]) ) )
        if (perc == TRUE)
          tmp$loadings = rbind(tmp$loadings,  contr)
        else
          tmp$loadings = rbind(tmp$loadings, a)      
      }  
    } ## end BB
 
    
    #### 
    colnames(tmp$Values) = c( "Card", "MinLoad", "MinCont", "PVE",  "PRCVE")
    rownames(tmp$Values) = paste(tmp$Values[,1])
    rownames(tmp$loadings) = paste(tmp$values[,1])
    colnames(tmp$loadings) = colnames(S)
    cardstoplot = min(nrow(tmp$Values), cardstoplot)
  if (doplot == TRUE){
    plotcard(tmp, compno = k , perc = perc , np = cardstoplot,
             plotminload = plotminload, plotcvexp = plotcvexp , plotlovsvexp = plotlovsvexp , 
             plotentropy = plotentropy, plotfarcomeni = plotfarcomeni, 
             mfrowplot = mfrowplot, mfcolplot = mfcolplot, ce = ce)

    if (plotentropy == TRUE){
      tmp$Values = cbind(tmp$Values, apply(tmp$load, 1, ent))
      colnames(tmp$Values)[ncol(tmp$Values)] = "Entropy"
    }
    if (plotfarcomeni == TRUE){
      tmp$Values = cbind(tmp$Values, farc(tmp, comp = k))
      colnames(tmp$Values)[ncol(tmp$Values)] = "Farcomeni"
    }
}
cardstoprint = min(nrow(tmp$Values), cardstoprint)

    if (prntrace == TRUE){
     message(paste("Trace Comp", k)) 
      myt = print_trace(tmp, comp = k, cards = cardstoprint, rtn = TRUE, perc = perc)## check dgt
    }else
      myt = print_trace(tmp, comp = k, cards = cardstoprint, prn = FALSE, rtn = TRUE, perc = perc)## check dgt

  if (rtntrace == TRUE){
      Trace[[k]] =  myt
      names(Trace)[k] = paste(ifelse(perc, "Contr ", "Load "), "Comp", k, sep ="")
    }
    if(interact[1] == TRUE){
    d <- readline(paste("enter the preferred cardinality for component",k, "(add a decimal to stop) : "))
    d = as.numeric(d)
    if (d < min(tmp$Val[,1]) | d > max(tmp$Val[,1])){
      message(paste("you have to choose a value between", min(tmp$Val[,1]), "and",
                    max(tmp$Val[,1]), "please re-enter a valiud cardinality 
                    or the process will stop"))
      d <- readline(paste("enter the preferred cardinality for component", k, "(add a decimal to stop) : "))
      d = as.numeric(d)
    }
    if (d < min(tmp$Val[,1]) | d > max(tmp$Val[,1])){
      message(paste(" stopping with minimal cardinality of", min(tmp$Val[,1]), 
                    "look at the documentation, maybe you want to set unc = FALSE for this component"))
      d = min(tmp$Val[,1]) + 0.1
    }
    card[k] = floor(d  )
    enough <- ifelse((d - card[k]) != 0, TRUE, FALSE)
    }
  else {### if not interactive
    card[k] = interact[k]
  }
## stops if all comps
  if (k == nd)
    enough = TRUE
  ind = c(ind, list(which( abs(tmp$load[tmp$Val[,1] == card[k],]) > 0.001 )))  
    
    vexp = c(vexp, tmp$Val[tmp$Val[,1] == card[k], 4])
    #tmpout = list(tmpout, paste("comp", k)) = tmp)
    
  }## end while enough
  prevload = c(prevload, list(tmp$load[tmp$Val[,1] == card[k],]))
  if (perc == TRUE){
    contributions = matrix(unlist(prevload), ncol = k)
    loadings = sweep(contributions, 2, sqrt(apply(contributions^2,2, sum)), "/")
  }
  else{
    loadings = matrix(unlist(prevload), ncol = k)
    contributions = sweep(loadings, 2, apply(abs(loadings),2, sum), "/")
  }
  rownames(loadings) = rownames(S)
  rownames(contributions) = rownames(S)
  colnames(loadings) = paste("Comp", 1:k)
  colnames(contributions) = paste("Comp", 1:k)
  vexpPC = eigen(S, only.values = TRUE)$val
  vexpPC = vexpPC[1:k]/ sum(vexpPC)
  if (all(unc==FALSE))
    unc = FALSE
  if (all(unc==TRUE))
    unc = TRUE  
  
  out = list(loadings = loadings, contributions = contributions, vexp = vexp, vexpPC = vexpPC, 
             cardinality = card, ind = ind, unc = unc)
  if (any(unc == FALSE) & k > 1){
    out$corComp = make.cor(S, loadings)
    out$laodingsUnc = make.uncLoad(loadings, S)
  }
  if (rtntrace == TRUE)
    out$Trace = Trace
  class(out) = "spca"
  return(out)
}


docard = function(S, compno, prevload = list(), perc = TRUE, unc = TRUE, trim = 1, reducetrim = TRUE)
{    
##==============================================================
  ##  called by choosecard
  ## computes spcabe for a given comp producing the trace
  ##     prevload = list(prevload)
##==============================================================
if (compno == 2)
    if (length(prevload[[1]]) < ncol(S))
      stop("the whole set of loadings must be passed in prevload")
  if (compno > 2 & length(prevload) < (compno - 1))
    stop("the loadings of all the previous components are required")
  if ( length(unc) < compno ){
    le = length(unc)
    nm = compno - le
    unc = c(unc, rep(unc[le], nm))
  }
  if (compno > 1){  
    startind = lapply(prevload, function(a) which(abs(a) > 0.01))
    mincard = c(sapply(startind, length), ifelse(unc[compno], compno, 1))
    startind = c((startind), list(1:ncol(S)))
  }
  
  if (compno == 1)
    out = spcabe(S, nd = 1, diag = TRUE, choosecard = 1, perc = perc, unc = unc, trim = trim, 
                 reducetrim = reducetrim, msg = FALSE)
  else{
    
    out = spcabe(S, nd = compno, startind = startind, mincard = mincard,
                 diag = TRUE, choosecard = compno, perc = perc, unc = unc, trim = trim, 
                 reducetrim = reducetrim, msg = FALSE)
  }
  
  return(out)  
}

plotcard = function(cdobj, compno = NULL, perc = TRUE, np = TRUE,
                    plotminload = TRUE, plotcvexp = "rel", plotlovsvexp = TRUE, 
                    plotentropy = TRUE, plotfarcomeni = FALSE, pcaload = NULL, scalepca = TRUE,
                    mfrowplot = 2, mfcolplot = 2, ce = 1){
  
##==============================================================
## produces plots for choosecard
##==============================================================
old.par <- par(no.readonly = TRUE) # all par settings which
  if(is.null(compno) )
    par(mar = c(4.5, 4.7, 1, 1))
  else
    par(mar = c(4.5, 4.7, 2, 1))
  q = nrow(cdobj$Val)
  if (np == TRUE)
    np = q
  else 
    if (!is.numeric(np))
      stop("from plotcard: cardstoplot in choosecard must be  TRUE or integer")
  else
    if (np > q){
      warning("something wong with cardstoplot in choosecard or np in plotcard, setting it to max")
      np = q
    }
  np = (q - np + 1) : q
  par(mfrow = c(mfrowplot, mfcolplot))
  
  tv = cdobj$Val
  ## plots minloads or minCont (2nd or 3rd col)
  if (plotminload == TRUE){
    plot(tv[np, 1], tv[np, 2 + perc], type = "l", yaxt = ifelse(perc, "n", "s"), 
         ylab = ifelse(perc, "Mincontr", "MinLoad"), xlab = "cardinality" )
    text(tv[np, 1], tv[np, 2 + perc], tv[np, 1], cex = ce)
    if(perc == TRUE){
      ra = range(tv[np,3])
      ra[1] = min(0, ra[1])
      di = diff(ra)/5
      yLabels <- seq(ra[1], ra[2], di)
      #####
      axis(2, at = pretty(yLabels), labels = sprintf(round(pretty(yLabels) * 100, 0), fmt="%2.0f%%"), 
           cex.axis = 0.75, cex.lab = 0.75, las = 1)          
      abline(h = 1/tv[1,1], lty = 3)
    }
    if (!is.null(compno))
      title(paste("component", compno))
  }
  ## plots cvexp or rel cvexp
  if (plotcvexp == "abs"){
    iv = 4
    labv = "PVE"  
  }
if (plotcvexp == "rel"){
    iv = 5
    labv = "PRCVE"  
}
if (plotcvexp == "abs" | plotcvexp == "rel"){
   
    plot(tv[np, 1], tv[np, iv], type = "l", ylab = labv ,  yaxt = "n", 
         xlab =  "cardinality")
    
    text(tv[np, 1], tv[np,iv], tv[np,1], cex = ce)
    
    ra = range(tv[np, iv])
    #ra[1] = min(0, ra[1])
    if (diff(ra)< 0.015)
      fmt = "%2.1f%%"
    else
      fmt = "%2.0f%%"
    di = diff(ra)/5
    yLabels <- pretty(seq(ra[1], ra[2], di))
    #####
    axis(2, at = yLabels, labels = sprintf(round(yLabels * 100, ifelse(iv == 4, 1, 1)), fmt = fmt), 
         cex.axis = 0.75, cex.lab = 0.75, las = 1)          
    if (!is.null(compno))
      title(paste("component", compno))
  }
  
  ###################  
## plots PCVE carefull with index, tv[,4]
  if (plotlovsvexp == TRUE){
    #    sl = sort(tv[np, 2+perc], decreasing = TRUE)
    plot(tv[np, 2 + perc] , tv[np, 4], type = "l", ylab = "PVE" ,  xaxt = ifelse(perc, "n", "s"), yaxt = "n", 
         xlab = ifelse(perc, "Mincontr", "MinLoad"))
    
    text(tv[np, 2+perc], tv[np, 4], tv[np,1], cex = ce)
    if(perc == TRUE){
      ra = range(tv[np,3])
      ra[1] = min(0, ra[1])
      di = diff(ra)/5
      yLabels <- seq(ra[1], ra[2], di)
      #####
      axis(1, at = pretty(yLabels), labels = sprintf(round(pretty(yLabels) * 100, 0), fmt="%2.0f%%"), 
           cex.axis = 0.75, cex.lab = 0.75, las = 1)          
    }
    
    ra = range(tv[np,4])
    if (diff(ra)< 0.015)
      fmt = "%2.1f%%"
    else
      fmt = "%2.0f%%"
    
    di = diff(ra)/5
    yLabels <- seq(ra[1], ra[2], di)
    #####
    axis(2, at = pretty(yLabels), labels = sprintf(round(pretty(yLabels) * 100, 0), fmt = fmt), 
         cex.axis = 0.75, cex.lab = 0.75, las = 1)          
    if (!is.null(compno))
      title(paste("component", compno))
  }
  ###### pca
  if (length(pcaload) > 1 ){
    if (perc == TRUE)
      pc = sort(abs(pcaload)/sum(abs(pcaload)))
    else
      pc = sort(abs(pcaload)/sqrt(sum(pcaload^2)))
    
    p = length(pc)
    
    if (scalepca == TRUE){
      if (perc == TRUE)
        pc = sapply(1:p, function(i, a){a[i]/sum( abs( a[i:p] ) ) }, a = pc)
      else
        pc = sapply(1:p, function(i, a){a[i]/sqrt(sum(( a[i:p] )^2 )) }, a = pc)
    }
    
    plot(p:1, pc, type = "l", yaxt = ifelse(perc, "n", "s"), 
         ylab = ifelse(perc, "Mincontr PCA", "MinLoad PCA"), xaxt = "n", xlab = "cardinality" )  
    text(p:1, pc, rev(1:length(pc)), cex = ce)
    axis(1, at = pretty(rev(1:p)), labels = round(pretty(rev(1:p)),0))
    if(perc == TRUE){
      ra = range(pc)
      ra[1] = min(0, ra[1])
      di = diff(ra)/5
      yLabels <- seq(ra[1], ra[2], di)
      #####
      axis(2, at = pretty(yLabels), labels = sprintf(round(pretty(yLabels) * 100, 0), fmt="%2.0f%%"), 
           cex.axis = 0.75, cex.lab = 0.75, las = 1)          
      abline(h = 1/length(pc), lty = 3)
    }
    if (!is.null(compno))
      title(paste("PC", compno))
  }
  
  ### plot entropy
  if (plotentropy == TRUE){
    en = apply(cdobj$load[np,], 1, ent, np = np)
    plot(tv[np,1], en, type = "l", xlab = "cardinality" , ylab = "Entropy")
    text(tv[np, 1], en, tv[np, 1], cex = ce)
    if (!is.null(compno))
      title(paste("component", compno))
  }
  
  ### plot farcomeni
  if (plotfarcomeni == TRUE){
    en = farc(cdobj, compno, np)
    plot(tv[np,1], en, type = "l", xlab = "cardinality" , ylab = "Index")
    text(tv[np, 1], en, tv[np, 1], cex = ce)
    if (!is.null(compno))
      title(paste("component", compno))
  }
  
  
  invisible()
  on.exit(par(old.par))
}




farc = function(cdobj, comp, np ) {
  ## co putes farcomeni's index
  ## cdobj is output from docard
  if (missing(np))
    np = nrow(cdobj$Val)
  if (length(np) == 1)
    np = 1:np
  cdobj$Val[np,4] * max(np) - log(cdobj$Val[np,1])/(comp+1)
}
ent = function(a, np){
  ## computes entropy for a vector of loadings
  if (missing(np))
    np = length(a)
  if (length(np) == 1)
    np = 1:np
  
  a = a[which(abs(a) > 0.01)]
  -sum(abs(a) * log(abs(a)))/length(a)
}

print_trace = function(smcc, comp = 1, cards = NULL, prn = TRUE, rtn = FALSE, dgt = 3, perc = TRUE){
  ## prints the trace of choosecard with variables ordered with respect to elimination
  ### =======
  ## smcc output from choosecard
  ## comp is the component to show
  ## cards is the number of cardinalities to print
  ## prn print? shows formatted selection trace
  ## rtn return the (whole) trace?
  ## dgt digits to print (dgt -2 for percentages)
  ## perc = scale laodings to percentage contributions  
  ### =======
  if (!is.null(smcc$Trace))
    G = smcc$Trace[[comp]]
  else 
    G = smcc
  if (is.null(cards) | (cards > nrow(G$Val)))
    cards = nrow(G$Val) 
  
  if (perc == TRUE)
    G$load = sweep(G$load, 1, apply(abs(G$load), 1, sum), "/")
  else
    G$load = sweep(G$load, 1, sqrt(apply(G$load, 1, sum)), "/")
  
#  cc = nrow(G$Val) - apply(abs(G$load) != 0, 2, sum) + 1
#   gl = G$load[, order(cc)]
  cc = ordload(G$load)
  gl = G$load[, cc]
  go = gl[, 1:cards]
####  if (perc == TRUE)

  if (prn == TRUE){
    gp =  go[(nrow(G$Val) - cards + 1):nrow(G$Val),]
    vp = G$Val[(nrow(G$Val) - cards + 1):nrow(G$Val),]
    if (perc == TRUE)
      gx <- format(round(gp*100, max(dgt-2,0)),drop0trailing = TRUE, justify = "left")
    else
      gx <- format(round(gp, dgt),drop0trailing = TRUE, justify = "left")
    nc <- nchar(gx[1L], type = "c")
    gx[abs(gp) < 0.01] <- paste(rep(" ", nc), collapse = "")
    gx = format(gx, justify = "left" )
    ###
    vx <- format(round(vp, dgt), digits = 2, drop0trailing = FALSE, justify = "left")
      vx[, 1] = format(vp[,1], digits = 0, drop0trailing = FALSE, justify = "left")  
    #  vx[, 3] =   paste(round(vp[,3] * 100,1), "%", sep = "")
      vx[, 3:5] = apply(format(round(vp[,3:5] * 100,1), digits = 3, drop0trailing = FALSE), 2, function(a)paste(a, "%", sep = ""))
    ##  
      if (perc == TRUE)
        message("Percent contributions")
      else  
        message("Loadings")
    
      print(cbind(vx, gx)  , quote = FALSE, justify = "left")#, )
  }
  if (rtn == TRUE){
     return(cbind(G$Val, gl))
  }
  else
    invisible()
}


ordload = function(h){
  p = nrow(h)
  q = ncol(h)
  for (i in p:1){
    
    if (i == p){
      indo = which(h[i,] !=0)  
    }
    else{
      indn = (1:q)[-indo]
      indu = which(h[i,-indo] !=0)
      
      indo = c(indo,indn[indu])
    }
  }
  if ( length(indo) < q)
    indo = c(indo, (1:q)[-indo])
  return(indo)
}

# print_ltrace = function(smcc, comptoprint, cardstoprint, dgt = 3, perc = TRUE){
#   
# ##Problem formatting in print_trace assumes different Vals
#   ## prints the traces from spcabe output
#   
#   if (missing(comptoprint))
#     comptoprint = length(smcc)
#   if (length(comptoprint) == 1)
#     comptoprint = 1:comptoprint
#    if (missing(cardstoprint)) 
#      cardstoprint = nrow(smcc[[1]]$Val)
# #   
#     for (j in comptoprint){
#       message(paste("Component", j))
#       print_trace(smcc[[j]],  cards = cardstoprint, prn = TRUE, rtn = FALSE, dgt = dgt, perc = perc)
#         
#     }
#   invisible()
#   
# }
