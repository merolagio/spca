#' SPCA by Backward Elimination algorithm 
#'  
#' Computes LS SPCA components by iteratively trimming small loadings. 
#'  
#' Sparse loadings are computed by iteratively trimming the ones smaller than 
#' thresh[j] for each component. If \emph{ndbyvexp} < 1, the algorithm will 
#' stop when that percentage of total Vexp is reached with the last component 
#' computed.\cr 
#'  
#' Arguments \emph{threshvar}, \emph{threshvaronPC}, \emph{thresh}, 
#' \emph{excludeload} and \emph{unc} can be entered with fewer elements than 
#' the number of components to compute, \emph{nd}. In this case, or if 
#' \emph{nd} is determined by the variance explained, the missing elements are 
#' set equal to the last one entered (also if just one value is given). The 
#' same is true for \emph{mincard} but for the components required to be 
#' uncorrelated their values are set equal to the order of the component. 
#'  
#' \emph{startind} can be set for the first few components, the following will 
#' be computed on the whole set of variables. 
#'  
#' Trimming stops if mincard[j] is reached. Trimming is controlled in two more 
#' optional ways: if the last trimming caused a loss of variance explained from 
#' the initial solution greater than \emph{threshvar} or the loss of proportion 
#' of total variance explained over the corresponding PCA value drops below the 
#' specified percentage \emph{threshvaronPC}. The rules can be used together, 
#' setting the values to FALSE or 1 to avoid them. 
#'  
#' When \emph{excludeload} = TRUE or \emph{startindex} is set, the cardinality 
#' of the starting indices could be less than the order of the component to 
#' compute. In this case uncorrelatedness cannot be achieved and the component 
#' will be computed as correlated. The flag \emph{converged} will be set to 3 
#' and a warning message printed. 
#'  
#' If vector arguments of length less than the number of components to compute 
#' are passed (hence also if a single one is passed), the last element is 
#' assigned to the missing ones. 
#'  
#' @param S A correlation or covariance matrix. 
#' @param nd Integer. Number of dimensions to compute. If FALSE and ndbyvexp < 
#' 1 the number of components is determined by the latter value. If FALSE and 
#' ndbyvexp = 1 or = FALSE the program will give an error. 
#' @param ndbyvexp Real in [0,1] or FALSE. Minimum percentage of total variance 
#' explained by the components computed. If reached before the specified nd, it 
#' takes priority. 
#' @param mincard Vector of minimal cardinality of each components. If FALSE 
#' and unc[j] = TRUE, the j-th value is set to j, otherwise all values are set 
#' to 1. Takes priority on other controls on trimming. 
#' @param thresh Vector of values below which loadings are trimmed. Can be 
#' shorter than nd. See details. 
#' @param threshvar Vector of minimal percentage of variance loss from the full 
#' initial solution allowed for each component. If reached current trimming is 
#' cancelled and solution returned. If FALSE it is set to 1. 
#' @param threshvaronPC Vector of minimal total percentage of variance loss 
#' from the total variance explained by the PCs allowed to trimming. If reached 
#' current trimming is cancelled and solution returned. If FALSE it is set to 
#' 1. It takes priority over threshvar[j], if both specified. 
#' @param unc Logical vector. If TRUE the corresponding component is computed 
#' uncorrelated, otherwise correlated. Can be shorter than nd. See details. 
#' @param perc Logical: does the threshold refers to the percentage 
#' contributions (the loadings scaled to unitary L1 norm)? 
#' @param startind List of vectors with the initial set of indices for each 
#' component. If NULL, the full set of indices (1:ncol(S)) is assigned to each 
#' component 
#' @param excludeload Logical: vector (length nd or shorter) should the indices 
#' of non-zero loadings in previous components be excluded from future 
#' searches? 
#' @param trim Number of loadings to trim at each iteration. mincard[j] takes 
#' priority if conflicting. 
#' @param reducetrim Logical. If TRUE and trim > 1 when are left less than trim 
#' + mincard[j] loadings, trim is reduced to 1 for these last loadings. 
#' @param diag Logical: should diagnostic output be returned?. 
#' @param choosecard NULL or Integer. Setting the value to an integer makes the 
#' function return a full trace of the elimination for that component.  It is 
#' used by the choosecard function. 
#' @param eps Value below which the absolute value of a loading is considered 
#' zero. 
#' @param msg Logical: should messages be printed after each component is 
#' computed 
#' @return spcabe returns an object of class \emph{spca}. On top of the basic 
#' elements of spca objects, it contains other ones useful for diagnostics and 
#' analysis. Some elements are present only if some of the arguments are 
#' activated. The object contains the following components: 
#' \item{loadings}{Matrix with the loadings scaled to unit \eqn{L_2} norm in 
#' the columns.} If \code{perc = TRUE} \item{contributions}{Matrix of loadings 
#' scaled to unit \eqn{L_1} norm.} \item{vexp}{Vector with the \% variance 
#' explained by each component.} \item{vexpPC}{Vector with the \% variance 
#' explained by each principal component.} \item{cardinality}{Vector with the 
#' cardinalities of each loadings.} \item{ind}{List with the indices of the 
#' non-zero loadings for each component.} \item{unc}{the argument unc passed.} 
#' \item{converged}{Vector with the stop for trimming: 0 by \emph{thresh}, 1 by 
#' \emph{mincard}, 2 by \emph{threshvar} or \emph{threshvaronPC}. The value 3 
#' means that uncorrelatedness could not be achieved because too few indices 
#' were available (see notes).} If any \code{unc[j] = TRUE} 
#' \item{corComp}{Matrix of correlations among the sparse components} 
#' \item{Aunc}{Loadings of components made uncorrelated} If \code{diag == TRUE} 
#' a number of details are returned: \item{vexpo}{Vector with the \% variance 
#' explained by the initial untrimmed components.} \item{totvcloss}{Vector with 
#' the \% loss in total variance explained including each component over that 
#' explained by the corresponding PC (vexpPC - vexp)/vexpPC.} 
#' \item{vlossbe}{Vector with the \% loss in variance explained loss by 
#' trimming over that explained by the initial component (vexpo).} 
#' \item{niter}{Vector with number of iterations for each trimming round.} 
#' \item{eliminated}{List of indices of loadings eliminated for each component} 
#' Call arguments, possibly modified by the algorithm: \item{thresh}{Vector of 
#' tresholds for the size of loadings} \item{threshvar}{Vector of tresholds on 
#' loss of variance explained by each component} \item{ndbyvexp}{Required total 
#' variance explained} \item{stopbyvar}{Logical, did the algorithm terminate 
#' because the required total variance explained was reached?} 
#' \item{mincard}{Minimal cardinalities required} 
#' @seealso \code{\link{spcabb}, \link{summary.spca}, \link{compare.spca}}. 
#' @keywords Backward Elimination 
#' @examples 
#' 
#' \dontrun{
#'   "Note the warnings and messages produced by the examples"
#'   data(anthrop, package = "spca")
#' 
#'   # 3 basic spcabe components with default values, 
#'   # since uncorrelated component these have card = 1, 2, and 3
#'   myspca1 <- spcabe(anthrop, nd = 3)
#'   myspca1
#'   summary(myspca1)
#'   ## plot the results
#'   plot(myspca1, plotload = TRUE, onlynonzero = FALSE, mfrowload = 3, variablesnames = TRUE)
#' 
#'   ## spcabe with 3 components trimmed to different thresholds and mincard
#'   myspca2 <- spcabe(anthrop, nd = 3, thresh = c(0.3, 0.25, 0.15), mincard = c(2,3,3))
#'   summary(myspca2)
#'   myspca2
#'   # show the first two loadings as percentage contributions
#'   showload(myspca2, cols = 1:2, perc = TRUE)
#' 
#'   ## spcabe requirig explaining at least 75% of total variance and that each component
#'   ## explains at least 95% of variance explained by the pcs (see details)
#'   myspca3 <- spcabe( anthrop, ndbyvexp = 0.75, threshvaronPC = 0.95)
#'   summary(myspca3)
#'   myspca3
#'   # compare the three solutions
#'   compare(smpc = myspca1, compareto = list( myspca2, myspca3), 
#'   methodsnames = c("myspca1", "myspca2", "myspca3"))
#'  }
#'  @export spcabe  
spcabe = function(S, nd = FALSE, ndbyvexp = FALSE, mincard = NULL, thresh = FALSE, 
                  threshvar = FALSE, threshvaronPC = FALSE, perc = TRUE, unc = TRUE, 
                  trim = 1, reducetrim = TRUE, startind = NULL, excludeload = FALSE, 
                  diag = FALSE, choosecard = NULL, eps = 1E-4, msg = TRUE){
### ===========================================================================
## spca with loadings trimmed by backward elimination
## iteratively eliminates the coefficients larger than the threshold
### ===========================================================================  
  p = ncol(S)
  # ------------------------------- CHECK AND VARIABLES FOR LOOPING 
  #nd
  if (nd == FALSE){
    if (( ndbyvexp == FALSE) ){
      stop("nd is required if the minimum total variance explained is not specified")  
    }
    else{
      nd = p
    }
  }
  
  ## threshvar
  ## it was originally designed to be the loss of vexp so now 1 -
  threshvar = 1 - threshvar

  if(length(threshvar) == 1)
    threshvar = rep(threshvar[1], nd)
  if(length(threshvar) < nd){
    nm = length(threshvar)
    threshvar[(nm+1):nd] = threshvar[nm]
    print(paste("less than", nd, " threshvar given, last ", nd - nm, "set to ", threshvar[nm]))
  }
  
  ## threshvaronPC
  ## it was originally designed to be the loss of vexp so now 1 -
  threshvaronPC = 1 - threshvaronPC
if (length(threshvaronPC) == 1)
    threshvaronPC = rep(threshvaronPC, nd)
  if (length(threshvaronPC) < nd) {
    nm = length(threshvaronPC)
    threshvaronPC[(nm+1):nd] = threshvaronPC[nm]
    print(paste("less than", nd, " threshvaronPC given, last ", nd - nm, " set to ", threshvaronPC[nm]))
  }
  
  ## if thrshold not specified, it is set to defauLt values
  ## makes thresholding values for each dimension  
  if (length(thresh) == 1){
    if ( thresh == FALSE)
      thresh = 1
    thresh = rep(thresh,nd)
  }
  else 
    if (length(thresh) < nd){
      nm = length(thresh)
      thresh[(nm+1):nd] = thresh[nm]
      print(paste("less than", nd, "thresh, last ", nd - nm, " set to ", thresh[nm]))
    } 
  ## makes uncorrelatedness flag for each dimension  
  if (length(unc) == 1)
    unc = rep(unc[1],nd)
  else 
    if (length(unc) < nd){
      nm = length(unc)
      unc[(nm+1):nd] = unc[nm]
      print(paste("less than", nd, "unc flags given, last ", nd - nm, " set to ", unc[nm]))
    } 
  # startindex for each dimension
  if (!is.null(startind)){
    if (!is.list(startind)){
      li = 1
      startind = list(startind)
    }
    else
      li = length(startind)
    if (li < nd){
      startind = append(startind, lapply(1:(nd-li), function(i, hh) 1:hh, hh = p))
      
    }
  } 
  
  if (is.null(startind)){
    startind = vector("list", nd)
    for (i in 1:nd)
      startind[[i]]= c(1:p)
  }
  # mincard
  if (!is.null(mincard) ) {
    if (length(mincard) < nd){
      mn = length(mincard)
      mincard[(mn+1):nd] = mincard[mn]
    }
    if (any(unc == TRUE)){
      if (any(mincard[unc] < c(1:nd)[unc])){
        mincard[unc][mincard[unc] < c(1:nd)[unc]] = c(1:nd)[unc][mincard[unc] < c(1:nd)[unc]]
      }
    }  
  }## end !is.null(mincard})
  
  if (is.null(mincard)){
    mincard = rep(1, nd)
    if (any(unc) == TRUE)
      mincard[unc] = c(1:nd)[unc] 
  }
  # ------------------------------- INITIALIZE VARIABLES FOR LOOPING
  ## prepares variables for looping
  A = matrix(0, p, nd)
  vexpv =rep(0, nd) #c()
  indlast = as.list(rep(0, nd)) #list()
  Z = diag(1, p)
  converged = rep(0, nd)
  stopbyvar = FALSE
  vexpo = rep(0, nd)
  # check excludeload
  if(length(excludeload) == 1)
    excludeload = rep(excludeload, nd)
  else
    if(length(excludeload) < nd){
      m = length(excludeload)
      excludeload[(m+1):nd] = excludeload[m]
    }    
  ## stopvex not zero if limit variannce loss from untrimmed comp
  ##  reached by elimination
  ## its value is the vloss that would be reached 
  stopvex = rep(0, nd)
  niter = rep(0,nd)
  indused = rep(0, nd) #c()
  changedunc = rep(FALSE, nd)
  eliminated = as.list(rep(0, nd))# list()
  tracing = as.list(rep(0, nd)) #list()
  Trace = as.list(rep(0, nd)) #list()
  # ------------- start looping --------------
  for ( j in 1:nd){  
    # 
    #j = 2
    # --- j > 1 computes Z
    if (j > 1){
      if (unc[j] == TRUE)
        Z = makez(out$a, S, Z)
      else
        Z = makezM(A[,1:j-1], S)
    }
    else{
      ee = eigen(S, symmetric = TRUE)
      vexpPC =   ee$val[1:nd]/sum(ee$val)
    }
    # ---- computes full component j   -------------------------
    indo = startind[[j]]    
    if (j == 1){
      out = cspca(S, indo, vexpn = FALSE)
      vexpo[j] = out$vexp
    }
    else{
      indused = unique(c(indused, indlast[[(j-1)]]))
      if(excludeload[j] == T){  
        indo = indo[!sapply(indo, function(a,b) any(a == b), b = indused)]
      }
      if (length(indo) < j & unc[j] == TRUE){
        warning(paste("the indices available for comp", j, "are too few to enforce unocrrelatedness"))
        unc[j] = FALSE
        changedunc[j] = TRUE
      }    
      if( unc[j] == TRUE){
        out = uspca(S, indo, A[, 1:(j-1)])
        vexpo[j] = out$vexp
      }
      else{
        out = cspca(S, indo, Z, vexpn = FALSE)
        vexpo[j] = out$vexp
      }
    }## end if(j >1)  
    # ---- sets trimming   -------------------------
    k = 0
    ns = length(indo)
    vexpold = out$vexp
    citer = 0
    if (perc == TRUE)
      a = out$a/sum(abs(out$a))
    else 
      a = out$a
    ### trimming loop conditions are:
    ##    any |ai| < thresh[j]
    ##    cardinality >= mincard[j]
    ##    loss of variance explained by trimmed solution not bigger than threshvar[j]   
    trimold = trim  
    elim = c()
    # initialises tracing  
    if(perc == FALSE)
      minl = c( min(abs(a[indo])), min(abs(a[indo])/sum(abs(a[indo]))))
    else
      minl = c(min(abs(a[indo])/sqrt(sum((a[indo])^2))), min(abs(a[indo])))
    
    if(j > 1)
      Trace$Values = c(ns, minl, vexpo[j], (sum(vexpv[1:(j-1)]) + vexpo[j])/sum(vexpPC[1:j]))
    else
      Trace$Values = c(ns, minl, vexpo[j], (vexpo[j])/sum(vexpPC[1:j]))
    Trace$loadings =  a
# ---- trimming   -------------------------
    while (sum(abs(a[abs(a) > eps]) < thresh[j]) > 0 ) 
    {
      if ( (ns - 1) < mincard[j]){
        if (msg == TRUE)
          message(paste("Comp", j, ", reached min cardinality of", mincard[j], ", smallest loading is",  
                        round(min(abs(out$a[abs(out$a) > eps])),3)))
        converged[j] = 1
        break          
      }
      k = k + 1      
      #    if (unc[j] == TRUE)
      if (trim > 1 & (reducetrim > 0))
        if ( ns - trim < mincard[j]){
          trim = reducetrim
          if (msg == TRUE)
            message(paste(paste0("Comp ", j, ": reduced trimming to"), trim, "remaining", ns - mincard[j], "iterations."))
        }  
      nl = max(mincard[j], ns - trim )
      
      outold = out
      indold = indo
      # finds smallest loading
      indb = rev(order(abs(a)))   
      indin = sort(indb[1:nl])
      elim = c(elim, indb[nl + 1])
      indo = (1:p)[indin]
      ns = length(indo)
      # compute trimmed solution
      if (j == 1)
        out = cspca(S, indo, vexpn = FALSE)
      else{
        if( unc[j] == TRUE){
          out = uspca(S, indo, A[, 1:(j-1)])
        }
        else{       
          out = cspca(S, indo, Z, vexpn = FALSE)
        }      
      }  
      ### current vexp    
      vexpo[j] = out$vexp    
      if (perc == TRUE)
        a = out$a/sum(abs(out$a))
      else 
        a = out$a
      ## updates trace    
      if(perc == FALSE)
        minl = c( min(abs(a[indo])), min(abs(a[indo])/sum(abs(a[indo]))))
      else
        minl = c(min(abs(a[indo])/sqrt(sum((a[indo])^2))), min(abs(a[indo])))
      
      #    Trace$Values = rbind(Trace$Values, c(ns, minl, out$vexp))
      ## this is the first comp with all loadings PRCvexp = 1
      if(j > 1)
        Trace$Values = rbind(Trace$Values, c(ns, minl, out$vexp, (sum(vexpv[1:(j-1)]) + out$vexp)/sum(vexpPC[1:j])))
      else
        Trace$Values = rbind(Trace$Values, c(ns, minl, out$vexp, out$vexp/sum(vexpPC[1:j])))
      
      Trace$loadings =  rbind(Trace$loadings, a)
      ### threshvaronPC
      if(threshvaronPC[j] != FALSE & threshvaronPC[j] < 1 ){
        vl = 1 - (sum(vexpv[1:(j-1)]) + out$vexp)/sum(vexpPC[1:j])
        if (vl > threshvaronPC[j]){
          out = outold
          indo = indold
          converged[j] = 2
          #          removes the trvar for this elimination
          Trace$Values = Trace$Values[-nrow(Trace$Values),]
          Trace$loadings = Trace$loadings[-nrow(Trace$loadings),]
          
          break
      }       
    }        
  ### check threshvar
  if (threshvar[j] != FALSE & threshvar[j] < 1){
    vl = (vexpold - out$vexp)/vexpold
    if (vl > threshvar[j]){
      out = outold
      indo = indold
      converged[j] = 2
      break
    }       
  }
  }## end TRIM while
  
  # ---- updates stats for component j   -------------------------
  eliminated[[j]] = elim
  
  A[,j] = out$a
  vexpv[j] = out$vexp
  indlast[[j]] = indo
  niter[j] = k
  trim = trimold  
  
  if (is.matrix(Trace$Values)){
    colnames(Trace$Values) = c("Card", "MinLoad", "MinCont", "PVE", "PRCVE")
    if (is.matrix(Trace$loadings))
      rownames(Trace$loadings) = Trace$Values[,1]
    if (!is.null(rownames(S)))
      colnames(Trace$loadings) = rownames(S)
    else
      if (!is.null(colnames(S)))
        colnames(Trace$loadings) = colnames(S)
  }
  tracing[[j]] = Trace
  
  if (length(indlast[[j]]) < j & changedunc[j] == TRUE)
    converged[j] = 3
  # ---- check if CVEXP reached ndbyvexp   -------------------------
  if( ndbyvexp < 1 & ndbyvexp != FALSE){
    if (sum(vexpv) > ndbyvexp){
      nd = j
      if (msg == TRUE)
        message(paste("reached PCVE", paste(round(sum(vexpv[1:nd]) * 100,1), "%", sep =""),
                      "with", nd, "components"))
      stopbyvar = TRUE
      break
    }
  }
  
  }## end for j in 1:nd
  # ---- tidy up output   -------------------------
  # if stop reached by ndbyvexp trims the results to the new value of nd
  if (ndbyvexp < 1 & ndbyvexp != FALSE){
    A = A[,1:nd]
    vexpv = vexpv[1:nd]
    vexpo = vexpo[1:nd] 
    vexpPC =   ee$val[1:nd]/sum(ee$val)  
    thresh = thresh[1:nd]
    threshvar = threshvar[1:nd]    
    indused =   indused[1 : nd]
    changedunc =	changedunc[1 : nd]
    eliminated =	eliminated[1 : nd]
    tracing = 	tracing[1 : nd]
    Trace = 	Trace[1 : nd]
  }
  if(!is.null(colnames(S)))
    if (nd > 1)
      rownames(A) = colnames(S)
    else
      names(A) = colnames(S)
  else
    if (nd > 1)
      rownames(A) = paste("V", 1:p,sep = "")
  else
    names(A) = paste("V", 1:p,sep = "")
  
  if (nd > 1)
    nomi = rownames(A)
  else
    nomi = names(A)
  # ---- computes final stats for output   -------------------------
  if (all(threshvar == 1) | all(threshvar == FALSE))
    threshvar = FALSE
  if (ndbyvexp == 1)
    ndbyvexp = FALSE
  vlossbe = round(100 * (vexpo - vexpv )/vexpo, 2)
  totvcloss = round(100 * (cumsum(vexpPC) - cumsum(vexpv) )/cumsum(vexpPC), 2)
  card = get.card(A)
  if (!is.null(choosecard )){
    return(tracing[[choosecard]])
  }
  else{ out = list(loadings = A)
        if(perc == TRUE){
          Ap = make.cont(A) #t(t(A)/ apply(abs(A), 2, sum))
          out$contributions = Ap 
        }
        out = c(out, list(vexp = vexpv, vexpPC = vexpPC, cardinality = card, ind = indlast, 
                          unc = unc, converged = converged))
        if (any(unc == FALSE)){
          out$corComp = make.cor(S, A)
          out$Uncloadings = make.uncLoad(A, S)
        }
        if (diag == TRUE){ ## diagnostic output
          eliminated = lapply(eliminated, 
                              function(a, nn){if (length(a) > 0){ names(a) = nn[a]}; return(a)}, nn = nomi)    
          names(eliminated) = paste("Comp", 1:length(eliminated))
          out = c(out, list(vexpo = vexpo, totvcloss = totvcloss, vlossbe = vlossbe, niter = niter, eliminated = eliminated, 
                            thresh = thresh, threshvar = threshvar, threshvaronPC = threshvaronPC, ndbyvexp = ndbyvexp, 
                            stopbyvar = stopbyvar, mincard = mincard, tracing = tracing))
        }
        class(out) = "spca"
        #print("done")
        return(out)
  }
}  

