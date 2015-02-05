## ----, echo = FALSE, message = FALSE--------------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = TRUE,
  collapse = TRUE)
options(width=100)

## ----ldspca, echo=TRUE, eval=TRUE-----------------------------------------------------------------
library(spca)

## ----vrsn, echo=FALSE, eval=TRUE, comment=""------------------------------------------------------
cat(paste("loaded spca version:", packageVersion("spca")))

## ----data-----------------------------------------------------------------------------------------
data(bsbl_avg)
data(bsbl_labels)
print(bsbl_labels, right = FALSE)

## ----heatmap, fig.width = 6, fig.height = 4-------------------------------------------------------
library(ggplot2)
library(reshape2)
q = qplot(x=Var1, y=Var2, xlab = "", ylab = "", las = 2,
      data=melt(bsbl_avg[,16:1]), fill=value, geom="tile") +
      scale_fill_gradient2(limits=c(-1, 1))
q + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----pca, cache = FALSE, fig.show='hold'----------------------------------------------------------
bc.pca = pca(bsbl_avg, scree = T, kai = T)
#
#<__ names in object -->
names(bc.pca)

## ----plotpca, fig.show = "hold"-------------------------------------------------------------------
#<__ plot the first four loadings
plot(bc.pca, cols = 4, plotvex = FALSE, plotload = TRUE, 
     variables = TRUE, rotlab = 45, size = 0.75,  mfrow = 1, mfcol = 1)

## ----pcatrimmout, fig.show = "hold"---------------------------------------------------------------
plot(bc.pca, cols = 4, thresh = 0.04, plotvex = FALSE, plotload = TRUE, 
     variables = TRUE, rotlab = 45, size = 0.75, mfrow = 1, mfcol = 1)

## ----choosecard, echo = TRUE, tidy = FALSE, fig.show='hold'---------------------------------------
##<__ print and plot the trace of trimming for each component using card 3, 3 and 4 
bc.cc = choosecard(bsbl_avg, unc = FALSE, prntrace = TRUE, cardstoprint = 6, mfrow = 1, mfcol = 1, 
                   interact = c(3))

## ----docc, echo = TRUE, tidy = TRUE---------------------------------------------------------------
##<__ print and plot the trace of trimming for each component using card 3, 3 and 4 
bc.cc = spcabe(bsbl_avg, nd = 3, unc = FALSE, mincard = c(3, 3, 4), msg = FALSE)

## ----lookcc---------------------------------------------------------------------------------------
#<__ print summaries of cc
summary(bc.cc)
##<__ print the contributions__
bc.cc

## ----checkcorcc-----------------------------------------------------------------------------------

##<__ check the correlation among the components
round(bc.cc$cor, 2)

## ----plotcc---------------------------------------------------------------------------------------

## ----plotccpc, echo = TRUE, tidy = TRUE, fig.keep="all", show="hold"------------------------------
##<== Percentage contributions of the the LS SPCA(BE)(3, 4, 4)  
##<== and Cumulative variance explained by  LS SPCA(BE) compared with PCA
plot(bc.cc, plotload = TRUE, methodname = "BE", variablesnames = TRUE, addlabels = TRUE,
        rotlabels = 0, size = 0.75)

##----plot cc pc, echo = TRUE, tidy = TRUE, fig.keep="all", fig.show="hold", fig.cap = "Sparse vs PC contr.", fig.align="center")----
#
##<== Percentage contributions of the the LS SPCA(BE)(3, 4, 4)  
##<== and Cumulative variance explained by  LS SPCA(BE) compared with PCA
plot(bc.cc, plotv = TRUE, plotloadvsPC = TRUE, pcs = bc.pca, variablesnames = TRUE, 
     addlabels = TRUE, rotlabels = 0, size = 0.75)

## ----bbvpv, fig.show = 'hold'---------------------------------------------------------------------
bc.be95 = spcabe(bsbl_avg, nd = 3, threshvaronPC = 0.95)
#
summary(bc.be95)
bc.be95

compare(bc.cc, bc.be95, plotload = TRUE, meth = c("CC", "BE95"), short = FALSE )

## ----bb-------------------------------------------------------------------------------------------
bc.bb = spcabb(bsbl_avg, card = c(3, 3, 4), unc = FALSE)
summary(bc.bb)

compare(bc.cc, bc.bb, plotload = TRUE, meth = c("CC", "BB"), short = FALSE )

## ----subind, echo = TRUE, tidy = TRUE-------------------------------------------------------------
indos = 1: 6
indoc = 7: 13
indds = 14: 16
bc.sub = spcabe(bsbl_avg, nd = 3, threshvaronPC = 0.85, startind = list(indoc, indos, indds), unc = FALSE)

summary(bc.sub)

bc.sub

## ----redtrim--------------------------------------------------------------------------------------
bc.bet = spcabe(bsbl_avg, nd = 3, mincard = c(2, 2, 2), trim = 3, unc = F)

