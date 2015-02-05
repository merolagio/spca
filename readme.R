## ----, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)

## ----example, fig.path='README_files/'-----------------------------------
library(spca)
data(bsbl)
#Ordinary PCA
bpca = pca(bsbl, screeplot = FALSE, kaiser.print = TRUE)

#-Sparse PCA
bbe1 <- spcabe(bsbl, nd = 4, thresh = 0.25, unc = FALSE)

#-summary output
summary(bbe1) 
#-# Explaining over 96% of the variance explained by PCA with 2, 3, 3 and 1 variables.

#-print percentage contributions
bbe1
#-# Simple combinations of offensive play in career and in season are most important.
#-# Defensive play appears only in 3rd component.

#-plot solution
plot(bbe1, plotloadvsPC = TRUE, pc = bpca, mfr = 2, mfc = 2, 
               variablesnames = TRUE)
#-# Explaining the variance pretty closely to PCA with much fewer variables.

