## ----, echo = FALSE, message = FALSE----------------------------------------------------------------------------------
library(spca)
library(formatR)
library(knitr)
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE)
options(width=120)

## ----spca,   comment = "", echo = FALSE-------------------------------------------------------------------------------
usage(spca)

## ----spcabb, comment = "", echo = FALSE-------------------------------------------------------------------------------
usage(spcabb)

## ----spcabe, comment = "", echo = FALSE-------------------------------------------------------------------------------
usage(spcabe)

## ----pca, comment = "", echo = FALSE----------------------------------------------------------------------------------
usage(pca)

## ----ex, fig.width = 3.5, fig.height = 3.5, fig.cap ="screeplot", fig.align= "center"---------------------------------
library(spca)
cat(paste("loaded spca version:", packageVersion("spca")))
data(bsbl)

#- ordinary PCA
bpca = pca(bsbl, screeplot = TRUE, kaiser.print = TRUE)

## ----exsecond---------------------------------------------------------------------------------------------------------
#- sparse PCA with minimal contribution 25%
bbe1 <- spcabe(bsbl, nd = 4, thresh = 0.25, unc = FALSE)

#- summary output
summary(bbe1)
#-# Explaining over 96% of the PCs' variance with 2, 3, 3 and 1 variables.

#- print percentage contributions
bbe1
#-# Simple combinations of offensive play in career and season are most important. Defensive play in season appears only in 3rd component.

#- The contributions can be printed one by one using the descriptive names in `bsbl_label`
data(bsbl_labels, package = "spca")
head(bsbl_labels)
showload(bbe1, variablesnames = bsbl_labels[,2])
#- plot solution
plot(bbe1, plotloadvsPC = TRUE, pc = bpca, mfr = 2, mfc = 2, 
               variablesnames = as.character(bsbl_labels[,2]))
#-# Explaining the variance pretty closely to PCA with much fewer variables.

#

## ----instc, eval = FALSE----------------------------------------------------------------------------------------------
#  install.packages("spca")

## ----instg, eval = FALSE----------------------------------------------------------------------------------------------
#  if (packageVersion("spca") < 0.4.0) {
#    install.packages("devtools")
#  }
#  devtools::install_github("merolagio/spca")

