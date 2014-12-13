---
title: "spca package"
author: "Giovanni Merola"
output: html_document
---

`spca` is an R package for running Sparse Principal Component Analysis. It implements the LS SPCA approach that computes the Least Squares estimates of sparse PCs ([Merola, 2014. arXiv](http://arxiv.org/abs/1406.1381 "Pre-print")). Unlike other SPCA methods, these solutions maximise the variance of the data explained. 


### SParse Principal Component Analysis
Principal Component Analysis is used for analysing a multivariate dataset with two or three uncorrelated components that explain the most variance of the data. 

In some situations more than three components are used. But this simply reduces a problem into a lower dimensional one, which is still difficult to analyse.

SPCA aims to obtain interpretable components.  In factor Analayis literature there is plenty of discussion about the  definition of *interpretable* and *simple* solutions (as qualities and in mathematical terms). 

* Simplicity can be defined by different measures, being linked to sparsness, variance explained and size of the loadings. 

* Instead, *interpretability* is also linked to which variables are included in the solution  and is not measurable.
    * it usually requires expert knowledge

For these reasons, usually there exist different competing solutions and it is necessary to choose the *best* ones among these. You can think of this as a sort of model selection in regression analysis.

### The package
`spca` is implemented as an **exploratory data analysis** tool so that
different solutions can be computed, printed, plotted and compared. Some of the features are listed below. The cardinality of the components can be chosen interactively after inspecting trace and plots of solutions of different cardinality.

* The solutions can be found throug two greedy algorthms: Branch-and-bound (**BB**) and Backward Elimination (**BB**). 


The code is implemented entirely in R, so it can only be used on medium size problems, sometimes less than 1000 variables, depending on the hardware used. 




a number of 


### The package `**spca**`
The package implements `spca`, which computes the optimal solutions for a given set of indices.

The functions `spcabb` and `spcabe` implement the **BB** and **BE** searches, rispectively.



* The package contains utilities for plotting, printing and comparing spca solutions.

The help for `spcabb` and `spcabe`. There you will find also examples of using other utilities. In the 'Vignettes' you will find a more complete example and details on the methods.

### Methods

- `print`: shows a formatted matrix of sparse loadings or contributions. Contributions are loadings expressed as percentages, while the loadings are scaled to unit sum of squares.

- `showload`: prints only the non-zero sparse loadings. This is useful when the number of loadings is large.

- `summary`: shows formatted summary statistics of a solution

- `plot`: plots the comparison of cumulative variance explained by the sparse solutions versus that explained by the PCs, whish is their upper bound.

- `choosecard`: aids the choice of cardinality by plotting and printing statistics for comparing solutions for different cardinalities.

- `compare`: plots and prints comparison of two or more *spca* objects.

### Future releases

This is the first release and will surely contain some bugs, even though I tried to test it. Please do let me know if you find any bug or can suggest improvements. Please use the *Github* tools for submitting bug reports or contributions.

For now most of the plots are produced with the basic plotting functions. In a later release i will produce them with ggplt2 (requires learning the package better).

I have in mind to develop C routines for the matrix algebra. Anybody willing to help, please, let me know. 

