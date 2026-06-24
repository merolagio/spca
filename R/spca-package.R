
#' Least Squares Sparse Principal Components Analysis
#'
#' The package provides functions to compute LS-SPCA solutions, where sparsity is
#'  imposed to Pearson's PCA's least-squares reconstruction objective. 
#' 
#' LS-SPCA is different for other SPCA methods that compute sparse PCs with
#'  maximal variance. Details about LS-SPCA can be found in the articles cited below and in the extended vignette.
#'  
#' This release  accompanies the related article and is intended  to support full
#'  reproduction of the results reported therein.
#'
#' Computation relies on efficient C++ routines and includes multiple options
#'  for variable selection and sparse loading estimation.
#'
#' Fitting functions
#' * [spca()] Computes LS-SPCA solutions from a data or covariance/correlation
#'   matrix. Returns an  \link{spca_object} of class `spca`.
#' * [pca()] Computes PCA solutions from a data or covariance/correlation
#'   matrix. Returns an  \link{spca_object} of class `spca`.
#'      
#' S3 methods for objects of class `spca` include:
#' [pca()] returns PCA results as an `spca` object.
#' \strong{methods}
#' * [print()]
#' * [plot()]
#' * [summary()]
#' 
#' \strong{Utilities}
#' * [is.spca()] Verifies if an object inherits from class `spca`.
#' * [compare_spca()] Compares two or more LS-SPCA solutions numerically 
#'   and visually.
#' * [new_spca()] Creates an `spca` object from a set of loadings.
#' * [aggregate_by_group()] Sums loadings or contributions wrt an index vector.
#' * [show_contributions_spca()] Prints the nonzero contributions 
#'   separately for each sPC.
#' * [change_loadings_sign_spca()] Changes the sign of the loadings and all
#'      related elements in an 'spca` object`.
#' * [spca_screeplot()] and  [wachter_qqplot()] Diagnostic plots usefull to 
#'   determine the number of components to retain in PCA.  
#'   
#' @references
#' Merola, G. M. (2015). Least Squares Sparse Principal Component Analysis:
#' a Backward Elimination approach to attain large loadings.
#' \emph{Australia & New Zealand Journal of Statistics}, 57, 391--429.
#' \doi{10.1111/anzs.12128}
#'
#' Merola, G. M. and Chen, G. (2019). Projection sparse principal component
#' analysis: An efficient least squares method. \emph{Journal of Multivariate
#' Analysis}, 173, 366--382. \doi{10.1016/j.jmva.2019.04.001}

#' @useDynLib spca, .registration = TRUE
#' @importFrom Rcpp evalCpp
"_PACKAGE"
NULL

#spca object=================

#' Sparse principal component analysis object
#'
#' Objects of class `spca` are returned by the fitting functions
#' \code{spca()}, \code{pca()} and by \code{new_spca()}..
#'
#' @section Components:
#' An object of class `spca` is a list with the following elements:
#'
#' \describe{
#' \item{loadings}{\eqn{p \times r} matrix of sparse loadings.}
#' \item{contributions}{\eqn{p \times r} matrix of loadings scaled to unit
#'   \eqn{L_1} norm within each sPC.}
#' \item{n_comps}{Number of sPCs.}
#' \item{cardinality}{Number of nonzero loadings in each sPC.}
#' \item{vexp}{Variance explained by each sPC.}
#' \item{vexp_pc}{Variance explained by the corresponding PCs.}
#' \item{cvexp}{Cumulative variance explained by the sPCs.}
#' \item{rvexp}{Ratio of \code{vexp} to the variance explained by the
#'   corresponding PC.}
#' \item{rcvexp}{Ratio of \code{cvexp} to the cumulative variance explained by
#'   the corresponding PCs.}
#' \item{cor_with_pc}{Correlation between each sPC and the
#'   corresponding PC.}
#' \item{tot_var}{Total variance of the data.}
#' \item{loadings_list}{List of nonzero loading vectors, one per sPC.}
#' \item{spc_cor}{\eqn{n_comps \times n_comps} correlation matrix of the 
#'   sPC scores.}
#' \item{indices}{List of variable indices with nonzero loadings, one per sPC.}
#' \item{scores}{Optional matrix of sPC scores, returned only when a data matrix
#'   is supplied.}
#' \item{parameters}{List of parameters used to compute the fit.}
#' \item{call}{Matched call used to compute the fit.}
#' }
#' @name spca_object
#' @family spca
NULL


