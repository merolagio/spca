
#' Least Squares Sparse Principal Components Analysis
#'
#' The package provides functions to compute LS-SPCA solutions, in which
#' sparsity is imposed on Pearson PCA's least-squares reconstruction objective.
#' 
#' LS-SPCA differs from SPCA methods that compute sparse PCs by maximizing
#' variance. Details are provided in the references below and in the extended
#' vignette.
#'  
#' This release accompanies the related article and supports reproduction of
#' the results reported therein.
#'
#' Computation relies on efficient C++ routines and includes multiple options
#'  for variable selection and sparse weight estimation.
#'
#' strong{Fitting functions}
#' * [spca()] Computes LS-SPCA solutions from a data or covariance/correlation
#'   matrix. It returns a \link{spca_object} of class `spca`.
#' * [pca()] Computes PCA solutions from a data or covariance/correlation
#'   matrix. It returns a \link{spca_object} inheriting from classes `spca_pca`
#'   and `spca`.
#'
#' \strong{Methods}
#' * [print()], [summary()], and [plot()] inspect and display `spca` objects.
#' * [change_sign()] changes the signs of selected components and their related
#'   object elements.
#' * [show_weights()] prints or returns the nonzero weights or contributions for
#'   selected components.
#' * [aggregate_by_group()] aggregates weights or contributions according to a
#'   grouping vector.
#' * [screeplot_spca()] and [qqplot_spca()] provide diagnostic plots for objects
#'   returned by [pca()].
#' 
#' \strong{Utilities}
#' * [is.spca()] Verifies whether an object is an `spca` object.
#' * [compare_spca()] Compares two or more LS-SPCA solutions numerically 
#'   and visually.
#' * [new_spca()] Creates an `spca` object from a set of weights.
#'
#' The former interfaces [change_weights_sign_spca()],
#' [change_loadings_sign_spca()], [spca_screeplot()], and [wachter_qqplot()] are
#' retained for backward compatibility and issue deprecation warnings. Objects
#' created by previous package versions with `loadings` and `loadings_list`
#' elements remain supported.
#'   
#' @references
#' Merola, G. M. (2015). Least Squares Sparse Principal Component Analysis:
#' a Backward Elimination approach to attain large weights.
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

#' Sparse Principal Component Analysis Object
#'
#' Objects of class `spca` are returned by the fitting functions
#' [spca()], [pca()], and [new_spca()]. Objects returned by [pca()] also inherit
#' from class `spca_pca`.
#'
#' @section Components:
#' An object of class `spca` is a list with the following elements:
#'
#' \describe{
#' \item{weights}{\eqn{p \times r} matrix of sparse weights.}
#' \item{contributions}{\eqn{p \times r} matrix of weights scaled to unit
#'   \eqn{L_1} norm within each sPC.}
#' \item{n_comps}{Number of sPCs.}
#' \item{cardinality}{Number of nonzero weights in each sPC.}
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
#' \item{weights_list}{List of nonzero weight vectors, one per sPC.}
#' \item{spc_cor}{\eqn{n_comps \times n_comps} correlation matrix of the 
#'   sPC scores.}
#' \item{indices}{List of variable indices with nonzero weights, one per sPC.}
#' \item{scores}{Optional matrix of sPC scores, returned only when a data matrix
#'   is supplied.}
#' \item{parameters}{List of parameters used to compute an [spca()] fit.}
#' \item{call}{Matched call used to compute an [spca()] fit.}
#' \item{eigenvalues}{For [pca()] objects, the available PCA eigenvalues.}
#' \item{n_obs}{For [pca()] objects, the number of observations when available.}
#' \item{method_name}{For [new_spca()] objects, an optional method label.}
#' }
#'
#' For backward compatibility, methods also accept objects from earlier package
#' versions containing `loadings` and `loadings_list` instead of `weights` and
#' `weights_list`.
#' @name spca_object
#' @family spca
NULL


