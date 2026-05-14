
#' spca: Least Squares Sparse PCA package
#'
#' Tools to compute LSSPCA solutions.
#'
#' The package provides functions to compute Least Squares Sparse Principal
#'  Components Analysis (LSSPCA) solutions, where sparsity is imposed while
#'  targeting PCA’s least-squares reconstruction objective. This release
#'  accompanies the related article and is intended  to support full
#'  reproduction of the results reported therein.
#'
#' Computation relies on efficient C++ routines and includes multiple options
#'  for variable selection and sparse loading estimation.
#'
#' S3 methods for objects of class `spca` include:
#' [pca()] returns PCA results as an `spca` object.
#' \strong{methods}
#' * [print()]
#' * [plot()]
#' * [summary()]
#' * [is.spca()]
#' 
#' \strong{helpers}
#' * [new_spca()] creates an `spca` object from a set of loadings and the
#'   covariance matrix of the data.
#' * [aggregate_by_group()] computes the sums of the loadings or contributions 
#'     by variable groups.
#' * [showload()] prints, the nonzero loadings or contributions
#'     separately for each sPC.
#'
#' @useDynLib spca, .registration = TRUE
#' @importFrom Rcpp evalCpp
"_PACKAGE"
NULL

#spca object=================

# spca object =================

#' Sparse principal component analysis object
#'
#' Objects of class `spca` are returned by the fitting functions
#' \code{spca()}, \code{pca()} and by #the function code{new-spca()}.
#'
#' @section Components:
#' An object of class `spca` is a list with the following elements:
#'
#' \describe{
#' \item{loadings}{\eqn{p \times r} matrix of sparse loadings.}
#' \item{contributions}{\eqn{p \times r} matrix of loadings scaled to unit
#'   \eqn{L_1} norm within each sPC.}
#' \item{ncomps}{Number of sPCs.}
#' \item{cardinality}{Number of nonzero loadings in each sPC.}
#' \item{vexp}{Variance explained by each sPC.}
#' \item{vexpPC}{Variance explained by the corresponding PCs.}
#' \item{cvexp}{Cumulative variance explained by the sPCs.}
#' \item{rvexp}{Ratio of \code{vexp} to the variance explained by the
#'   corresponding PC.}
#' \item{rcvexp}{Ratio of \code{cvexp} to the cumulative variance explained by
#'   the corresponding PCs.}
#' \item{sq_cor_with_PC}{Squared correlation between each sPC and the
#'   corresponding PC.}
#' \item{tot_var}{Total variance of the data.}
#' \item{loadings_list}{List of nonzero loading vectors, one per sPC.}
#' \item{spc_cor}{\eqn{ncomps \times ncomps} correlation matrix among sPCs.}
#' \item{indices}{List of variable indices with nonzero loadings, one per sPC.}
#' \item{scores}{Optional matrix of sPC scores, returned only when a data matrix
#'   is supplied.}
#' \item{parameters}{List of parameters used to compute the fit.}
#' \item{Call}{Matched call used to compute the fit.}
#' }
#' @name spca-object
#' @aliases spca_object spca-class
#' @family spca
NULL


