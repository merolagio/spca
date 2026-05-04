
#' spca: Least Squares Sparse PCA 
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

#' Sparse principal components object
#'
#' Objects of class `spca` returned by the [lsspca()] and [pca()] functions
#'  store sparse loadings, scores, and other values needed to run the methods.
#'
#' @section Components:
#' An object of class `spca` is a list with the following elements:
#'  
#' Essential
#' \describe{
#' \item{\emph{loadings}}{Numeric matrix \eqn{p \times r} of sparse loadings.}
#' \item{\code{contributions}}{Numeric matrix \eqn{p \times r} of contributions (sign convention as stored).}
#' \item{\emph{vexp}}{Vector of variance explained by each sparse component.}
#' \item{\emph{vexpPC}}{a vector of variance explained by the PCs named.} 
#' \item{\emph{ncomps}}{Integer the number of components to compute. If `NULL` all components are computed.}
#' \item{\code{cvvexp}}{a vector of cumulative variance explained.}
#' \item{\code{rpcvexp}}{a vector of recovered variance relative to the corresponding PC.}
#' \item{\code{card}}{integer vector of component cardinalities (number of nonzeros).}
#' \item{\code{loadingslist}}{ list of per-component sparse loading vectors.}
#' \item{\code{corComp}}{Optional \eqn{r \times r} correlation matrix among sparse components.}
#' }
#' Optional
#' \describe{
#' \item{\code{scores}}{Numeric matrix of component scores. Returned only if the data matrix is passed.}
#' }
#'

#' @family spca
#' @name spca_object
NULL
