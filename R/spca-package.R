

#' Anthropometric measures of criminals
#' 
#' This dataset was used for the first application of PCA. It consists of the
#' correlation matrix of seven measures of physical characteristics of a random
#' sample of British criminals. This dataset was used for the first PCA application (by hand!).
#' Useful for testing.
#' 
#' 
#' @name anthrop
#' @docType data
#' @format A 7 by 7 correlation matrix.
#' \describe{
#' \item{\code{Head Length}}{}
#' \item{\code{Head Breadth}}{} 
#' \item{\code{Face Breadth}}{}
#' \item{\code{Finger}}{}
#' \item{\code{Cubit}}{}
#' \item{\code{Foot}}{}
#' \item{\code{Height}}{}
#' }
#' @references Macdonell, W. (1902). Criminal Anthropometry and the
#' Identification of Criminals. \emph{Biometrika}, 1(2):177-227.
#' @keywords datasets
NULL





#' Baseball hitters career and 1986 season total statistics
#' 
#' Correlation matrix of 16 statistics of major league hitters some of the
#' overall career and others relative to the 1986 season. Available at StatLib.
#' The matrix has a block structure, defined by season offensive play, career 
#' offensive play and season defensive play.
#' 
#' 
#' @name bsbl
#' @docType data
#' @format A \emph{16} by \emph{16} correlation matrix.
#' \describe{
#' \item{\code{TAB_86}}{times at bat in 1986}
#' \item{\code{HIT_86}}{hits in 1986}
#' \item{\code{HR_86}}{home runs in 1986}
#' \item{\code{RUN_86}}{runs in 1986}
#' \item{\code{RB_86}}{runs batted-in in 1986}
#' \item{\code{WAL_86}}{walks in 1986}
#' \item{\code{YC}}{years in the major leagues}
#' \item{\code{TAB}}{times at bat during his career}
#' \item{\code{HIT}}{hits during his career}
#' \item{\code{HR}}{home runs during his career}
#' \item{\code{RUN}}{runs during his career}
#' \item{\code{RUNB}}{runs batted-in during his career}
#' \item{\code{WAL}}{walks during his career}
#' \item{\code{PO_86}}{put outs in 1986}
#' \item{\code{ASS_86}}{assists in 1986}
#' \item{\code{ERR_86}}{errors in 1986}
#' }
#' @source \url{http://lib.stat.cmu.edu/datasets/baseball.data}
#' @keywords datasets
NULL

#' Baseball hitters career and 1986 season average statistics
#' 
#' Same data as above after averaging the career totals with the years in career. \cr
#' The matrix no longer has a block structure.
#'  
#' @name bsbl_avg
#' @docType data
#' @format A \emph{16} by \emph{16} correlation matrix. See 
#' \code{\link{bsbl}} for variables names.
#' @source \url{http://lib.stat.cmu.edu/datasets/baseball.data}
#' @keywords datasets
NULL

#' Baseball hitters statistics labels reference table
#' 
#' This data frame provides descriptive labels for the variables in the
#' bsbl datasets matching the short ones used. 
#'   
#' @name bsbl_labels
#' @docType data
#' @format A \emph{16} by \emph{16} correlation matrix.
#' @source \url{http://lib.stat.cmu.edu/datasets/baseball.data}
#' @keywords datasets
NULL


#' Utilities for computing Sparse Principal Components with the LS SPCA method.
#' 
#' Sparse principal components have few loadings different from zero.  The
#' functions in this package compute the sparse components with the method LS
#' SPCA. These solutions attain the Least Squares approximation to the data
#' using a correlation (or covariance) matrix.\cr \cr The solutions are
#' obtained either through a Branch-and-Bound search (\code{\link{spcabb}}) or
#' a more efficient iterative backward Elimination Algorithm
#' (\code{\link{spcabe}}). \cr\cr If the indices of the sparse loadings are
#' known, the LS SPCA solutions can be computed with \code{\link{spca}}\cr\cr
#' The output is an object of class spca. The minimal spca object contains the
#' following elements: \tabular{ll}{ loadings\tab A matrix with the loadings
#' scaled to unit \eqn{L_2} norm in the columns.\cr vexp\tab A vector with the
#' \% variance explained by each component.\cr vexpv\tab A vector with the \%
#' variance explained by each principal component.\cr ind\tab A list of the indices
#' of the sparse loadings.} The following methods
#' are available \tabular{ll}{ \code{\link{print.spca}}\tab Prints the nonzero
#' loadings\cr \code{\link{plot.spca}}\tab Plots the variance explained and the
#' nonzero loadings\cr \code{\link{summary.spca}}\tab Prints summary statistics
#' of the solutions\cr \code{\link{showload}}\tab shows and plots the spca
#' loadings. Not implemented as an spca method.\cr \code{\link{compare.spca}}\tab
#' Compares different spca objects, giving summaries and plots. Not implemented
#' as an spca method. }
#' 
#' 
#' @name spca-package
#' @docType package
#' @references Giovanni M. Merola. 2014. \emph{Least Squares Sparse Principal 
#' Component Analysis: a Backward Elimination approach to attain large 
#' loadings.} To appear on Austr.&NZ Jou. Stats. Giovanni M. Merola.\cr\cr
#' Giovanni M. Merola. 2014. \emph{Sparse Principal Component Analysis: a
#' Least Squares approximation approach.}  \url{http://arxiv.org/abs/1406.1381}
#' @keywords package
#' @seealso \code{\link{spcabb}} and \code{\link{spcabe} for usage examples}. 
NULL
