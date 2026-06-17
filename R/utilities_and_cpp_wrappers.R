

# R wrappers for exported C++ utilities =======
#
# Naming convention:
# - Matrix multiplication exported h C suffix: 
# ataC(), abC(), etc.
# are named:
#  
#     aat(), ata(), atb(), ab(), abt(), atda(), atdb()
#     av(), atv(), vta(), vtau(), vtv(), vtu()
#
#     eigen_sym() for eigenC()
#     eigenvalues_sym() for eigvalC(eigenvalues.only = true)
#     gen_eigen_sym() for GenEigenC()
#     solve_spd() for solveC()
#
# R wrapper responsibilities:
# - data.frame -> matrix
# - reject NA with anyNA()
# - reject non-matrix/non-vector as appropriate
# - reject non-numeric inputs before sending to C++
# - convert storage mode to double
#
# C++ responsibilities already present:
# - dimension checks for conformable multiplication
# - symmetry checks for eigen helpers
# - LLT/solver/eigen failure guards
# - try/catch wrappers
#
# More functions not calling C++ raw code:
# - var2cor()
# - make_spc_cor_S()
# - make_vexp()
# - scale_columns()
# - standardize_data()
# - get_minload
# - get_card
# - make_contributions


# Matrix products

#' Compute A times t(A)
#'
#' Wrapper around the C++ helper for \eqn{A A^T}.
#'
#' @param A A numeric matrix or data frame.
#'
#' @return A numeric matrix.
#' @noRd
aat = function(A) {
  A = as_numeric_matrix_no_na(A, "A")
  aatC(A)
}

#' Compute t(A) times A
#'
#' Wrapper around the C++ helper for \eqn{A^T A}.
#'
#' @param A A numeric matrix or data frame.
#'
#' @return A numeric matrix.
#' @noRd
ata = function(A) {
  A = as_numeric_matrix_no_na(A, "A")
  ataC(A)
}

#' Compute t(A) times B
#'
#' Wrapper around the C++ helper for \eqn{A^T B}.
#'
#' @param A A numeric matrix or data frame.
#' @param B A numeric matrix or data frame.
#'
#' @return A numeric matrix.
#' @noRd
atb = function(A, B) {
  A = as_numeric_matrix_no_na(A, "A")
  B = as_numeric_matrix_no_na(B, "B")
  atbC(A, B)
}

#' Compute A times B
#'
#' Wrapper around the C++ helper for \eqn{A B}.
#'
#' @param A A numeric matrix or data frame.
#' @param B A numeric matrix or data frame.
#'
#' @return A numeric matrix.
#' @noRd
ab = function(A, B) {
  A = as_numeric_matrix_no_na(A, "A")
  B = as_numeric_matrix_no_na(B, "B")
  abC(A, B)
}

#' Compute A times t(B)
#'
#' Wrapper around the C++ helper for \eqn{A B^T}.
#'
#' @param A A numeric matrix or data frame.
#' @param B A numeric matrix or data frame.
#'
#' @return A numeric matrix.
#' @noRd
abt = function(A, B) {
  A = as_numeric_matrix_no_na(A, "A")
  B = as_numeric_matrix_no_na(B, "B")
  abtC(A, B)
}

#' Compute t(A) times D times A
#'
#' Wrapper around the C++ helper for \eqn{A^T D A}.
#'
#' @param A A numeric matrix or data frame.
#' @param D A numeric matrix or data frame.
#'
#' @return A numeric matrix.
#' @noRd
atda = function(A, D) {
  A = as_numeric_matrix_no_na(A, "A")
  D = as_numeric_matrix_no_na(D, "D")
  atdaC(A, D)
}

#' Compute t(A) times D times B
#'
#' Wrapper around the C++ helper for \eqn{A^T D B}.
#'
#' @param A A numeric matrix or data frame.
#' @param D A numeric matrix or data frame.
#' @param B A numeric matrix or data frame.
#'
#' @return A numeric matrix.
#' @noRd
atdb = function(A, D, B) {
  A = as_numeric_matrix_no_na(A, "A")
  D = as_numeric_matrix_no_na(D, "D")
  B = as_numeric_matrix_no_na(B, "B")
  atdbC(A, D, B)
}

# Matrix-vector products

#' Compute A times v
#'
#' Wrapper around the C++ helper for \eqn{A v}.
#'
#' @param A A numeric matrix or data frame.
#' @param v A numeric vector.
#'
#' @return A numeric vector.
#' @noRd
av = function(A, v) {
  A = as_numeric_matrix_no_na(A, "A")
  v = as_numeric_vector_no_na(v, "v")
  avC(A, v)
}

#' Compute t(A) times v
#'
#' Wrapper around the C++ helper for \eqn{A^T v}.
#'
#' @param A A numeric matrix or data frame.
#' @param v A numeric vector.
#'
#' @return A numeric vector.
#' @noRd
atv = function(A, v) {
  A = as_numeric_matrix_no_na(A, "A")
  v = as_numeric_vector_no_na(v, "v")
  atvC(A, v)
}

#' Compute t(v) times A
#'
#' Wrapper around the C++ helper for \eqn{v^T A}.
#'
#' @param v A numeric vector.
#' @param A A numeric matrix or data frame.
#'
#' @return A numeric vector.
#' @noRd
vta = function(v, A) {
  v = as_numeric_vector_no_na(v, "v")
  A = as_numeric_matrix_no_na(A, "A")
  vtaC(v, A)
}

#' Compute t(v) times A times u
#'
#' Wrapper around the C++ helper for \eqn{v^T A u}.
#'
#' @param v A numeric vector.
#' @param A A numeric matrix or data frame.
#' @param u A numeric vector.
#'
#' @return A numeric scalar.
#' @noRd
vtau = function(v, A, u) {
  v = as_numeric_vector_no_na(v, "v")
  A = as_numeric_matrix_no_na(A, "A")
  u = as_numeric_vector_no_na(u, "u")
  vtauC(v, A, u)
}

#' Compute t(v) times v
#'
#' Wrapper around the C++ helper for \eqn{v^T v}.
#'
#' @param v A numeric vector.
#'
#' @return A numeric scalar.
#' @noRd
vtv = function(v) {
  v = as_numeric_vector_no_na(v, "v")
  vtvC(v)
}

#' Compute t(v) times u
#'
#' Wrapper around the C++ helper for \eqn{v^T u}.
#'
#' @param v A numeric vector.
#' @param u A numeric vector.
#'
#' @return A numeric scalar.
#' @noRd
vtu = function(v, u) {
  v = as_numeric_vector_no_na(v, "v")
  u = as_numeric_vector_no_na(u, "u")
  vtuC(v, u)
}

# Other linear algebra helpers

#' Compute the trace of a matrix
#'
#' Wrapper around the C++ trace helper.
#'
#' @param S A numeric matrix or data frame.
#'
#' @return A numeric scalar.
#' @noRd
trace_mat = function(S) {
  S = as_numeric_matrix_no_na(S, "S")
  traceC(S)
}

#returns the eigendecomposition of S
#' Compute a symmetric eigendecomposition
#'
#' Compute eigenvectors and eigenvalues of a symmetric matrix.
#'
#' @param S A numeric symmetric matrix or data frame.
#'
#' @return A list with elements \code{vectors} and \code{values}.
#' @noRd
eigen_sym = function(S) {
  S = as_numeric_matrix_no_na(S, "S")
  out = EigenC(S)
  names(out) = c("vectors", "values")
  out
}

#returns only the eigenvalues of S
#' Compute symmetric eigenvalues
#'
#' Compute only the eigenvalues of a symmetric matrix.
#'
#' @param S A numeric symmetric matrix or data frame.
#'
#' @return A list with element \code{values}.
#' @noRd
eigenvalues_sym = function(S) {
  S = as_numeric_matrix_no_na(S, "S")
  out = comp_eigvalC(S)
  names(out) = "values"
  out
}

## returns the generalized eigendecomposition of Av = l Bv
#' Compute a generalized symmetric eigendecomposition
#'
#' Compute the generalized eigendecomposition \eqn{A v = \lambda B v}.
#'
#' @param A A numeric symmetric matrix or data frame.
#' @param B A numeric symmetric positive-definite matrix or data frame.
#'
#' @return A list with elements \code{vectors} and \code{values}.
#' @noRd
gen_eigen_sym = function(A, B) {
  A = as_numeric_matrix_no_na(A, "A")
  B = as_numeric_matrix_no_na(B, "B")
  out = GenEigenC(A, B)
  names(out) = c("vectors", "values")
  out
}

## inverts a symmetric positive definite matrix
#' Solve a symmetric positive-definite system
#'
#' Invert or solve a symmetric positive-definite matrix through the C++ helper.
#'
#' @param S A numeric symmetric positive-definite matrix or data frame.
#'
#' @return A numeric matrix.
#' @noRd
solve_spd = function(S) {
  S = as_numeric_matrix_no_na(S, "S")
  solveC(S)
}
# transforms a variance/covariance matrix to a correlation matrix
#' Convert a covariance matrix to a correlation matrix
#'
#' Convert a symmetric covariance matrix to the corresponding correlation
#' matrix.
#'
#' @param S A numeric symmetric covariance matrix or data frame.
#'
#' @return A numeric correlation matrix.
#' @noRd
var2cor = function(S) {
  if (is.data.frame(S))
    S = as.matrix(S)
  
  if (!is.matrix(S) || !is.numeric(S))
    stop("S must be a numeric matrix")
  
  if (anyNA(S))
    stop("S must not contain NA values")
  
  if (!isSymmetric(S))
    stop("S must be symmetric")
  
  if (any(diag(S) <= 1e-4))
    stop("S must have non-zero diagonal values")
  
  storage.mode(S) = "double"
  
  var2corC(S)
}


# computes correlations between pairs of sPCs from loadings and covariance
# matrix
#' Compute correlations between sparse principal components
#'
#' Compute correlations between pairs of sparse principal components from a
#' loadings matrix and covariance/correlation matrix.
#'
#' @param A A numeric matrix or data frame of loadings.
#' @param S A numeric symmetric covariance or correlation matrix or data frame.
#'
#' @return A numeric correlation matrix, or \code{NULL} if correlations cannot
#'   be computed.
#' @noRd
make_spc_cor_S = function(A, S) {
  if (is.data.frame(A))
    A = as.matrix(A)
  
  if (is.data.frame(S))
    S = as.matrix(S)
  
  if (!is.matrix(A) || !is.numeric(A))
    stop("A must be a numeric matrix")
  
  if (!is.matrix(S) || !is.numeric(S))
    stop("S must be a numeric matrix")
  
  if (anyNA(A))
    stop("A must not contain NA values")
  
  if (anyNA(S))
    stop("S must not contain NA values")
  
  if (ncol(A) < 2) {
    warning("A must have at least two columns")
    return(NULL)
  }
  
  if (nrow(A) != nrow(S))
    stop("A must have the same number of rows as S")
  
  if (!isSymmetric(S)) {
    warning("S must be symmetric")
    return(NULL)
  }
  
  if (any(diag(S) <= 1e-4)) {
    warning("S must have non-zero diagonal values")
    return(NULL)
  }
  
  storage.mode(A) = "double"
  storage.mode(S) = "double"
  
  makeCorCompC(A, S)
}

# wrapper for C++ function
# computes the extra variance explained by difference
#' Compute variance explained by sparse components
#'
#' Compute variance explained from a loadings matrix and covariance/correlation
#' matrix.
#'
#' @param A A numeric matrix of loadings.
#' @param S A numeric symmetric covariance or correlation matrix.
#'
#' @return Output from the C++ variance-explained helper.
#' @noRd
make_vexp =  function (A, S) {
  { ## should add a flag for unc 
    ## new improved using ind
    ## revised to allow one component only
    ## computes vexp for uncorrelated components
  }
  if ((!is.matrix(A)) || (!is.matrix(S)))
    stop("A and S must be matrices")
  
  if((!isSymmetric(S)))
    stop("S must be a covariance or correlation matrix")
  
  if (nrow(A) != nrow(S))
    stop("A must have the same number of rows as S")
  
  return(make_vexpSC(A, S))
  
}

#' Scale matrix columns
#'
#' Scale each column of a matrix by an L1 or L2 norm.
#'
#' @param M A numeric matrix or data frame.
#' @param normtype A numeric scalar (default \code{2}). Norm type. Supported
#'   values are \code{1} and \code{2}; only the first element is used.
#' @param sig A numeric vector or \code{NULL} (default \code{NULL}). Optional
#'   scaling values. If \code{NULL}, a vector of ones is used.
#'
#' @return A numeric matrix.
#' @noRd
scale_columns = function(M, normtype = 2, sig = NULL){
  if(is.data.frame(M))
    M = as.matrix(M)
  if(!is.matrix(M))
    stop("M must be a matrix or a data.frame")
  if (!(normtype[1] %in% (1:2)))
    stop("normtype must be 1 or 2")
  if (is.null(sig))
    sig = rep(1, ncol(M))
  
  scaleColsC(M, normtype, sig)
}


#' Standardize a data matrix
#'
#' Center and/or scale columns of a matrix through the C++ helper.
#' replicates R's scale without extra copying
#' 
#' @param M A numeric matrix or data frame.
#' @param center A logical value (default \code{TRUE}). If \code{TRUE}, center
#'   columns.
#' @param scale A logical value (default \code{TRUE}). If \code{TRUE}, scale
#'   columns.
#'
#' @return A numeric matrix.
#' @noRd
standardize_data = function(M, center = TRUE, scale = TRUE){
  if(is.data.frame(M))
    M = as.matrix(M)
  if(!is.matrix(M))
    stop("M must be a matrix or a data.frame")
  scaleC(M, center, scale)
}

#' Compute a correlation matrix without extra copying
#'
#' @param X A numeric matrix or data frame.
#' @param center A logical value (default \code{TRUE}). Currently accepted for
#'   compatibility.
#' @param scale A logical value (default \code{TRUE}). Currently accepted for
#'   compatibility.
#'
#' @return A numeric correlation matrix.
#' @noRd
cor_nocopy = function(X, center = TRUE, scale = TRUE){
  if(is.data.frame((x)))
    X = as.matrix(X)
  corC(X)
}

#data manipulation ======
#' Converts a vector to a list
#'
#' Convert a vector of group labels to a list of positions.
#'
#' @param vec A vector of group labels.
#'
#' @return A list with one element per unique value in \code{vec}. Each element
#' contains the positions of that value.
#'
#' @details For example, \code{c(1, 2, 2, 1)} is converted to
#' \code{list(`1` = c(1, 4), `2` = c(2, 3))}.
#'
#' @examples
#' vec = c(1, 2, 2, 1)
#' vec2list(vec)
#' @noRd
vec2list = function(vec) {
  u = sort(unique(vec))
  li = lapply(u, function(x) which(vec == x))
  names(li) = as.character(u)
  li
}

#' Convert a list of indices to a vector
#'
#' Concatenate a list of indices and optionally remove duplicates or sort the
#' result.
#'
#' @param li A list of indices.
#' @param uniq A logical value (default \code{TRUE}). If \code{TRUE}, duplicate
#'   values are removed.
#' @param sorted A logical value (default \code{FALSE}). If \code{TRUE}, the
#'   result is sorted.
#'
#' @return A vector of indices.
#'
#' @details For example, \code{list(c(1, 4), c(2, 3))} is converted to
#' \code{c(1, 4, 2, 3)}.
#' @noRd
list2vec = function(li, uniq = TRUE, sorted = FALSE) {
  a = unlist(li, use.names = FALSE)
  
  if (uniq)
    a = unique(a)
  
  if (sorted)
    a = sort(a)
  
  a
}

#' Convert a factor to a list of positions
#'
#' Convert a factor to a list with one element per level.
#'
#' @param fac A factor.
#'
#' @return A list of positions, named by factor levels.
#' @noRd
fac2list = function(fac) {
  fac = droplevels(fac)
  u = levels(fac)
  li = lapply(u, function(x) which(fac == x))
  names(li) = u
  li
}

#' Convert a list of indices to a factor
#'
#' Convert a list of positions to a factor with one level per list element.
#'
#' @param li A list of index vectors.
#'
#' @return A factor with levels defined by the names of \code{li}, or generated
#' names if \code{li} has no names.
#' @noRd
list2fac = function(li) {
  if (is.null(names(li)))
    namex = paste0("e", seq_along(li))
  else
    namex = names(li)
  
  ind = unlist(li, use.names = FALSE)
  fa = rep(NA_integer_, max(ind))
  
  for (i in seq_along(li))
    fa[li[[i]]] = i
  
  factor(fa, levels = seq_along(li), labels = namex)
}

#' Convert a factor to ordered indices
#'
#' Return positions ordered by factor level.
#'
#' @param fac A factor.
#'
#' @return An integer vector of positions ordered by factor level.
#' @noRd
fac2index = function(fac) {
  fac = droplevels(fac)
  vec = as.numeric(fac)
  (seq_along(fac))[order(vec)]
}

#' Convert a vector to a factor
#'
#' Convert a vector of group labels to a factor with sorted unique values as
#' levels.
#'
#' @param vec A vector of group labels.
#'
#' @return A factor.
#' @noRd
vec2fac = function(vec) {
  u = sort(unique(vec))
  val = match(vec, u)
  factor(val, levels = seq_along(u), labels = as.character(u))
}

#' Round the elements of a list
#'
#' Apply \code{round()} to each element of a list.
#'
#' @param li A list of numeric objects.
#' @param d An integer scalar (default \code{2}). Number of digits.
#'
#' @return A list with rounded elements.
#' @noRd
roundl = function(li, d = 2) {
  lapply(li, round, digits = d)
}

# spca object utilities ====================
# returns the non-zero loading with the smallest absolute value for each column
# input a matrix of loadings or contributions
#' Get minimum nonzero loading magnitudes
#'
#' Compute the smallest absolute nonzero loading or contribution in each column.
#'
#' @param smpc A numeric matrix of loadings or contributions.
#' @param eps A numeric scalar (default \code{1e-4}). Values with absolute
#'   magnitude less than or equal to this threshold are treated as zero.
#'
#' @return A numeric vector with one value per column.
#' @noRd
get_minload = function(smpc, eps = 1e-4){
  if(!is.matrix(smpc))
    stop("get_minload: a matrix of loadings or contributions is needed") 
  gl = function(x)
    min(abs(x[abs(x)> eps]))
  apply(smpc, 2, gl)
}

# returns the cardinality of the columns of a matrix of loadings  
#' Compute loading cardinality
#'
#' Count nonzero entries in an \code{spca} object, matrix, or vector.
#'
#' @param A An \code{spca} object, numeric matrix, or numeric vector.
#' @param thresh_card A numeric scalar (default \code{1e-4}). Values with
#'   absolute magnitude less than or equal to this threshold are treated as
#'   zero.
#'
#' @return An integer count or vector of counts.
#' @noRd
get_card = function(A, thresh_card = 1e-4){
  if (is.spca(A) )
    A = A$loadings
  if(is.vector(A))
    sum(abs(A) > thresh_card)
  else
    if(is.matrix(A))
      colSums(abs(A)> thresh_card)
  else
    stop("A must be an spca object or a matrix or a vector")
  
}


# computes the unit L1 norm contributions from a matrix of loadings
# x can be either the matrix or vector of loadings or list containing a 
# matrix called loadings
#' Compute percentage contribution weights
#'
#' Convert loadings to unit L1-normalized contribution weights.
#'
#' @param x An \code{spca} object, numeric matrix, or numeric vector of
#'   loadings.
#'
#' @return A numeric matrix or vector of contribution weights.
#' @noRd
make_contributions = function(x){
  
  if (is.spca(x) && validate_spca(x))
  {
    return(scale_columns(x$loadings, normtype = 1, sig = NULL))
  }
  if (is.data.frame(x))
    x = as.matrix(x)
  if (is.matrix(x))
    return(scale_columns(x, normtype = 1, sig = NULL))
  
  if (is.vector(x))
    return(x/sum(abs(x)))
  
  stop("x must be an spca object, or a matrix or vector of loadings")
}

#' Compute sparse component scores
#'
#' Compute component scores efficiently from a data matrix and sparse loadings.
#'
#' @param X A numeric data matrix or data frame.
#' @param A A numeric matrix or vector of loadings.
#'
#' @return A numeric matrix of scores.
#' @noRd 
make_scores = function(X, A){
  if (is.data.frame(A))
    A = as.matrix(A)
  if ( is.vector(A))
    A = matrix(A, 1)
  if (!is.matrix(A))
    stop("A must be a matrix of loadings")
  
  if (is.data.frame(X))
    X = as.matrix(X)
  if (!is.matrix(X))
    stop("X must be a matrix of loadings")
  
  # C++ efficient computation of X*A when A is sparse  
  make_scoresC(X, A)
}
