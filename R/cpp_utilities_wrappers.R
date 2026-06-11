# R wrappers for exported C++ utilities
# Draft plan saved 2026-05-18
#
# Naming convention:
# - Internal R wrappers to exported C++ helpers with C suffix: 
# ataC(), abC(), etc.
# are named:
#     aat(), ata(), atb(), ab(), abt(), atda(), atdb()
#     av(), atv(), vta(), vtau(), vtv(), vtu()
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

#validation helper for matrices
as_numeric_matrix_no_na = function(x, name) {
  if (is.data.frame(x))
    x = as.matrix(x)
  if (!is.matrix(x))
    stop(name, " must be a matrix or data.frame")
  if (!is.numeric(x))
    stop(name, " must be numeric")
  if (anyNA(x))
    stop(name, " must not contain NA values")
  storage.mode(x) = "double"
  x
}

#validation helper for vectors
as_numeric_vector_no_na = function(x, name) {
  if (!is.vector(x) || is.list(x))
    stop(name, " must be a numeric vector")
  if (!is.numeric(x))
    stop(name, " must be numeric")
  if (anyNA(x))
    stop(name, " must not contain NA values")
  storage.mode(x) = "double"
  x
}

# Matrix products

aat = function(A) {
  A = as_numeric_matrix_no_na(A, "A")
  aatC(A)
}

ata = function(A) {
  A = as_numeric_matrix_no_na(A, "A")
  ataC(A)
}

atb = function(A, B) {
  A = as_numeric_matrix_no_na(A, "A")
  B = as_numeric_matrix_no_na(B, "B")
  atbC(A, B)
}

ab = function(A, B) {
  A = as_numeric_matrix_no_na(A, "A")
  B = as_numeric_matrix_no_na(B, "B")
  abC(A, B)
}

abt = function(A, B) {
  A = as_numeric_matrix_no_na(A, "A")
  B = as_numeric_matrix_no_na(B, "B")
  abtC(A, B)
}

atda = function(A, D) {
  A = as_numeric_matrix_no_na(A, "A")
  D = as_numeric_matrix_no_na(D, "D")
  atdaC(A, D)
}

atdb = function(A, D, B) {
  A = as_numeric_matrix_no_na(A, "A")
  D = as_numeric_matrix_no_na(D, "D")
  B = as_numeric_matrix_no_na(B, "B")
  atdbC(A, D, B)
}

# Matrix-vector products

av = function(A, v) {
  A = as_numeric_matrix_no_na(A, "A")
  v = as_numeric_vector_no_na(v, "v")
  avC(A, v)
}

atv = function(A, v) {
  A = as_numeric_matrix_no_na(A, "A")
  v = as_numeric_vector_no_na(v, "v")
  atvC(A, v)
}

vta = function(v, A) {
  v = as_numeric_vector_no_na(v, "v")
  A = as_numeric_matrix_no_na(A, "A")
  vtaC(v, A)
}

vtau = function(v, A, u) {
  v = as_numeric_vector_no_na(v, "v")
  A = as_numeric_matrix_no_na(A, "A")
  u = as_numeric_vector_no_na(u, "u")
  vtauC(v, A, u)
}

vtv = function(v) {
  v = as_numeric_vector_no_na(v, "v")
  vtvC(v)
}

vtu = function(v, u) {
  v = as_numeric_vector_no_na(v, "v")
  u = as_numeric_vector_no_na(u, "u")
  vtuC(v, u)
}

# Other linear algebra helpers

trace_mat = function(S) {
  S = as_numeric_matrix_no_na(S, "S")
  traceC(S)
}

#returns the eigendecomposition of S
eigen_sym = function(S) {
  S = as_numeric_matrix_no_na(S, "S")
  out = EigenC(S)
  names(out) = c("vectors", "values")
  out
}

#returns only the eigenvalues of S
eigenvalues_sym = function(S) {
  S = as_numeric_matrix_no_na(S, "S")
  out = comp_eigvalC(S)
  names(out) = "values"
  out
}

## returns the generalized eigendecomposition of Av = l Bv
gen_eigen_sym = function(A, B) {
  A = as_numeric_matrix_no_na(A, "A")
  B = as_numeric_matrix_no_na(B, "B")
  out = GenEigenC(A, B)
  names(out) = c("vectors", "values")
  out
}

## inverts a symmetric positive definite matrix
solve_spd = function(S) {
  S = as_numeric_matrix_no_na(S, "S")
  solveC(S)
}


# transforms a variance/covariance matrix to a correlation matrix
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


standardize_data = function(M, center = TRUE, scale = TRUE){
  if(is.data.frame(M))
    M = as.matrix(M)
  if(!is.matrix(M))
    stop("M must be a matrix or a data.frame")
  scaleC(M, center, scale)
}

cor_nocopy = function(X, center = TRUE, scale = TRUE){
  if(is.data.frame((x)))
    X = as.matrix(X, center, scale)
  corC(X)
}
##Other utilities ====================
# returns the non-zero loading with the smallest absolute value for each column
# input a matrix of loadings or contributions
get_minload = function(smpc, eps = 1e-4){
  if(!is.matrix(smpc))
    stop("get_minload: a matrix of loadings or contributions is needed") 
  gl = function(x)
    min(abs(x[abs(x)> eps]))
  apply(smpc, 2, gl)
}

# returns the cardinality of the columns of a matrix of loadings  
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

