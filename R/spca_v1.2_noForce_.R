#' Validate inputs for spca
#'
#' Checks user inputs for \code{spca}. This helper only validates inputs.
#' It does not parse strings for C++, change index bases, choose the C++
#' backend, or modify the data matrix.
#'
#' @param M Real matrix or data.frame. Data matrix or covariance/correlation matrix.
#' @param alpha Real in (0, 1]. Target retained proportion.
#' @param ncomps Integer scalar. Maximum number of components. Use 0 for automatic stopping.
#' @param ncomp_byvexp Real in (0, 1].
#' @param method Character vector. Only the first letter of each entry is relevant later.
#' @param var_selection Character vector. Only the first letter of the first entry is relevant later.
#' @param stop_criterion Character vector. Only the first letter of the first entry is relevant later.
#' @param intensive Logical scalar.
#' @param fat_matrix Logical scalar or `NULL`.
#' @param fixedindex_list List of integer vectors, one per component.
#' @param centerdata Logical scalar.
#' @param scaledata Logical scalar.
#' @param PMC Logical scalar.
#' @param epsPMC Numeric scalar.
#' @param maxiterPMC Integer scalar.
#' @param PMVS Logical scalar.
#' @param epsPMVS Numeric scalar.
#' @param maxiterPMVS Integer scalar.
#' 
#' @return Invisible TRUE.
#' @noRd

validate_spca_inputs = function(M,
                                  alpha,
                                  ncomps,
                                  ncomp_byvexp,
                                  method,
                                  var_selection,
                                  stop_criterion,
                                  intensive,
                                  fat_matrix,
                                  fixedindex_list,
                                  centerdata,
                                  scaledata,
                                  PMC,
                                  epsPMC,
                                  maxiterPMC,
                                  PMVS,
                                  epsPMVS,
                                  maxiterPMVS) {
  
  fun_inp = mget(names(formals()), envir = environment(), inherits = FALSE)
  validate_no_na(arg_list = fun_inp)  
  
  if (!is.matrix(M) && !is.data.frame(M))
    stop("M must be a matrix or data.frame", call. = FALSE)

  if (is.data.frame(M) && !all(vapply(M, is.numeric, logical(1))))
    stop("all columns of M must be numeric", call. = FALSE)

  if (is.matrix(M) && !is.numeric(M))
    stop("M must be numeric", call. = FALSE)

  if (length(dim(M)) != 2)
    stop("M must be two-dimensional", call. = FALSE)

  if (anyNA(M))
    stop("M cannot contain missing values", call. = FALSE)

  if ((alpha <= 0) || (alpha > 1))
    stop("alpha must be between 0 and 1", call. = FALSE)

  if (!is.numeric(ncomps) || length(ncomps) != 1 || is.na(ncomps) || (ncomps < 0))
    stop("ncomps must be a nonnegative scalar", call. = FALSE)

    if (!(identical(ncomp_byvexp, FALSE) ||
          (is.numeric(ncomp_byvexp) && length(ncomp_byvexp) == 1 &&
           !is.na(ncomp_byvexp) && (ncomp_byvexp > 0) && (ncomp_byvexp <= 1))))
      stop("ncomp_byvexp must be FALSE or a number in (0, 1]", call. = FALSE)

  if (!is.character(method) || (length(method) < 1))
    stop("method must be a character vector", call. = FALSE)

  method_check = tolower(substr(trimws(method), 1, 1))
  if (!all(method_check %in% c("u", "c", "p")))
    stop("method must start with one of u, c, or p", call. = FALSE)

  if (!is.character(var_selection) || (length(var_selection) < 1))
    stop("var_selection must be a character vector", call. = FALSE)

  var_selection_check = tolower(substr(trimws(var_selection[1]), 1, 1))
  if (!(var_selection_check %in% c("f", "b", "s")))
    stop("var_selection must start with one of f, b, or s", call. = FALSE)

  if (!is.character(stop_criterion) || (length(stop_criterion) < 1))
    stop("stop_criterion must be a character vector", call. = FALSE)

  stop_check = tolower(substr(trimws(stop_criterion[1]), 1, 1))
  if (!(stop_check %in% c("r", "c")))
    stop("stop_criterion must start with one of r or c", call. = FALSE)

  if (!is.boolean(intensive))
    stop("intensive must be TRUE or FALSE", call. = FALSE)

  if (!is.null(fat_matrix) && !is.boolean(fat_matrix))
    stop("fat_matrix must be TRUE, FALSE, or NULL", call. = FALSE)

  if (!is.list(fixedindex_list))
    stop("fixedindex_list must be a list", call. = FALSE)

  if (length(fixedindex_list) > 0) {
    ok_list = vapply(fixedindex_list,
                     function(x) is.null(x) || is.numeric(x) || is.integer(x),
                     logical(1))
    if (!all(ok_list))
      stop("each element of fixedindex_list must be NULL or a numeric/integer vector", call. = FALSE)

    null_ok = vapply(fixedindex_list,
                     function(x) if (is.null(x)) FALSE else anyNA(x),
                     logical(1))
    if (any(null_ok))
      stop("fixedindex_list cannot contain missing indices", call. = FALSE)
  }

  if (!is.boolean(centerdata))
    stop("centerdata must be TRUE or FALSE", call. = FALSE)

  if (!is.boolean(scaledata))
    stop("scaledata must be TRUE or FALSE", call. = FALSE)

  if (!is.boolean(PMC))
    stop("PMC must be TRUE or FALSE", call. = FALSE)

  if (!is.numeric(epsPMC) || length(epsPMC) != 1 || is.na(epsPMC) || (epsPMC <= 0))
    stop("epsPMC must be a positive scalar", call. = FALSE)

  if (!is.numeric(maxiterPMC) || length(maxiterPMC) != 1 || is.na(maxiterPMC) || (maxiterPMC < 1))
    stop("maxiterPMC must be a positive scalar", call. = FALSE)

  if (!is.boolean(PMVS))
    stop("PMVS must be TRUE or FALSE", call. = FALSE)

  if (!is.numeric(epsPMVS) || length(epsPMVS) != 1 || is.na(epsPMVS) || (epsPMVS <= 0))
    stop("epsPMVS must be a positive scalar", call. = FALSE)

  if (!is.numeric(maxiterPMVS) || length(maxiterPMVS) != 1 || is.na(maxiterPMVS) || (maxiterPMVS < 1))
    stop("maxiterPMVS must be a positive scalar", call. = FALSE)

  invisible(TRUE)
}


#' Computes LS SPCA components
#'
#' Computes sparse principal components from either a data matrix or a
#' covariance/correlation matrix. The wrapper routes the computation to the thin or the fat-matrix C++ engine.
#'
#' Character inputs are parsed transparently by their first letter inside the
#' main function. Fixed indices are passed through \code{fixedindex_list}, one
#' list element per component. Different variable selection approaches can be
#' selected, as explained in the `Deatails` section.
#'
#' @param M Real matrix. The \eqn{n \times p} data matrix or the
#'   \eqn{p \times p} covariance/correlation matrix.
#' @param alpha Real in (0,1]. Percentage of variance of the PCs explained by
#'   the sparse component.
#' @param ncomps Number of components to compute (default 0, automatic).
#'   Has priority over `ncomp_byvexp`.
#' @param ncomp_byvexp Real in (0,1]. Stops computating sPCs when the cumulative
#'  vexp reaches this value.
#' @param method Character vector. Partial matching enabled.
#'   Allowed values are `uSPCA`, `cSPCA`, and `pSPCA`.
#'   If fewer than `ncomps` elements are supplied, the last value is used
#'   for all remaining components.
#' @param var_selection Character vector. Only the first letter of the first
#'   element is used: \code{"f"}, \code{"b"}, or \code{"s"}.
#' @param stop_criterion Character vector. Only the first letter of the first
#'   element is used: \code{"r"} or \code{"c"}.
#' @param intensive Logical. For thin matrices only, requests intensive forward
#'   cvexp selection.
#' @param fat_matrix Logical or `NULL`. If `NULL`, the backend is selected
#'   automatically: data matrices with \eqn{n < p} use the fat backend and all
#'   other inputs use the thin backend. If `TRUE`, request the fat-matrix
#'   backend. If `FALSE`, use the thin backend, allowing users to force the thin
#'   path for mildly fat matrices.
#' @param fixedindex_list List of predetermined indices for components.
#'   Each list element is a vector of 1-based indices. Use \code{NULL} or
#'   \code{integer(0)} for a component with no fixed indices. If the list is
#'   shorter than the number of components to be computed, the remaining
#'   components are treated as unconstrained.
#' @param centerdata Logical. Center the columns of \code{M} to zero mean?
#'   Used only when a data matrix is passed.
#' @param scaledata Logical. Scale the columns of \code{M}? Used only when a
#'   data matrix is passed.
#' @param PMC Logical. Use the power method for PCs and sparse-component
#'   eigenvectors.
#' @param epsPMC Numeric. Tolerance for the PMC group.
#' @param maxiterPMC Integer. Maximum iterations for the PMC group.
#' @param PMVS Logical. Use the power method in variable selection.
#' @param epsPMVS Numeric. Tolerance for the PMVS group.
#' @param maxiterPMVS Integer. Maximum iterations for the PMVS group.
#'
#' @details
#' For thin matrices, either the covariance/correlation matrix can be passed. In the former case, the covariance will be computed and assigned to a new matrix.  
#' 
#' @return An object of class `spca`.
#' 
#' @family spca
#' @export
spca = function(M,
                  alpha = 0.95,
                  ncomps = 0,
                  ncomp_byvexp = 0.99,
                  method = "c",
                  var_selection = c("fwd", "bkw", "step"),
                  stop_criterion = c("r2", "cvexp"),
                  intensive = FALSE,
                  fat_matrix = NULL,
                  fixedindex_list = list(),
                  centerdata = FALSE,
                  scaledata = FALSE,
                  PMC = FALSE,
                  epsPMC = 1e-5,
                  maxiterPMC = 100,
                  PMVS = FALSE,
                  epsPMVS = 1e-5,
                  maxiterPMVS = 200) {

  validate_spca_inputs(M = M,
                         alpha = alpha,
                         ncomps = ncomps,
                         ncomp_byvexp = ncomp_byvexp,
                         method = method,
                         var_selection = var_selection,
                         stop_criterion = stop_criterion,
                         intensive = intensive,
                         fat_matrix = fat_matrix,
                         fixedindex_list = fixedindex_list,
                         centerdata = centerdata,
                         scaledata = scaledata,
                         PMC = PMC,
                         epsPMC = epsPMC,
                         maxiterPMC = maxiterPMC,
                         PMVS = PMVS,
                         epsPMVS = epsPMVS,
                         maxiterPMVS = maxiterPMVS)

  if (is.data.frame(M))
    M = as.matrix(M)

  n = nrow(M)
  p = ncol(M)
  
  var_names = colnames(M)
  if(is.null(var_names)) var_names = paste0("V", 1:ncol(M))
  
  method_cpp = tolower(substr(trimws(method), 1, 1))

  var_selection_cpp = switch(tolower(substr(trimws(var_selection[1]), 
                                           1, 1)),
                            "f" = 0,
                            "b" = 1,
                            "s" = 2)

  stop_criterion_cpp = switch(tolower(substr(trimws(stop_criterion[1]), 1, 1)),
                              "r" = 0,
                              "c" = 1)

  is_datamatrix_M = TRUE
  if ((n == p) && isSymmetric(M))
    is_datamatrix_M = FALSE

  if (is.null(fat_matrix)) {
    fat_matrix = (is_datamatrix_M && (n < p))
    if (fat_matrix)
      warning("fat_matrix = NULL selected the fat backend because M is a data matrix with n < p")
  }

  if (isTRUE(fat_matrix)) {
    if (!is_datamatrix_M) {
      warning("fat_matrix = TRUE ignored because the input is a covariance/correlation matrix; using the thin backend")
      use_fat_backend = FALSE
    } else if (n < p) {
      use_fat_backend = TRUE
    } else {
      warning("fat_matrix = TRUE ignored because the data matrix is not fat; using the thin backend")
      use_fat_backend = FALSE
    }
  } else {
    use_fat_backend = FALSE
    if (is_datamatrix_M && (n < p))
      warning("fat_matrix = FALSE forces the thin backend on a fat data matrix; X'X may be singular")
  }

  if (ncomps > 0) {
    ncomps_cpp = ncomps
  } else if (!identical(ncomp_byvexp, FALSE) && (ncomp_byvexp > 0) && (ncomp_byvexp < 1)) {
    ncomps_cpp = if (use_fat_backend) n else p
  } else {
    ncomps_cpp = 0L
  }

  if ((ncomps_cpp > 0) && (length(method_cpp) < ncomps_cpp) && (length(method_cpp) > 0))
    method_cpp = c(method_cpp, rep(method_cpp[length(method_cpp)], ncomps_cpp - length(method_cpp)))

  if ((ncomps_cpp > 0) && (length(method_cpp) > ncomps_cpp))
    method_cpp = method_cpp[seq_len(ncomps_cpp)]

  if (length(fixedindex_list) > 0) {
    if (ncomps_cpp == 0)
      stop("when fixedindex_list is used, ncomps must be positive or ncomp_byvexp must be in (0, 1)", call. = FALSE)

    if (length(fixedindex_list) > ncomps_cpp)
      stop("fixedindex_list cannot be longer than the number of components to be computed", call. = FALSE)

    if (length(fixedindex_list) < ncomps_cpp)
      fixedindex_list = c(fixedindex_list, rep(list(NULL), ncomps_cpp - length(fixedindex_list)))

    index_lengths = vapply(fixedindex_list,
                           function(x) if (is.null(x)) 0L else length(x),
                           integer(1))
    
    fixedindex_list_num = lapply(fixedindex_list,
                                 function(x) if (is.null(x)) integer(0) 
                                 else as.integer(x))
    
    
    indvec_in = as.integer(unlist(fixedindex_list_num)) - 1L
    cardvec_in = as.integer(index_lengths)

    bad_fixed_u = (method_cpp == "u") &
      (cardvec_in > 0) &
      (cardvec_in < seq_len(ncomps_cpp))
    
    if ((length(method_cpp) > 0) && any(bad_fixed_u))
      stop(paste("for uncorrelated components need cardinality not less than component order; too few fixed indices for components",
                 paste(which(bad_fixed_u), collapse = ", ")),
           call. = FALSE)
    
  } else {
    indvec_in = NULL
    cardvec_in = NULL
  }

  if (use_fat_backend && (var_selection_cpp != 0)){
    warning("Only forward variable selection for fat matrices is available. Switching to that ", call. = FALSE)
    var_selection_cpp = 0
    }
  if (use_fat_backend && intensive){
    warning("intensive is currently available only for thin matrices", 
         call. = FALSE)
    intensive = FALSE
    }
  if (intensive && (stop_criterion_cpp != 1)){
    warning("intensive search requires stop_criterion = cvexp. Switching to that.", call. = FALSE)
    stop_criterion_cpp = 1
    }

##  X = M
  if (is_datamatrix_M) {
      if (any(abs(colMeans(M)) > 1e-4)) {
      warning("Centering column means to zero")
      centerdata = TRUE
    }
    if (centerdata || scaledata) {
        M = scaleC(M, centerdata, scaledata)

      }
    S = ataC(M)
  } else 
    if(!use_fat_backend){
    S = M
  }

  if (use_fat_backend) {
    spout = lsspcaTC(X = M,
                     ncomps = ncomps,
                     stop_criterion = stop_criterion_cpp,
                     exact_cvexp = FALSE,
                     alpha = alpha,
                     ncompbycvexp = ncomp_byvexp,
                     method = method_cpp,
                     indvec_in = indvec_in,
                     cardvec_in = cardvec_in,
                     PMPC = PMC,
                     PMS = PMVS,
                     epsPMPC = epsPMC,
                     epsPMS = epsPMVS,
                     maxiterPMPC = maxiterPMC,
                     maxiterPMS = maxiterPMVS,
                     rank_tol = 0.0)
  } else {
## THIN_MATRIX BACKEND    
    selection_method_cpp = var_selection_cpp
    if (intensive)
      selection_method_cpp = 3

    spout = lsspcaC(S = S,
                      ncomps = ncomps,
                      selection_method = selection_method_cpp,
                      stop_criterion = stop_criterion_cpp,
                      exact_cvexp = FALSE,
                      alpha = alpha,
                      ncompbycvexp = ncomp_byvexp,
                      method = method_cpp,
                      indvec_in = indvec_in,
                      cardvec_in = cardvec_in,
                      PMPC = PMC,
                      PMS = PMVS,
                      epsPMPC = epsPMC,
                      epsPMS = epsPMVS,
                      maxiterPMPC = maxiterPMC,
                      maxiterPMS = maxiterPMVS,
                      rank_tol = 0.0)
  }

  if ((ncomps > 0) && (spout$ncomps != ncomps))
    warning(paste("the number of components computed is", spout$ncomps))

  if (is.null(spout$loadings)) {
    stop("backend output does not contain loadings", call. = FALSE)
  }

  if (is.null(spout$card)) {
    stop("backend output does not contain component cardinalities", call. = FALSE)
  }

  if (is.null(spout$ind))
    stop("backend output does not contain selected indices", call. = FALSE)



## loadings=============

  # methods take loadings to be a matrix
  if (is.vector(spout$loadings))
    spout$loadings = matrix(spout$loadings, ncol = 1)
  spout$loadings = apply(spout$loadings, 2, function(x) {
    nz = which(abs(x) > 0)
    if (length(nz) > 0) x * sign(x[nz[1]]) else x
  })
  rownames(spout$loadings) = var_names
  colnames(spout$loadings) = paste0("sPC", 1:spout$ncomps)

#contributions =====================  
  contributions = make_contributions(spout$loadings)
  
# spca methods take contributions to be a matrix
  if (is.vector(contributions))
      contributions = matrix(contributions, ncol = 1)
  dimnames(contributions) = dimnames(spout$loadings)
  
  
  if(is.null(spout$loadlist)){
  spout$loadlist = lapply(seq_len(spout$ncomps), function(i, A) {
    A[A[, i] != 0, i]
  }, A = spout$loadings)
  }
  names(spout$loadlist) = colnames(contributions)
  names(spout$ind) = colnames(contributions)

  ncomps = spout$ncomps
  
  ## gathering values from lsspcaC 
  if(is.null(spout$totvar))
  spout$totvar = sum(spout$vexpPC)
  spout$vexp = spout$vexp/spout$totvar
  spout$cvexp = spout$cvexp/spout$totvar
  spout$vexpPC = spout$vexpPC/spout$totvar

  vexpPC = spout$vexpPC[1:ncomps]
  rvexp =  spout$vexp/vexpPC[1:ncomps]
  rcvexp = spout$cvexp/cumsum(vexpPC[1:ncomps])
  
  parameters = list(
    method = method,
    var_selection = var_selection,
    stop_criterion = stop_criterion,
    intensive = intensive,
    fat_matrix = use_fat_backend
  )

  # OUTPUT =================
  
  
  out = list(loadings = spout$loadings,
             contributions = contributions,
             ncomps = spout$ncomps,
             cardinality = spout$card,
             vexp = spout$vexp,
             vexpPC = spout$vexpPC[1:ncomps],
             cvexp = spout$cvexp,
             rvexp = rvexp,
             rcvexp = rcvexp,
             cor_with_PC = spout$r2,
             total_variance = spout$totvar,
             loadlist = spout$loadlist,
             indices = spout$ind
             )
  ## FIX TIME NAMES
  if (use_fat_backend) {
    out$scores = spout$scores
    if (ncomps > 1){
      out$corComp = cor(spout$scores)
      colnames(out$corComp) = paste0("sPC", seq_len(spout$ncomps))
      rownames(out$corComp) = paste0("sPC", seq_len(spout$ncomps))
    }
    out$parameters = parameters
    out$time_wall = spout$Time_wall
    out$time_colnames = spout$Time_colnames
    out$setup_wall = spout$setup_wall
    out$setup_cpu = spout$setup_cpu
    out$time_unit_raw = spout$time_unit_raw
  }
  else {
# check if exist both scores and corComp add to cpp (done in thin but not checked) add to docs

    if ((is_datamatrix_M)) {
      out$scores = abC(M, spout$loadings[, seq_len(spout$ncomps), 
                                          drop = FALSE])
      
      colnames(out$scores) = paste0("sPC", seq_len(spout$ncomps))
      if (ncomps > 1){
      out$corComp = cor(out$scores)
      colnames(out$corComp) = paste0("sPC", seq_len(spout$ncomps))
      rownames(out$corComp) = paste0("sPC", seq_len(spout$ncomps))
      }
    }
    else{
      out$scores = NULL
      if (ncomps > 1){
        out$corComp = make_corComp_S(spout$loadings, S)
      }
      else 
        out$corComp = NULL
    }
    colnames(spout$Time) = spout$Time_colnames
    out$parameters = parameters
    out$time = spout$Time
    out$timevec = spout$timevec
    out$timecomp = spout$timecomp
  }
  out$Call = match.call()
  class(out) = c(class(out), "spca")
  return(out)
}
