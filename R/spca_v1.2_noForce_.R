#' Validate inputs for spca
#'
#' Checks user inputs for \code{spca}. This helper only validates inputs.
#' It does not parse strings for C++, change index bases, choose the C++
#' backend, or modify the data matrix.
#'
#' @param M Real matrix or data.frame. Data matrix or covariance/correlation matrix.
#' @param alpha Real in (0, 1]. Target retained proportion.
#' @param ncomps Integer scalar. Maximum number of components. Use 0 for automatic stopping.
#' @param ncomp_by_cvexp Real in (0, 1].
#' @param method Character vector. Only the first letter of each entry is relevant later.
#' @param var_selection Character vector. Only the first letter of the first entry is relevant later.
#' @param stop_criterion Character vector. Only the first letter of the first entry is relevant later.
#' @param intensive Logical scalar.
#' @param fat_matrix Logical scalar or `NULL`.
#' @param fixed_index_list List of integer vectors, one per component.
#' @param center_data Logical scalar.
#' @param scale_data Logical scalar.
#' @param pm_loading Logical scalar.
#' @param epspm_loading Numeric scalar.
#' @param maxiterpm_loading Integer scalar.
#' @param pm_varsel Logical scalar.
#' @param epspm_varsel Numeric scalar.
#' @param maxiterpm_varsel Integer scalar.
#' 
#' @return Invisible TRUE.
#' @noRd

validate_spca_inputs = function(M,
                                  alpha,
                                  ncomps,
                                  ncomp_by_cvexp,
                                  method,
                                  var_selection,
                                  stop_criterion,
                                  intensive,
                                  fat_matrix,
                                  fixed_index_list,
                                  center_data,
                                  scale_data,
                                  pm_loading,
                                  epspm_loading,
                                  maxiterpm_loading,
                                  pm_varsel,
                                  epspm_varsel,
                                  maxiterpm_varsel) {
  
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

    if (!(identical(ncomp_by_cvexp, FALSE) ||
          (is.numeric(ncomp_by_cvexp) && length(ncomp_by_cvexp) == 1 &&
           !is.na(ncomp_by_cvexp) && (ncomp_by_cvexp > 0) && (ncomp_by_cvexp <= 1))))
      stop("ncomp_by_cvexp must be FALSE or a number in (0, 1]", call. = FALSE)

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

  if (!is.list(fixed_index_list))
    stop("fixed_index_list must be a list", call. = FALSE)

  if (length(fixed_index_list) > 0) {
    ok_list = vapply(fixed_index_list,
                     function(x) is.null(x) || is.numeric(x) || is.integer(x),
                     logical(1))
    if (!all(ok_list))
      stop("each element of fixed_index_list must be NULL or a numeric/integer vector", call. = FALSE)

    null_ok = vapply(fixed_index_list,
                     function(x) if (is.null(x)) FALSE else anyNA(x),
                     logical(1))
    if (any(null_ok))
      stop("fixed_index_list cannot contain missing indices", call. = FALSE)
  }

  if (!is.boolean(center_data))
    stop("center_data must be TRUE or FALSE", call. = FALSE)

  if (!is.boolean(scale_data))
    stop("scale_data must be TRUE or FALSE", call. = FALSE)

  if (!is.boolean(pm_loading))
    stop("pm_loading must be TRUE or FALSE", call. = FALSE)

  if (!is.numeric(epspm_loading) || length(epspm_loading) != 1 || is.na(epspm_loading) || (epspm_loading <= 0))
    stop("epspm_loading must be a positive scalar", call. = FALSE)

  if (!is.numeric(maxiterpm_loading) || length(maxiterpm_loading) != 1 || is.na(maxiterpm_loading) || (maxiterpm_loading < 1))
    stop("maxiterpm_loading must be a positive scalar", call. = FALSE)

  if (!is.boolean(pm_varsel))
    stop("pm_varsel must be TRUE or FALSE", call. = FALSE)

  if (!is.numeric(epspm_varsel) || length(epspm_varsel) != 1 || is.na(epspm_varsel) || (epspm_varsel <= 0))
    stop("epspm_varsel must be a positive scalar", call. = FALSE)

  if (!is.numeric(maxiterpm_varsel) || length(maxiterpm_varsel) != 1 || is.na(maxiterpm_varsel) || (maxiterpm_varsel < 1))
    stop("maxiterpm_varsel must be a positive scalar", call. = FALSE)

  invisible(TRUE)
}


#' Computes LS SPCA components
#'
#' Computes sparse principal components from either a data matrix or a
#' covariance/correlation matrix. The wrapper routes the computation to the thin or the fat-matrix C++ engine.
#'
#' Character inputs are parsed transparently by their first letter inside the
#' main function. Fixed indices are passed through \code{fixed_index_list}, one
#' list element per component. Different variable selection approaches can be
#' selected, as explained in the `Deatails` section.
#'
#' @param M Real matrix. The \eqn{n \times p} data matrix or the
#'   \eqn{p \times p} covariance/correlation matrix.
#' @param alpha Real in (0,1]. Percentage of variance of the PCs explained by
#'   the sparse component.
#' @param ncomps Number of components to compute (default 0, automatic).
#'   Has priority over `ncomp_by_cvexp`.
#' @param ncomp_by_cvexp Real in (0,1]. Stops computating sPCs when the cumulative
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
#' @param fixed_index_list List of predetermined indices for components.
#'   Each list element is a vector of 1-based indices. Use \code{NULL} or
#'   \code{integer(0)} for a component with no fixed indices. If the list is
#'   shorter than the number of components to be computed, the remaining
#'   components are treated as unconstrained.
#' @param center_data Logical. Center the columns of \code{M} to zero mean?
#'   Used only when a data matrix is passed.
#' @param scale_data Logical. Scale the columns of \code{M}? Used only when a
#'   data matrix is passed.
#' @param pm_loading Logical. Use the power method for PCs and sparse-component
#'   eigenvectors.
#' @param epspm_loading Numeric. Tolerance for the pm_loading group.
#' @param maxiterpm_loading Integer. Maximum iterations for the pm_loading group.
#' @param pm_varsel Logical. Use the power method in variable selection.
#' @param epspm_varsel Numeric. Tolerance for the pm_varsel group.
#' @param maxiterpm_varsel Integer. Maximum iterations for the pm_varsel group.
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
                  ncomp_by_cvexp = 0.99,
                  method = "c",
                  var_selection = c("fwd", "bkw", "step"),
                  stop_criterion = c("cvexp", "r2"),
                  intensive = FALSE,
                  fat_matrix = NULL,
                  fixed_index_list = list(),
                  center_data = FALSE,
                  scale_data = FALSE,
                  pm_loading = FALSE,
                  epspm_loading = 1e-5,
                  maxiterpm_loading = 100,
                  pm_varsel = FALSE,
                  epspm_varsel = 1e-5,
                  maxiterpm_varsel = 200) {

  validate_spca_inputs(M = M,
                         alpha = alpha,
                         ncomps = ncomps,
                         ncomp_by_cvexp = ncomp_by_cvexp,
                         method = method,
                         var_selection = var_selection,
                         stop_criterion = stop_criterion,
                         intensive = intensive,
                         fat_matrix = fat_matrix,
                         fixed_index_list = fixed_index_list,
                         center_data = center_data,
                         scale_data = scale_data,
                         pm_loading = pm_loading,
                         epspm_loading = epspm_loading,
                         maxiterpm_loading = maxiterpm_loading,
                         pm_varsel = pm_varsel,
                         epspm_varsel = epspm_varsel,
                         maxiterpm_varsel = maxiterpm_varsel)

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
  } else if (!identical(ncomp_by_cvexp, FALSE) && (ncomp_by_cvexp > 0) && (ncomp_by_cvexp < 1)) {
    ncomps_cpp = if (use_fat_backend) n else p
  } else {
    ncomps_cpp = 0L
  }

  if ((ncomps_cpp > 0) && (length(method_cpp) < ncomps_cpp) && (length(method_cpp) > 0))
    method_cpp = c(method_cpp, rep(method_cpp[length(method_cpp)], ncomps_cpp - length(method_cpp)))

  if ((ncomps_cpp > 0) && (length(method_cpp) > ncomps_cpp))
    method_cpp = method_cpp[seq_len(ncomps_cpp)]

  if (length(fixed_index_list) > 0) {
    if (ncomps_cpp == 0)
      stop("when fixed_index_list is used, ncomps must be positive or ncomp_by_cvexp must be in (0, 1)", call. = FALSE)

    if (length(fixed_index_list) > ncomps_cpp)
      stop("fixed_index_list cannot be longer than the number of components to be computed", call. = FALSE)

    if (length(fixed_index_list) < ncomps_cpp)
      fixed_index_list = c(fixed_index_list, rep(list(NULL), ncomps_cpp - length(fixed_index_list)))

    index_lengths = vapply(fixed_index_list,
                           function(x) if (is.null(x)) 0L else length(x),
                           integer(1))
    
    fixed_index_list_num = lapply(fixed_index_list,
                                 function(x) if (is.null(x)) integer(0) 
                                 else as.integer(x))
    
    
    indvec_in = as.integer(unlist(fixed_index_list_num)) - 1L
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
      center_data = TRUE
    }
    if (center_data || scale_data) {
        M = scaleR(M, center_data, scale_data)

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
                     ncompbycvexp = ncomp_by_cvexp,
                     method = method_cpp,
                     indvec_in = indvec_in,
                     cardvec_in = cardvec_in,
                     PMPC = pm_loading,
                     PMS = pm_varsel,
                     epsPMPC = epspm_loading,
                     epsPMS = epspm_varsel,
                     maxiterPMPC = maxiterpm_loading,
                     maxiterPMS = maxiterpm_varsel,
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
                      ncompbycvexp = ncomp_by_cvexp,
                      method = method_cpp,
                      indvec_in = indvec_in,
                      cardvec_in = cardvec_in,
                      PMPC = pm_loading,
                      PMS = pm_varsel,
                      epsPMPC = epspm_loading,
                      epsPMS = epspm_varsel,
                      maxiterPMPC = maxiterpm_loading,
                      maxiterPMS = maxiterpm_varsel,
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

  for(i in seq_along(spout$loadlist)){
    names(spout$loadlist[[i]]) = var_names[spout$ind[[i]]]
  }
 
  
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
             sq_cor_with_PC = spout$r2,
             tot_var = spout$totvar,
             loadings_list = spout$loadlist,
             indices = spout$ind
             )
  ## FIX TIME NAMES
  if (use_fat_backend) {
    out$scores = spout$scores
    if (ncomps > 1){
      out$spc_cor = cor(spout$scores)
      colnames(out$spc_cor) = paste0("sPC", seq_len(spout$ncomps))
      rownames(out$spc_cor) = paste0("sPC", seq_len(spout$ncomps))
    }
    out$parameters = parameters
    out$time_wall = spout$Time_wall
    out$time_colnames = spout$Time_colnames
    out$setup_wall = spout$setup_wall
    out$setup_cpu = spout$setup_cpu
    out$time_unit_raw = spout$time_unit_raw
  }
  else {
# check if exist both scores and spc_cor add to cpp (done in thin but not checked) add to docs

    if ((is_datamatrix_M)) {
      out$scores = abC(M, spout$loadings[, seq_len(spout$ncomps), 
                                          drop = FALSE])
      
      colnames(out$scores) = paste0("sPC", seq_len(spout$ncomps))
      if (ncomps > 1){
      out$spc_cor = cor(out$scores)
      colnames(out$spc_cor) = paste0("sPC", seq_len(spout$ncomps))
      rownames(out$spc_cor) = paste0("sPC", seq_len(spout$ncomps))
      }
    }
    else{
      out$scores = NULL
      if (ncomps > 1){
        out$spc_cor = make_spc_cor_S(spout$loadings, S)
      }
      else 
        out$spc_cor = NULL
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
