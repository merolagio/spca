#' Validate inputs for spca
#'
#' Check user inputs for \code{spca()}. This helper verifies that supplied
#' values have admissible types, lengths, and ranges. It does not assign
#' defaults, parse character options for C++, choose the backend, change index
#' bases, or modify the data matrix; those steps are handled in \code{spca()}.
#'
#' @param M A numeric matrix or data frame. This can be a data matrix or a
#'   covariance/correlation matrix and must not contain missing values.
#' @param alpha A numeric scalar in \eqn{(0, 1]} or \code{NULL}. Target retained
#'   proportion used by variable selection.
#' @param n_comps A nonnegative integer scalar or \code{NULL}. Number of
#'   components to compute.
#' @param ncomp_by_cvexp A numeric scalar in \eqn{(0, 1]} or \code{NULL}.
#'   Target cumulative variance explained used to determine the number of
#'   components. At least one of \code{n_comps} and \code{ncomp_by_cvexp} must
#'   be supplied.
#' @param method A character vector or \code{NULL}. The first element must start
#'   with \code{"u"}, \code{"c"}, or \code{"p"}.
#' @param var_selection A character vector or \code{NULL}. The first element
#'   must start with \code{"f"}, \code{"b"}, or \code{"s"}.
#' @param objective A character vector or \code{NULL}. The first element must
#'   start with \code{"r"} or \code{"c"}.
#' @param intensive A logical scalar or \code{NULL}.
#' @param fat_matrix A logical scalar or \code{NULL}.
#' @param fixed_index_list A list of numeric or integer vectors, a factor, or
#'   \code{NULL}. If supplied, it must define a mutually exclusive and
#'   exhaustive partition of the variables with at least two groups.
#' @param center_data A logical scalar or \code{NULL}.
#' @param scale_data A logical scalar or \code{NULL}.
#' @param pm_loading A logical scalar or \code{NULL}.
#' @param eps_pm_loading A positive numeric scalar or \code{NULL}.
#' @param maxiter_pm_loading A positive integer scalar or \code{NULL}.
#' @param pm_varsel A logical scalar or \code{NULL}.
#' @param eps_pm_varsel A positive numeric scalar or \code{NULL}.
#' @param maxiter_pm_varsel A positive integer scalar or \code{NULL}.
#'
#' @return Invisibly returns \code{TRUE} if all inputs are valid; otherwise,
#'   throws an error.
#' @noRd

validate_spca_inputs = 
  function(M,
           alpha,
           n_comps,
           ncomp_by_cvexp,
           method,
           var_selection,
           objective,
           intensive,
           fat_matrix,
           fixed_index_list,
           center_data,
           scale_data,
           pm_loading,
           eps_pm_loading,
           maxiter_pm_loading,
           pm_varsel,
           eps_pm_varsel,
           maxiter_pm_varsel) {
    
    validate_no_na(
    alpha = alpha,
    n_comps = n_comps,
    ncomp_by_cvexp = ncomp_by_cvexp,
    method = method,
    var_selection = var_selection,
    objective = objective,
    intensive = intensive,
    fat_matrix = fat_matrix,
    fixed_index_list = fixed_index_list,
    center_data = center_data,
    scale_data = scale_data,
    pm_loading = pm_loading,
    eps_pm_loading = eps_pm_loading,
    maxiter_pm_loading = maxiter_pm_loading,
    pm_varsel = pm_varsel,
    eps_pm_varsel = eps_pm_varsel,
    maxiter_pm_varsel = maxiter_pm_varsel
  )
  
  validate_booleans_or_null(
    intensive = intensive,
    fat_matrix = fat_matrix,
    center_data = center_data,
    scale_data = scale_data,
    pm_loading = pm_loading,
    pm_varsel = pm_varsel
  )
  #data or covariance matrix
  if (is.null(M))
    stop("M cannot be NULL", call. = FALSE)
  
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
  
  p = ncol(M)
  
  #alpha    
  if (!is.null(alpha)) {
    if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) ||
        (alpha <= 0) || (alpha > 1))
      stop("alpha must be NULL or a numeric scalar in (0, 1]", call. = FALSE)
  }
  
  #n_comps    
  if (!is.null(n_comps)) {
    if (!is_int(n_comps) || length(n_comps) != 1 || (n_comps < 0))
      stop("n_comps must be NULL or a nonnegative integer scalar", call. = FALSE)
  }
  #n_comp_by_cvexp   
  if (!is.null(ncomp_by_cvexp)) {
    if (!is.numeric(ncomp_by_cvexp) || length(ncomp_by_cvexp) != 1 ||
        is.na(ncomp_by_cvexp) || (ncomp_by_cvexp <= 0) ||
        (ncomp_by_cvexp > 1))
      stop("n_comp_by_cvexp must be NULL or a numeric scalar in (0, 1]", call. = FALSE)
  }
  
  
  if (is.null(n_comps) && is.null(ncomp_by_cvexp))
    stop("specify either n_comps or ncomp_by_cvexp", call. = FALSE)
  if (!is.null(method)) {
    if (!is.character(method) || length(method) < 1 || anyNA(method))
      stop("method must be NULL or a character vector without missing values",
           call. = FALSE)
    
    if (!(tolower(substr(trimws(method[1]), 1, 1)) %in% c("c", "p", "u")))
      stop("method must start with one of c, p or u", call. = FALSE)
  }
  
  
  #variable selection   
  if (!is.null(var_selection)) {
    if (!is.character(var_selection) || (length(var_selection) < 1) ||
        anyNA(var_selection))
      stop("var_selection must be NULL or a character vector", call. = FALSE)
    
    var_selection_check = tolower(substr(trimws(var_selection[1]), 1, 1))
    if (!(var_selection_check %in% c("f", "b", "s")))
      stop("var_selection must start with one of f, b, or s", call. = FALSE)
  }
  # objective  
  if (!is.null(objective)) {
    if (!is.character(objective) || (length(objective) < 1) ||
        anyNA(objective))
      stop("objective must be NULL or a character vector", call. = FALSE)
    
    stop_check = tolower(substr(trimws(objective[1]), 1, 1))
    if (!(stop_check %in% c("r", "c")))
      stop("objective must start with one of r or c", call. = FALSE)
  }
  #intensive  
  if (!is.null(intensive) && !is_boolean(intensive))
    stop("intensive must be NULL, TRUE, or FALSE", call. = FALSE)
  
  #fixed indices  
  if (!is.null(fixed_index_list) && (length(fixed_index_list) < 2))
    stop("fixed_index_list can be either NULL or contain at least 2 elements")
  
  if (!is.null(fixed_index_list)) {
    
    if (!(is.list(fixed_index_list) || is.factor(fixed_index_list))) {
      stop("fixed_index_list must be NULL, a list indexing the variables 
        in each group or a factor",
           call. = FALSE)
    }
    
    noexh = FALSE
    
    if (is.list(fixed_index_list)) {
      noexh = (length(unlist(fixed_index_list)) != ncol(M))
      
      if (!noexh)
        noexh = !isTRUE(all.equal(sort(unlist(fixed_index_list)),
                                  seq_len(ncol(M))))
    } else {
      if ((length(fixed_index_list) != ncol(M)) ||
          (nlevels(fixed_index_list) < 2))
        noexh = TRUE
    }
    
    if (noexh) {
      stop("fixed_index_list must be a mutually exclusive and exhaustive
       partition of the variables of length > 1",
           call. = FALSE)
    }
  }
  
  #center and scale data  
  if (!is.null(center_data) && !is_boolean(center_data))
    stop("center_data must be NULL, TRUE, or FALSE", call. = FALSE)
  
  if (!is.null(scale_data) && !is_boolean(scale_data))
    stop("scale_data must be NULL, TRUE, or FALSE", call. = FALSE)
  
  #power method  
  if (!is.null(pm_loading) && !is_boolean(pm_loading))
    stop("pm_loading must be NULL, TRUE, or FALSE", call. = FALSE)
  
  if (!is.null(eps_pm_loading)) {
    if (!is.numeric(eps_pm_loading) || length(eps_pm_loading) != 1 ||
        is.na(eps_pm_loading) || (eps_pm_loading <= 0))
      stop("eps_pm_loading must be NULL or a positive numeric scalar", call. = FALSE)
  }
  
  if (!is.null(maxiter_pm_loading)) {
    if (!is_int(maxiter_pm_loading) || length(maxiter_pm_loading) != 1 || (maxiter_pm_loading < 1))
      stop("maxiter_pm_loading must be NULL or a positive integer", call. = FALSE)
  }
  
  if (!is.null(pm_varsel) && !is_boolean(pm_varsel))
    stop("pm_varsel must be NULL, TRUE, or FALSE", call. = FALSE)
  
  if (!is.null(eps_pm_varsel)) {
    if (!is.numeric(eps_pm_varsel) || length(eps_pm_varsel) != 1 ||
        is.na(eps_pm_varsel) || (eps_pm_varsel <= 0))
      stop("eps_pm_varsel must be NULL or a positive numeric scalar", call. = FALSE)
  }
  
  if (!is.null(maxiter_pm_varsel)) {
    if (!is_int(maxiter_pm_varsel) || length(maxiter_pm_varsel) != 1 || (maxiter_pm_varsel < 1))
      stop("maxiter_pm_varsel must be NULL or a positive integer scalar", call. = FALSE)
  }
  
  invisible(TRUE)
}

#spca=========================
#' Compute LS-SPCA components
#'
#' Compute least squares sparse principal components (LS-SPCA) from a data
#' matrix or from a covariance/correlation matrix.
#'
#' @param M A numeric matrix or data frame. If \code{M} is square, it is treated
#'   as a covariance/correlation matrix and the tall backend is used. Otherwise,
#'   \code{M} is treated as an \eqn{n \times p} data matrix.
#' @param n_comps A nonnegative integer scalar or \code{NULL} (default
#'   \code{NULL}). Number of components to compute. If \code{NULL},
#'   \code{ncomp_by_cvexp} is used to determine the number of components. At
#'   least one of \code{n_comps} and \code{ncomp_by_cvexp} must be supplied.
#' @param alpha A numeric scalar in \eqn{(0, 1]} (default \code{0.95}). Target
#'   retained proportion used by variable selection.
#' @param ncomp_by_cvexp A numeric scalar in \eqn{(0, 1]} or \code{NULL}
#'   (default \code{NULL}). If \code{n_comps = NULL}, components are computed
#'   until cumulative variance explained reaches this value.
#' @param method A character vector (default first element \code{"cspca"}).
#'   LS-SPCA variant. Accepted values are \code{"cspca"}, \code{"uspca"}, and
#'   \code{"pspca"}; only the first letter is used.
#' @param var_selection A character vector (default first element
#'   \code{"fwd"}). Variable-selection algorithm. Values starting with
#'   \code{"f"} use forward selection, values starting with \code{"b"} use
#'   backward elimination, and values starting with \code{"s"} use
#'   forward-stepwise selection.
#' @param objective A character vector (default first element \code{"r2"}).
#'   Stopping criterion for variable selection. Values starting with \code{"r"}
#'   use the squared-correlation criterion; values starting with \code{"c"} use
#'   cumulative variance explained.
#' @param intensive A logical value (default \code{FALSE}). If \code{TRUE}, the
#'   tall backend uses intensive forward CVEXP selection. This option is not
#'   available for fat matrices.
#' @param fat_matrix A logical value or \code{NULL} (default \code{NULL}). If
#'   \code{NULL}, data matrices with more columns than rows use the fat backend,
#'   and all other inputs use the tall backend. If \code{TRUE}, the fat backend
#'   is requested. If \code{FALSE}, the tall backend is used.
#' @param fixed_index_list A list of integer-valued vectors, a factor, or
#'   \code{NULL} (default \code{NULL}). If supplied, it must define a mutually
#'   exclusive and exhaustive partition of the variables with at least two
#'   groups. List indices are 1-based.
#' @param center_data A logical value (default \code{FALSE}). If \code{TRUE},
#'   center data-matrix columns before fitting. Ignored when \code{M} is treated
#'   as a covariance/correlation matrix.
#' @param scale_data A logical value (default \code{FALSE}). If \code{TRUE},
#'   scale data-matrix columns before fitting. Ignored when \code{M} is treated
#'   as a covariance/correlation matrix.
#' @param pm_loading A logical value (default \code{FALSE}). If \code{TRUE}, use
#'   the power method for PC and sparse-loading eigenvectors.
#' @param eps_pm_loading A positive numeric scalar (default \code{1e-4}).
#'   Convergence tolerance for \code{pm_loading}.
#' @param maxiter_pm_loading A positive integer scalar (default \code{1000}).
#'   Maximum number of iterations for \code{pm_loading}.
#' @param pm_varsel A logical value (default \code{FALSE}). If \code{TRUE}, use
#'   the power method inside variable selection.
#' @param eps_pm_varsel A positive numeric scalar (default \code{1e-4}).
#'   Convergence tolerance for \code{pm_varsel}.
#' @param maxiter_pm_varsel A positive integer scalar (default \code{500}).
#'   Maximum number of iterations for \code{pm_varsel}.
#'
#' @details Data matrices are routed to the tall or fat C++ backend. Square
#' matrices are treated as covariance/correlation matrices and use the tall
#' backend.
#'
#' Variable selection is controlled by \code{var_selection}, \code{objective},
#' and \code{intensive}.
#'
#' \tabular{lll}{
#' \code{var_selection} \tab \code{objective} \tab Algorithm \cr
#' \code{"fwd"} \tab \code{"r2"} \tab Forward selection with
#' squared-correlation stopping \cr
#' \code{"bkw"} \tab \code{"r2"} \tab Backward elimination with
#' squared-correlation stopping \cr
#' \code{"step"} \tab \code{"r2"} \tab Forward-stepwise selection with
#' squared-correlation stopping \cr
#' \code{"fwd"} \tab \code{"cvexp"} \tab Forward selection with CVEXP stopping
#' \cr
#' \code{"bkw"} \tab \code{"cvexp"} \tab Backward elimination with CVEXP
#' stopping \cr
#' \code{"step"} \tab \code{"cvexp"} \tab Forward-stepwise selection with
#' CVEXP stopping \cr
#' \code{intensive = TRUE} requires \code{"fwd"} \tab \code{"cvexp"} \tab
#' Intensive forward CVEXP selection \cr
#' }
#'
#' The fat backend currently supports regression-based forward variable
#' selection only: \code{var_selection = "f"} and \code{intensive = FALSE}.
#' Other combinations generate an error.
#'
#' The returned object is documented in [spca_object].
#' @return An object of class \code{spca}.
#' 
#' @examples
#' data(holzinger)
#' #default
#' ho_cspca = spca(holzinger, n_comps = 4)
#' #uncorrelated components and subsets determined using CVEXP as stopping rule
#' ho_uspca = spca(holzinger, n_comps = 4, method = "uspca", 
#'                 objective = "cvexp")
#' 
#' @family spca
#' @export
spca = function(M,
                n_comps = NULL,
                alpha = 0.95,
                ncomp_by_cvexp = NULL,
                method =  c("cspca", "uspca", "pspca"),
                var_selection = c("fwd", "bkw", "step"),
                objective = c("r2", "cvexp"),
                intensive = FALSE,
                fat_matrix = NULL,
                fixed_index_list = NULL,
                center_data = FALSE,
                scale_data = FALSE,
                pm_loading = FALSE,
                eps_pm_loading = 1e-4,
                maxiter_pm_loading = 1000,
                pm_varsel = FALSE,
                eps_pm_varsel = 1e-4,
                maxiter_pm_varsel = 500) {
  
  validate_spca_inputs(M = M,
                       alpha = alpha,
                       n_comps = n_comps,
                       ncomp_by_cvexp = ncomp_by_cvexp,
                       method = method,
                       var_selection = var_selection,
                       objective = objective,
                       intensive = intensive,
                       fat_matrix = fat_matrix,
                       fixed_index_list = fixed_index_list,
                       center_data = center_data,
                       scale_data = scale_data,
                       pm_loading = pm_loading,
                       eps_pm_loading = eps_pm_loading,
                       maxiter_pm_loading = maxiter_pm_loading,
                       pm_varsel = pm_varsel,
                       eps_pm_varsel = eps_pm_varsel,
                       maxiter_pm_varsel = maxiter_pm_varsel)
  
  if (is.data.frame(M))
    M = as.matrix(M)
  
  n = nrow(M)
  p = ncol(M)
  
  var_names = colnames(M)
  if(is.null(var_names)) var_names = paste0("V", seq_len(ncol(M)))
  
  
  method_cpp = tolower(substr(trimws(method[1]), 1, 1))
  
  var_selection = switch(tolower(substr(trimws(var_selection[1]), 1, 1)),
                         "f" = "fwd",
                         "b" = "bkw",
                         "s" = "step")
  objective = switch(tolower(substr(trimws(objective[1]), 1, 1)),
                     "r" = "r2",
                     "c" = "cvexp")
  if (intensive) {
    if (var_selection != "fwd") {
      stop("Intensive variable selection is only implemented for forward
           selection", call. = FALSE)
    }
    if (objective == "r2") {
      stop("Intensive variable selection is not implemented for 
           objective = 'r2'", call. = FALSE)
    }
  }
  
  var_selection_cpp = switch(var_selection, 
                             "fwd" = 0,
                             "bkw" = 1,
                             "step" = 2)
  
  objective_cpp = switch(objective,
                         "r2" = 0,
                         "cvexp" = 1)
  
  is_datamatrix_M = TRUE
  if ((n == p))
    is_datamatrix_M = FALSE
  
  if (is.null(fat_matrix)) {
    fat_matrix = (is_datamatrix_M && (n < p))
    if (fat_matrix)
      warning("fat_matrix backend selected because n < p")
  }
  
  if (isTRUE(fat_matrix)) {
    if (!is_datamatrix_M) {
      warning("fat_matrix = TRUE ignored because the input is a covariance/correlation matrix; using the tall backend")
      use_fat_backend = FALSE
    } else if (n < p) {
      use_fat_backend = TRUE
    } else {
      warning("fat_matrix = TRUE ignored because the data matrix is not fat; using the tall backend")
      use_fat_backend = FALSE
    }
  } else {
    use_fat_backend = FALSE
    if (is_datamatrix_M && (n < p))
      warning("fat_matrix = FALSE forces the tall backend on a fat data matrix; X'X may be singular")
  }
  
  if (use_fat_backend) {
    if(intensive){
      stop("Intensive variable selection not implemented for fat matrices.")
    }
    if (var_selection != "fwd") {
      stop(paste("var_selection", var_selection, "is not implemented for fat matrices"), 
           call. = FALSE)
    }
  }
  
  max_n_comps = if (use_fat_backend) n else p
  
  fixed_n_comps = !is.null(n_comps)
  
  if (is.null(n_comps)){
    if (is.null(ncomp_by_cvexp))
      stop("one of n_comps and ncomps_by_cvexp must have anumeric value")
    if (ncomp_by_cvexp < 1) {
      n_comps = 0L
      ncomps_cpp = max_n_comps
    }
    else {
      n_comps = max_n_comps
      ncomps_cpp = max_n_comps
    }
  } else {
    n_comps = as.integer(n_comps)
    ncomps_cpp = n_comps
    ncomp_by_cvexp = 1
  }
  
  indvec_in = NULL
  cardvec_in = NULL
  
  if (is.factor(fixed_index_list) && (nlevels(fixed_index_list) > 1)) {
    indvec_in = as.integer(fac2index(fixed_index_list)) - 1L #C++ takes 0 based indices
    cardvec_in = table(fixed_index_list)
  }
  else {
    if(length(fixed_index_list) > 1){
      indvec_in = as.integer(unlist(fixed_index_list)) - 1L #C++ takes 0 based indices
      cardvec_in = sapply(fixed_index_list, length)
    }  
  }
  if (!is.null(cardvec_in)) 
    if((method_cpp == "u") && any(cardvec_in < seq_len(p)) )
      stop("for uspca components need cardinality not less than component order")
  if (use_fat_backend && (var_selection_cpp != 0)){
    warning("Only forward variable selection for fat matrices is available. Switching to that ", call. = FALSE)
    var_selection_cpp = 0
  }
  if (use_fat_backend && intensive){
    warning("intensive is currently available only for tall matrices", 
            call. = FALSE)
    intensive = FALSE
  }
  if (intensive && (objective_cpp != 1)){
    warning("intensive search requires objective = cvexp. Switching to that.", call. = FALSE)
    objective_cpp = 1
  }
  ##  X = M
  if (is_datamatrix_M) {
    if (any(abs(colMeans(M)) > 1e-4)) {
      warning("Centering column means to zero")
      center_data = TRUE
    }
    if (center_data || scale_data) {
      M = standardize_data(M, center_data, scale_data)
      
    }
    if(!use_fat_backend)
      S = ata(M)
  } else 
    if(!use_fat_backend){
      S = M
    }
  if (use_fat_backend) {
    spout = lsspcaTC(X = M,
                     ncomps = n_comps,
                     stop_criterion = objective_cpp,
                     exact_cvexp = FALSE,
                     alpha = alpha,
                     ncompbycvexp = ncomp_by_cvexp,
                     method = method_cpp,
                     indvec_in = indvec_in,
                     cardvec_in = cardvec_in,
                     PMPC = pm_loading,
                     PMS = pm_varsel,
                     epsPMPC = eps_pm_loading,
                     epsPMS = eps_pm_varsel,
                     maxiterPMPC = maxiter_pm_loading,
                     maxiterPMS = maxiter_pm_varsel,
                     rank_tol = 0.0)
  } else {
    ## TALL_MATRIX BACKEND    
    selection_method_cpp = var_selection_cpp
    if (intensive)
      selection_method_cpp = 3
    
    spout = lsspcaC(S = S,
                    ncomps = n_comps,
                    selection_method = selection_method_cpp,
                    stop_criterion = objective_cpp,
                    exact_cvexp = FALSE,
                    alpha = alpha,
                    ncompbycvexp = ncomp_by_cvexp,
                    method = method_cpp,
                    indvec_in = indvec_in,
                    cardvec_in = cardvec_in,
                    PMPC = pm_loading,
                    PMS = pm_varsel,
                    epsPMPC = eps_pm_loading,
                    epsPMS = eps_pm_varsel,
                    maxiterPMPC = maxiter_pm_loading,
                    maxiterPMS = maxiter_pm_varsel,
                    rank_tol = 0.0)
  }
  
  if (fixed_n_comps && (spout$ncomps != n_comps))
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
  
  n_comps_input = n_comps
  n_comps = spout$ncomps
  
  ## modifying values from lsspcaC 
  if(is.null(spout$totvar))
    spout$totvar = sum(spout$vexpPC)
  spout$vexp = spout$vexp/spout$totvar
  spout$cvexp = spout$cvexp/spout$totvar
  spout$vexpPC = spout$vexpPC/spout$totvar
  
  vexpPC = spout$vexpPC[1:n_comps]
  rvexp =  spout$vexp/vexpPC[1:n_comps]
  rcvexp = spout$cvexp/cumsum(vexpPC[1:n_comps])
  spc_cor = spout$cor_comps
  dimnames(spc_cor) = list(paste0("sPC", seq_len(spout$ncomps)),
                           paste0("sPC", seq_len(spout$ncomps)))
  
  parameters = list(
    method = ifelse(method_cpp == "c", "cspca", 
                    ifelse(method_cpp == "u", "uspca", "pspca")),
    var_selection = var_selection,
    objective = objective,
    intensive = intensive,
    fat_matrix = use_fat_backend
  )
  
  # OUTPUT =================
  
  out = list(loadings = spout$loadings,
             contributions = contributions,
             n_comps = spout$ncomps,
             cardinality = spout$card,
             vexp = spout$vexp,
             vexp_pc = spout$vexpPC[1:n_comps],
             cvexp = spout$cvexp,
             rvexp = rvexp,
             rcvexp = rcvexp,
             cor_with_pc = spout$r,
             spc_cor = spc_cor,
             tot_var = spout$totvar,
             loadings_list = spout$loadlist,
             indices = spout$ind
  )
  if (use_fat_backend) {
    out$scores = spout$scores
    out$parameters = parameters
    #    out$cpp_time_wall = spout$Time_wall
    out$time_wall = sum(spout$Time_wall)
    out$time_colnames = spout$Time_colnames
    out$setup_wall = spout$setup_wall
    out$setup_cpu = spout$setup_cpu
    out$time_unit_raw = spout$time_unit_raw
  } 
  else {
    if ((is_datamatrix_M)) {
      out$scores = make_scores(M, spout$loadings[, seq_len(spout$ncomps), 
                                                 drop = FALSE])
      colnames(out$scores) = paste0("sPC", seq_len(spout$ncomps))
    }
    else{
      out$scores = NULL
    }
    #out$cpp_time_wall = sum(spout$Time)
    
    out$parameters = parameters
    colnames(spout$Time) = spout$Time_colnames
    out$time = spout$Time
    out$time_vec = spout$timevec
    out$time_comp = spout$timecomp
  }
  if (use_fat_backend) {
    out$cpp_time_wall = sum(spout$Time_wall)
  } else {
    out$cpp_time_wall = spout$Time
  }
  
  out$call = match.call()
  class(out) = c(class(out), "spca")
  return(out)
}
