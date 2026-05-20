
#' Validate inputs for spca
#'
#' Checks user inputs for \code{spca}. This helper only verifies that supplied
#' values have admissible types, lengths, and ranges. It does not assign
#' defaults, parse character options for C++, choose the backend, change index
#' bases, or modify the data matrix. All input normalization is done explicitly
#' in \code{spca()}.
#'
#' @param M Numeric matrix or data.frame. Data matrix or covariance/correlation
#'   matrix. Cannot contain missing values.
#' @param alpha \code{NULL} or numeric scalar in \eqn{(0, 1]}. Target retained
#'   proportion.
#' @param n_comps \\code{NULL} or nonnegative integer scalar. Number of
#'   components to compute. If \\code{NULL}, components are computed until
#'   \\code{ncomp_by_cvexp} is reached.
#' @param ncomp_by_cvexp \\code{NULL} or numeric scalar in \\eqn{(0, 1]}. Target
#'   cumulative variance explained used to determine the number of components.
#'   At least one of \\code{n_comps} and \\code{ncomp_by_cvexp} must be supplied.
#' @param method \code{NULL} or character vector. Each entry must start with
#'   \code{"u"}, \code{"c"}, or \code{"p"}.
#' @param var_selection \code{NULL} or character vector. The first entry must
#'   start with \code{"f"}, \code{"b"}, or \code{"s"}.
#' @param stop_criterion \code{NULL} or character vector. The first entry must
#'   start with \code{"r"} or \code{"c"}.
#' @param intensive \code{NULL} or logical scalar.
#' @param fat_matrix \code{NULL} or logical scalar.
#' @param fixed_index_list \code{NULL} or list of numeric/integer vectors or
#'  a factor.
#'  \code{NULL} implies no grouping. 
#' @param center_data \code{NULL} or logical scalar.
#' @param scale_data \code{NULL} or logical scalar.
#' @param pm_loading \code{NULL} or logical scalar.
#' @param eps_pm_loading \code{NULL} or positive numeric scalar.
#' @param maxiter_pm_loading \code{NULL} or positive integer scalar.
#' @param pm_varsel \code{NULL} or logical scalar.
#' @param eps_pm_varsel \code{NULL} or positive numeric scalar.
#' @param maxiter_pm_varsel \code{NULL} or positive integer scalar.
#'
#' @return Invisibly returns \code{TRUE} if all inputs are valid.
#' @noRd

validate_spca_inputs = function(M,
                                alpha,
                                n_comps,
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
    stop_criterion = stop_criterion,
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
  
  if (!is.null(alpha)) {
    if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) ||
        (alpha <= 0) || (alpha > 1))
      stop("alpha must be NULL or a numeric scalar in (0, 1]", call. = FALSE)
  }
  
  if (!is.null(n_comps)) {
    if (!is_int(n_comps) || length(n_comps) != 1 || (n_comps < 0))
      stop("n_comps must be NULL or a nonnegative integer scalar", call. = FALSE)
  }
  
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
  
  
  if (!is.null(var_selection)) {
    if (!is.character(var_selection) || (length(var_selection) < 1) ||
        anyNA(var_selection))
      stop("var_selection must be NULL or a character vector", call. = FALSE)
    
    var_selection_check = tolower(substr(trimws(var_selection[1]), 1, 1))
    if (!(var_selection_check %in% c("f", "b", "s")))
      stop("var_selection must start with one of f, b, or s", call. = FALSE)
  }
  
  if (!is.null(stop_criterion)) {
    if (!is.character(stop_criterion) || (length(stop_criterion) < 1) ||
        anyNA(stop_criterion))
      stop("stop_criterion must be NULL or a character vector", call. = FALSE)
    
    stop_check = tolower(substr(trimws(stop_criterion[1]), 1, 1))
    if (!(stop_check %in% c("r", "c")))
      stop("stop_criterion must start with one of r or c", call. = FALSE)
  }
  
  if (!is.null(intensive) && !is_boolean(intensive))
    stop("intensive must be NULL, TRUE, or FALSE", call. = FALSE)
  
  if (!is.null(fixed_index_list) && (length(fixed_index_list) < 2))
    stop("fixed_index_list can be either NULL or contain at least 2 elements")
  
  if (!is.null(fixed_index_list)) {
      
      if (!(is.list(fixed_index_list) || is.factor(fixed_index_list))) {
        stop("fixed_index_list must be NULL, a list indexing the variables 
        in each group or a factor",
             call. = FALSE)
      }
    
      if (is.list(fixed_index_list)) {
        noexh = (length(unlist(fixed_index_list)) != ncol(M))
        if(!noexh)
          noexh = !all.equal(sort(unlist(fixed_index_list)), 
                                      seq_len(ncol(M)))
      } 
      else {
        noexh = (!noexh) && ((length(fixed_index_list) !=  ncol(M)) || 
          (nlevels(fixed_index_list) < 2))
      }
      if (noexh) {
        stop("fixed_index_list must be a mutually exclusive and exhaustive
             partition of the variables of length > 1",
             call. = FALSE)
      }
    }
  
  if (!is.null(center_data) && !is_boolean(center_data))
    stop("center_data must be NULL, TRUE, or FALSE", call. = FALSE)
  
  if (!is.null(scale_data) && !is_boolean(scale_data))
    stop("scale_data must be NULL, TRUE, or FALSE", call. = FALSE)
  
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

#spca doc=========================
#' Compute LS-SPCA components
#'
#' `spca()` computes sparse principal components from a data matrix or from a
#' covariance/correlation matrix. Data matrices are routed to the tall or fat
#' C++ backend; covariance/correlation matrices always use the tall backend.
#'
#' @param M A numeric matrix or data frame. If `M` is square and symmetric, it
#'   is treated as a covariance/correlation matrix (only for the tall backend).
#'    Otherwise it is treated as an \eqn{n \times p} data matrix.
#' @param alpha A number in \eqn{(0, 1]}. Target proportion used by variable
#'   selection.
#' @param n_comps `NULL` or a nonnegative integer scalar. Number of components
#'   to compute. If `NULL`, `ncomp_by_cvexp` is used to determine the number of
#'   components.
#' @param ncomp_by_cvexp `NULL` or a number in \eqn{(0, 1]}. If `n_comps` is
#'   `NULL`, components are computed until cumulative variance explained reaches
#'   this value. At least one of `n_comps` and `ncomp_by_cvexp` must be 
#'   supplied.
#' @param method A character selecting the LS-SPCA variant. Values may
#'   be `"cspca"`, `"uspca"`, or `"pspca"`; only the first letter is used. 
#' @param var_selection A character vector selecting the variable-selection
#'   algorithm. Values starting with `"f"` use forward selection, values
#'   starting with `"b"` use backward elimination, and values starting with
#'   `"s"` use forward-stepwise selection.
#' @param stop_criterion A character vector selecting the stopping criterion
#'   for variable selection. Values starting with `"r"` use the squared
#'   correlation criterion; values starting with `"c"` use cumulative variance
#'   explained.
#' @param intensive A logical scalar. If `TRUE`, the tall backend uses intensive
#'   forward CVEXP selection. This option is not available for fat matrices.
#' @param fat_matrix `NULL` or a logical scalar. If `NULL`, data matrices with
#'   more columns than rows use the fat backend and all other inputs use the
#'   tall backend. If `TRUE`, the fat backend is requested. If `FALSE`, the tall
#'   backend is used.
#' @param fixed_index_list [`NULL`] A list of integer-valued vectors giving
#'   fixed 1-based variable indices for selected components or a factor. 
#'   Use `NULL` for components without fixed indices.
#' @param center_data A logical scalar. If `TRUE`, center data-matrix columns
#'   before fitting. Ignored when `M` is a covariance/correlation matrix.
#' @param scale_data A logical scalar. If `TRUE`, scale data-matrix columns
#'   before fitting. Ignored when `M` is a covariance/correlation matrix.
#' @param pm_loading A logical scalar. If `TRUE`, use the power method for PC
#'   and sparse-loading eigenvectors.
#' @param eps_pm_loading A positive number. Convergence tolerance for
#'   `pm_loading`.
#' @param maxiter_pm_loading A positive integer. Maximum number of iterations
#'   for `pm_loading`.
#' @param pm_varsel A logical scalar. If `TRUE`, use the power method inside
#'   variable selection.
#' @param eps_pm_varsel A positive number. Convergence tolerance for
#'   `pm_varsel`.
#' @param maxiter_pm_varsel A positive integer. Maximum number of iterations
#'   for `pm_varsel`.
#'
#' @details
#' A covariance matrix as input to `M` is accepted only for 
#'   tall matrices. 

#' Variable selection is controlled by `var_selection`, `stop_criterion`, and
#' `intensive`.
#'
#' \tabular{lll}{
#'   `var_selection` \tab `stop_criterion` \tab Algorithm \cr
#'   `"fwd"` \tab `"r2"` \tab Forward selection with squared-correlation stopping \cr
#'   `"bkw"` \tab `"r2"` \tab Backward elimination with squared-correlation stopping \cr
#'   `"step"` \tab `"r2"` \tab Forward-stepwise selection with squared-correlation stopping \cr
#'   `"fwd"` \tab `"cvexp"` \tab Forward selection with CVEXP stopping \cr
#'   `"bkw"` \tab `"cvexp"` \tab Backward elimination with CVEXP stopping \cr
#'   `"step"` \tab `"cvexp"` \tab Forward-stepwise selection with CVEXP stopping
#'    .\cr  `intensive = TRUE` requires "fwd" \tab `"cvexp"` \tab Intensive
#'    forward CVEXP selection. \cr
#' }
#'
#' The fat backend currently uses regression-based forward variable selection only: `var_selection = "f"`, `intensive = FALSE`. 
#' Any another combination supplied will generate an error.  
#'
#' The returned object is documented in [spca_object].
#'
#' @return An object of class `spca`.
#' @family spca
#' @export
spca = function(M,
                alpha = 0.95,
                n_comps = NULL,
                ncomp_by_cvexp = NULL,
                method =  c("cspca", "uspca", "pspca"),
                var_selection = c("fwd", "bkw", "step"),
                stop_criterion = c("cvexp", "r2"),
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
                       stop_criterion = stop_criterion,
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
  if(is.null(var_names)) var_names = paste0("V", 1:ncol(M))
  
  
  method_cpp = tolower(substr(trimws(method[1]), 1, 1))
  
  var_selection = switch(tolower(substr(trimws(var_selection[1]), 1, 1)),
                             "f" = "fwd",
                             "b" = "bkw",
                             "s" = "step")
  stop_criterion = switch(tolower(substr(trimws(stop_criterion[1]), 1, 1)),
         "r" = "r2",
         "c" = "cvexp")
  if (intensive) {
    if (var_selection != "fwd") {
      stop("Intensive variable selection is only implemented for forward
           selection", call. = FALSE)
    }
    if (stop_criterion == "r2") {
      stop("Intensive variable selection is not implemented for 
           stop_criterion = 'r2'", call. = FALSE)
    }
  }

  var_selection_cpp = switch(var_selection, 
                             "fwd" = 0,
                             "bkw" = 1,
                             "step" = 2)
  
  stop_criterion_cpp = switch(stop_criterion,
                              "r2" = 0,
                              "cvexp" = 1)
  
  is_datamatrix_M = TRUE
  if ((n == p) && isSymmetric(M))
    is_datamatrix_M = FALSE
  
  if (is.null(fat_matrix)) {
    fat_matrix = (is_datamatrix_M && (n < p))
    if (fat_matrix)
      warning("fat_matrix backend selected because n < p")
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
  
#    browser()
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
      M = standardize_data(M, center_data, scale_data)
      
    }
    S = ata(M)
  } else 
    if(!use_fat_backend){
      S = M
    }
# browser()
  if (use_fat_backend) {
    spout = lsspcaTC(X = M,
                     ncomps = n_comps,
                     stop_criterion = stop_criterion_cpp,
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
    ## THIN_MATRIX BACKEND    
    selection_method_cpp = var_selection_cpp
    if (intensive)
      selection_method_cpp = 3
    
    spout = lsspcaC(S = S,
                    ncomps = n_comps,
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

  parameters = list(
    method = ifelse(method_cpp == "c", "cspca", 
                    ifelse(method_cpp == "u", "uspca", "pspca")),
    var_selection = var_selection,
    stop_criterion = stop_criterion,
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
             tot_var = spout$totvar,
             loadings_list = spout$loadlist,
             indices = spout$ind
  )
  ## FIX TIME NAMES
 # browser()
  if (use_fat_backend) {
    out$scores = spout$scores
    if (n_comps > 1){
      out$spc_cor = cor(spout$scores)
      colnames(out$spc_cor) = paste0("sPC", seq_len(spout$ncomps))
      rownames(out$spc_cor) = paste0("sPC", seq_len(spout$ncomps))
    }
    else out$spc_cor = list(NULL)
    out$parameters = parameters
#    out$cpp_time_wall = spout$Time_wall
     out$time_wall = sum(spout$Time_wall)
    out$time_colnames = spout$Time_colnames
    out$setup_wall = spout$setup_wall
    out$setup_cpu = spout$setup_cpu
    out$time_unit_raw = spout$time_unit_raw
  }
  else {
    # check if exist both scores and spc_cor add to cpp (done in thin but not checked) add to docs
    if ((is_datamatrix_M)) {
      out$scores = ab(M, spout$loadings[, seq_len(spout$ncomps), 
                                         drop = FALSE])
      
      colnames(out$scores) = paste0("sPC", seq_len(spout$ncomps))
#      browser()
      if (n_comps > 1){
        out$spc_cor = cor(out$scores)
        colnames(out$spc_cor) = paste0("sPC", seq_len(spout$ncomps))
        rownames(out$spc_cor) = paste0("sPC", seq_len(spout$ncomps))
      }
      else out$spc_cor = list(NULL)
    }
    else{
      out$scores = NULL
      if (n_comps > 1){
        out$spc_cor = make_spc_cor_S(spout$loadings, S)
      }
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
