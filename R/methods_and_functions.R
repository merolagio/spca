#FUNCTIONS----------------------------------
# is.spca==============
#' Test for SPCA Objects
#'
#' Check whether an object has class \code{spca} and contains the core elements
#' required by the package.
#'
#' @param x An object to test.
#'
#' @details The function checks for class \code{spca} and for the presence of
#' the core elements used by the package, including weights, contributions,
#' explained-variance summaries, component counts, cardinalities, weight lists,
#' and active indices. It performs a lightweight structural check; use
#' \code{validate_spca()} for a more detailed internal validation.
#'
#' @return A logical value. Returns \code{TRUE} if \code{x} has class
#' \code{spca} and contains the required core elements, and \code{FALSE}
#' otherwise.
#'
#' @examples
#' data(holzinger)
#' ho_cspca = spca(holzinger, n_comps = 2)
#' is.spca(ho_cspca)
#' 
#' @family spca
#' @export
is.spca = function(x) {
  inherits(x, "spca") && 
    (!is.null(x$weights) || !is.null(x$loadings)) &&
    !is.null(x$contributions) &&
    !is.null(x$vexp) &&
    !is.null(x$vexp_pc) &&
    !is.null(x$cvexp) &&
    !is.null(x$rvexp) &&
    !is.null(x$rcvexp) &&
    !is.null(x$n_comps) &&
    !is.null(x$cardinality) &&
    (!is.null(x$weights_list) || !is.null(x$loadings_list)) &&
    !is.null(x$indices) 
}

#new.spca==================
#' Construct an SPCA Object from a Set of Weights
#'
#' Build an object of class \code{spca} from a weights matrix and either a
#' covariance or correlation matrix, a data matrix, or both.
#'
#' @param A A numeric matrix of weights.
#' @param S A numeric covariance or correlation matrix (default \code{NULL}).
#'   If \code{NULL}, \code{X} is used to estimate the covariance matrix.
#' @param X A numeric data matrix or data frame (default \code{NULL}). Used to
#'   compute \code{S} when \code{S = NULL}, and to compute scores when supplied.
#'   At least one of \code{S} or \code{X} must be provided.
#' @param method_name A character scalar or \code{NULL} (default \code{NULL}).
#'   Name of the method used to compute the weights.
#'
#' @return An `spca` object.
#' @examples
#' set.seed(1)
#' A = round(matrix(runif(24, -1, 1), 12))
#' A[abs(A) < 0.4] = 0 #no need to scale to unit norm
#' data(holzinger)
#' spca_new = new_spca(A, X = holzinger)
#' is.spca(spca_new)
#' summary(spca_new)
#' 
#' @family spca
#' @export 
new_spca = function(A, S = NULL, X = NULL, method_name = NULL){
  
# validation   ==============P
  
  fun_inp = list(A = A, S = S, X = X, method_name = method_name)
  validate_no_na(arg_list = fun_inp)
  
  if(is.data.frame(A)) 
    A = as.matrix(A)
  if (!is.matrix(A)){
    stop("A must be a matrix of weights")
  }
  if (is.null(S)) {
    if (is.null(X))
      stop("S and X cannot be both NULL", call. = FALSE)
    
    if (is.data.frame(X))
      X = as.matrix(X)
    
    if (any(abs(colMeans(X)) > 1e-4))
      X = standardize_data(X, TRUE, FALSE)
    
    S = ata(X) / (nrow(X) - 1)
  }
  
  if (!is.null(X) && !is.null(S)) {
    if (is.data.frame(X))
      X = as.matrix(X)
    
    if (ncol(X) != ncol(S))
      stop("X and S have incompatible dimensions", call. = FALSE)
    
    # avoids multiplication that could be expensive
    x_var = colSums(X^2) / (nrow(X) - 1)
    
    if (!isTRUE(all.equal(x_var, diag(S), check.attributes = FALSE, 
                          tolerance = 1e-6)))
      stop("diag(S) is not compatible with X", call. = FALSE)
  }
  
  if(!is_symmetric_fast(S))
    stop("S must be a symmetric covariance or correlation matrix")
  
  if (any((colSums(A^2) - 1) > 1e-5)){
    message("Scaling weights to unit L2 norm")
    A = scale_columns(A, 2)
  }
  
  
  n_comps = ncol(A)
  
  # compute vexp and eigen(S)
  vexp = make_vexpSC(A, S)
  
  s_ee = eigen_sym(S)
  
  
  ind_list = apply(A, 2, function(x) which(x != 0))
  
  #cor with PCs
  cor_with_pc = numeric(n_comps)
  weights_list = vector("list", n_comps)
  ind_list = vector("list", n_comps)
  
  for (j in seq_len(n_comps)) {
    nonzero = (A[, j] != 0)
    weights_list[[j]] = A[nonzero, j]
    ind_list[[j]] = which(nonzero)
    #cor_with_pc  
    cor_with_pc[j] =
      sum(A[ind_list[[j]], j] *
            ab(S[ind_list[[j]], ,drop = FALSE],
               s_ee$vectors[, j, drop = FALSE])) /
      sqrt(
        vtau(A[ind_list[[j]], j], 
             S[ind_list[[j]], ind_list[[j]], drop = FALSE],
             A[ind_list[[j]], j]) *  s_ee$values[j]
      )
    
  }
  
  
  
  cor_with_pc = pmax(-1, pmin(1, cor_with_pc))
  
  
  obj = list()
  obj$weights = A
  obj$contributions = make_contributions(A)
  dimnames(obj$contributions) = dimnames(A)
  obj$n_comps = n_comps
  obj$cardinality = colSums(A != 0)
  
  totv = sum(s_ee$values)
  
  vexp = make_vexp(A, S)
  obj$vexp = vexp$vexp/totv
  obj$vexp_pc = s_ee$values[1:obj$n_comps]/totv
  obj$cvexp = vexp$cvexp/totv
  obj$rvexp = vexp$vexp/s_ee$values[1:n_comps]
  obj$rcvexp = vexp$cvexp/cumsum(s_ee$values[seq_len(n_comps)])
  
  obj$cor_with_pc = cor_with_pc
  obj$tot_var = totv
  
  obj$indices = ind_list
  obj$weights_list = weights_list
  if(!is.null(X)){
    obj$scores = ab(X, A)
    obj$spc_cor = makeCorScoresC(obj$scores)
  }
  else{
    obj$spc_cor = make_spc_cor_S(A, S)
  }
  dimnames(obj$spc_cor) = list(paste0("sPC", seq_len(n_comps)),
                               paste0("sPC", seq_len(n_comps)))
  obj$method_name = method_name
  
  class(obj) = c("spca", "list")
  
  return(obj)
}

#METHODS-------------------------------
#change_weights_sign================
##deprecated-----------------------
#' Change Component Signs in an SPCA Object (Deprecated Alias)
#'
#' `change_weights_sign_spca()` is retained for backward compatibility.
#' Use [change_sign()] in new code.
#'
#' @param spca_obj An object of class \code{spca}.
#' @param index_to_change An integer vector of component indices whose signs
#'   should be changed.
#' @return The modified `spca_obj`.
#' @family spca
#' @export
#' @keywords internal
change_weights_sign_spca = function(spca_obj, index_to_change) {
  .Deprecated("change_sign")
  change_sign(spca_obj, index_to_change = index_to_change)
}
##deprecated=============
#' Change Component Signs in an SPCA Object (Deprecated Alias)
#'
#' `change_loadings_sign_spca()` is retained for backward compatibility.
#' Use [change_sign()] in new code.
#'
#' @param spca_obj An object of class \code{spca}.
#' @param index_to_change An integer vector of component indices whose signs
#'   should be changed.
#' @return The modified `spca_obj`.
#' @family spca
#' @export
#' @keywords internal
change_loadings_sign_spca = function(spca_obj, index_to_change) {
  .Deprecated("change_sign")
  change_sign(spca_obj, index_to_change = index_to_change)
}

# change_sign=============
#' Change Component Signs in a \code{pca} or \code{spca} Object
#'
#' Change the signs of selected Weights in a fitted object.
#'
#' @param object An object of class \code{pca} or \code{spca}.
#' @param spca_obj Deprecated alias for \code{object} (default \code{NULL}).
#'   Supply only one of \code{object} and \code{spca_obj}. Using the old
#'   argument name issues a warning.
#' @param index_to_change An integer vector of component indices whose signs
#'   should be changed.
#' @return The modified object, preserving its classes.
#' @examples
#' data(holzinger)
#' ho_cspca = spca(holzinger, n_comps = 4)
#' show_correlations(ho_cspca) 
#' # In applications we would change only the fourth set of weights
#' ho_changed = change_sign(ho_cspca, index_to_change = c(2, 4))
#' is.spca(ho_changed)
#' show_weights(ho_cspca, cols = 2)
#' show_weights(ho_changed, cols = 2)
#' show_correlations(ho_changed) 
#' @family spca
#' @family pca
#' @export
change_sign = function(object, index_to_change,
    spca_obj = NULL) {
  if (!missing(spca_obj)) {
    if (!missing(object)) {
      stop("Supply only one of `object` and `spca_obj`.",
           call. = FALSE)
    }
    warning("`spca_obj` is deprecated; use `object` instead.",
            call. = FALSE)
    return(change_sign(
      object = spca_obj,
      index_to_change = index_to_change
    ))
  }

  UseMethod("change_sign")
}

#' @rdname change_sign
#' @exportS3Method
change_sign.spca = function(object, index_to_change,
    spca_obj = NULL) {
  if (!missing(spca_obj)) {
    if (!missing(object)) {
      stop("Supply only one of `object` and `spca_obj`.",
           call. = FALSE)
    }
    warning("`spca_obj` is deprecated; use `object` instead.",
            call. = FALSE)
    object = spca_obj
  }

  
  if (length(index_to_change) < 1L ||
      !is.numeric(index_to_change) ||
      anyNA(index_to_change) ||
      any(index_to_change != as.integer(index_to_change)) ||
      any(index_to_change < 1L) ||
      any(index_to_change > object$n_comps)) {
    stop("index_to_change must contain valid component indices",
         call. = FALSE)
  }
  
  weights = .get_spca_weights(object)
  weights_list = .get_spca_weights_list(object, required = FALSE)
  
  for (i in as.integer(index_to_change)) {
    weights[, i] = -weights[, i]
    object$contributions[, i] = -object$contributions[, i]
    
    if (!is.null(weights_list))
      weights_list[[i]] = -weights_list[[i]]
    if (!is.null(object$scores))
      object$scores[, i] = -object$scores[, i]
    if (!is.null(object$spc_cor)) {
      object$spc_cor[i, ] = -object$spc_cor[i, ]
      object$spc_cor[, i] = -object$spc_cor[, i]
    }
    if (!is.null(object$cor_with_pc))
      object$cor_with_pc[i] = -object$cor_with_pc[i]
  }
  
  if (!is.null(object$weights))
    object$weights = weights
  else
    object$loadings = weights
  
  if (!is.null(weights_list)) {
    if (!is.null(object$weights_list))
      object$weights_list = weights_list
    else
      object$loadings_list = weights_list
  }
  
  object
}

# show_weights==================
#' Show the Weights or Contributions of a \code{pca} or \code{spca} Object
#'
#' Show selected nonzero component weights or their unit-L1 contributions.
#'
#' @param object An object of class \code{pca} or \code{spca}.
#' @param spca_obj Deprecated alias for \code{object} (default \code{NULL}).
#'   Supply only one of \code{object} and \code{spca_obj}. Using the old
#'   argument name issues a warning.
#' @param cols An integer vector or \code{NULL}. Components to show.
#' @param contribution A logical value. If \code{TRUE}, show unit-L1
#'   contributions; otherwise, show the original nonzero weights.
#' @param print_list A logical value indicating whether to print the result.
#' @param return_list A logical value indicating whether to return the result.
#' @param ... Additional arguments reserved for S3 method compatibility.
#' @return The selected weights or contributions when requested; otherwise
#'   \code{NULL} invisibly.
#' @family spca
#' @family pca
#' @export
show_weights = function(
    object, cols = NULL, contribution = TRUE, print_list = TRUE,
    return_list = FALSE, ...,
    spca_obj = NULL) {
  if (!missing(spca_obj)) {
    if (!missing(object)) {
      stop("Supply only one of `object` and `spca_obj`.",
           call. = FALSE)
    }
    warning("`spca_obj` is deprecated; use `object` instead.",
            call. = FALSE)
    return(show_weights(
      object = spca_obj,
      cols = cols,
      contribution = contribution,
      print_list = print_list,
      return_list = return_list,
      ...
    ))
  }

  UseMethod("show_weights")
}

#' @rdname show_weights
#' @exportS3Method
show_weights.spca = function(
    object, cols = NULL, contribution = TRUE, print_list = TRUE,
    return_list = FALSE, ...,
    spca_obj = NULL) {
  if (!missing(spca_obj)) {
    if (!missing(object)) {
      stop("Supply only one of `object` and `spca_obj`.",
           call. = FALSE)
    }
    warning("`spca_obj` is deprecated; use `object` instead.",
            call. = FALSE)
    object = spca_obj
  }

  
  if (!validate_spca(object))
    stop("show_weights requires an spca object as first argument",
         call. = FALSE)
  validate_booleans(
    contribution = contribution,
    print_list = print_list,
    return_list = return_list
  )
  
  if (is.null(cols))
    cols = seq_along(object$vexp)
  if (!is.numeric(cols) || anyNA(cols) ||
      any(cols != as.integer(cols)) ||
      any(cols < 1L) || any(cols > object$n_comps)) {
    stop("cols must contain valid component indices", call. = FALSE)
  }
  
  values = .get_spca_weights_list(object)[as.integer(cols)]
  if (contribution)
    values = lapply(values, function(a) a / sum(abs(a)))
  if (length(values) == 1L)
    values = values[[1L]]
  
  if (print_list) {
    message(ifelse(contribution, "Percentage contributions", "Weights"))
    print(values)
  }
  if (return_list)
    return(values)
  invisible(NULL)
}

## show_contributions_spca ==================
#' Shows the non-zero contributions separately for each component (Deprecated).
#' Use \code{show_weights()} instead.. 
#' It just turns an spca object loadings_list into a list of loadings 
#' 
#' @param spca_obj An spca object
#' 
#' @param cols A vector containing the indices of the loadings to be shown.  Can
#'  be a single value. if missing all loadings are shown: If an integer is
#'  passed, only that dimension will be returned.
#' @details
#'  Deprecated, will not work for spca version > 1.1.1. Use \code{show_weights()}.
#' @param return_list Logical: if `TRUE` the list is returned
#' @family spca
#' @keywords internal
#' @export 
show_contributions_spca = function(spca_obj, cols = NULL, return_list = FALSE)
{
  test = validate_spca(spca_obj)
  if (!test)
    stop("show_loadings requires an spca object as first argument")
  
  if(is.null(cols)){
    cols = seq_along(spca_obj$vexp)
  }
  
  contributions = lapply(spca_obj$loadings_list, function(x) x/ sum(abs(x)))
  
  message("Percentage Contributions")
  print(contributions)
  
  if (return_list == TRUE)
    return(contributions)
  
  invisible()
}

#show_correlations===========
#' Show Component Correlations for a \code{pca} or \code{spca} Object
#'
#' Print and optionally return the mutual correlations among sparse principal
#' components and their correlations with the corresponding principal
#' components.
#'
#' @param object An object of class \code{pca} or \code{spca}.
#' @param spca_obj Deprecated alias for \code{object} (default \code{NULL}).
#'   Supply only one of \code{object} and \code{spca_obj}. Using the old
#'   argument name issues a warning.
#' @param type A character value specifying which correlations to show. Values
#'   beginning with \code{"s"}, \code{"p"}, or \code{"b"} select the mutual
#'   sPC correlations, the correlations with the corresponding PCs, or both,
#'   respectively. The default is \code{"both"}.
#' @param digits A non-negative integer scalar (default \code{2}). Number of
#'   decimal places used for printing. Returned matrices are not rounded.
#' @param print_matrices A logical value (default \code{TRUE}). If \code{TRUE},
#'   print the requested correlations.
#' @param return_matrices A logical value (default \code{FALSE}). If
#'   \code{TRUE}, return the requested unrounded numeric matrix or matrices.
#'
#' @return If \code{return_matrices = TRUE}, a numeric matrix when one type of
#'   correlation is requested, or a named list of two numeric matrices when
#'   \code{type = "both"}. Otherwise, returns \code{NULL} invisibly.
#'
#' @examples
#' data(holzinger)
#' ho_cspca = spca(holzinger, n_comps = 3)
#' show_correlations(ho_cspca)
#' show_correlations(ho_cspca, type = "s", return_matrices = TRUE)
#'
#' @family spca
#' @family pca
#' @export
show_correlations = function(
    object, type = c("both", "spcs", "pcs"), digits = 2, 
    print_matrices = TRUE, return_matrices = FALSE,
    spca_obj = NULL) {
  if (!missing(spca_obj)) {
    if (!missing(object)) {
      stop("Supply only one of `object` and `spca_obj`.",
           call. = FALSE)
    }
    warning("`spca_obj` is deprecated; use `object` instead.",
            call. = FALSE)
    return(show_correlations(
      object = spca_obj,
      type = type,
      digits = digits,
      print_matrices = print_matrices,
      return_matrices = return_matrices
    ))
  }

  UseMethod("show_correlations")
}

#' @rdname show_correlations
#' @exportS3Method
show_correlations.spca = function(
    object, type = c("both", "spcs", "pcs"), digits = 2, 
    print_matrices = TRUE, return_matrices = FALSE,
    spca_obj = NULL) {
  if (!missing(spca_obj)) {
    if (!missing(object)) {
      stop("Supply only one of `object` and `spca_obj`.",
           call. = FALSE)
    }
    warning("`spca_obj` is deprecated; use `object` instead.",
            call. = FALSE)
    object = spca_obj
  }

  
  if (!is.spca(object))
    stop("`show_correlations()` requires an `spca` object as first argument.",
         call. = FALSE)
  
  if (!is.character(type) || length(type) < 1L || is.na(type[1L]) ||
      !nzchar(type[1L]))
    stop("`type` must begin with 's', 'p', or 'b'.", call. = FALSE)
  
  type = substr(tolower(type[1L]), 1L, 1L)
  if (!type %in% c("s", "p", "b"))
    stop("`type` must begin with 's', 'p', or 'b'.", call. = FALSE)
  
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      !is.finite(digits) || digits < 0 || digits != as.integer(digits))
    stop("`digits` must be a non-negative integer scalar.", call. = FALSE)
  digits = as.integer(digits)
  
  if (!is.logical(print_matrices) || length(print_matrices) != 1L ||
      is.na(print_matrices))
    stop("`print_matrices` must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(return_matrices) || length(return_matrices) != 1L ||
      is.na(return_matrices))
    stop("`return_matrices` must be TRUE or FALSE.", call. = FALSE)
  
  need_spc = type %in% c("s", "b")
  need_pc = type %in% c("p", "b")
  spc_correlations = NULL
  spc_pc_correlations = NULL
  
  if (need_spc) {
    spc_correlations = object[["spc_cor"]]
    
    if (is.null(spc_correlations)) {
      scores = object[["scores"]]
      if (is.matrix(scores) && is.numeric(scores))
        spc_correlations = stats::cor(scores)
    }
    
    if (!is.matrix(spc_correlations) || !is.numeric(spc_correlations) ||
        nrow(spc_correlations) != ncol(spc_correlations) ||
        anyNA(spc_correlations)) {
      stop("Mutual correlations among the sPCs are not available in this object.",
           call. = FALSE)
    }
    
    spc_names = paste0("sPC", seq_len(nrow(spc_correlations)))
    dimnames(spc_correlations) = list(spc_names, spc_names)
  }
  
  if (need_pc) {
    cor_with_pc = object[["cor_with_pc"]]
    
    if (!is.numeric(cor_with_pc) || length(cor_with_pc) < 1L ||
        anyNA(cor_with_pc)) {
      stop(paste(
        "Correlations between sPCs and the corresponding PCs are not",
        "available in this object."
      ), call. = FALSE)
    }
    
    spc_pc_correlations = matrix(
      as.numeric(cor_with_pc), nrow = 1L,
      dimnames = list("sPC-PC", paste0("PC", seq_along(cor_with_pc)))
    )
  }
  
  if (type == "b" &&
      ncol(spc_correlations) != ncol(spc_pc_correlations)) {
    stop(paste(
      "The number of sPC correlations does not match the number of",
      "correlations with the corresponding PCs."
    ), call. = FALSE)
  }
  
  if (print_matrices) {
    if (type == "s") {
      print(round(spc_correlations, digits = digits))
    } else if (type == "p") {
      print(round(spc_pc_correlations, digits = digits))
    } else {
      n_comps = ncol(spc_correlations)
      spc_print = matrix(
        formatC(spc_correlations, format = "f", digits = digits),
        nrow = n_comps,
        dimnames = dimnames(spc_correlations)
      )
      separator = matrix(
        "-----", nrow = 1L, ncol = n_comps,
        dimnames = list("", colnames(spc_print))
      )
      pc_print = matrix(
        formatC(spc_pc_correlations, format = "f", digits = digits),
        nrow = 1L,
        dimnames = list("sPC-PC", colnames(spc_print))
      )
      print(rbind(spc_print, separator, pc_print),
            quote = FALSE, right = TRUE)
    }
  }
  
  if (return_matrices) {
    if (type == "s")
      return(spc_correlations)
    if (type == "p")
      return(spc_pc_correlations)
    return(list(
      spc_correlations = spc_correlations,
      spc_pc_correlations = spc_pc_correlations
    ))
  }
  
  invisible(NULL)
}


# aggregate_by_group==================
#' Aggregate Weights or Contributions of a \code{pca} or \code{spca} Object by Group
#'
#' Aggregate component weights or contributions according to a grouping
#' variable.
#' @param object An object of class \code{pca} or \code{spca}.
#' @param spca_obj Deprecated alias for \code{object} (default \code{NULL}).
#'   Supply only one of \code{object} and \code{spca_obj}. Using the old
#'   argument name issues a warning.
#' @param groups A vector or factor with one group label per variable.
#' @param only_nonzero A logical value indicating whether to omit groups whose
#'   values are zero in every selected component.
#' @param contributions A logical value. If \code{TRUE}, aggregate percentage
#'   contributions; otherwise, aggregate weights.
#' @param digits Number of digits used in printed output.
#' @param print_table A logical value indicating whether to print the table.
#' @param return_table A logical value indicating whether to return the table
#'   visibly.
#' @family spca
#' @family pca
#' @export
aggregate_by_group = function(object, 
                              groups, 
                              only_nonzero = TRUE,
                              contributions = TRUE,
                              digits = ifelse(contributions, 1, 3), 
                              print_table = TRUE,
                              return_table = FALSE,
    spca_obj = NULL) {
  if (!missing(spca_obj)) {
    if (!missing(object)) {
      stop("Supply only one of `object` and `spca_obj`.",
           call. = FALSE)
    }
    warning("`spca_obj` is deprecated; use `object` instead.",
            call. = FALSE)
    return(aggregate_by_group(
      object = spca_obj,
      groups = groups,
      only_nonzero = only_nonzero,
      contributions = contributions,
      digits = digits,
      print_table = print_table,
      return_table = return_table
    ))
  }

  UseMethod("aggregate_by_group")
}

#' @rdname aggregate_by_group
#' @exportS3Method
aggregate_by_group.spca = function(
    object, groups, only_nonzero = TRUE, contributions = TRUE,
    digits = ifelse(contributions, 1, 3), print_table = TRUE,
    return_table = FALSE,
    spca_obj = NULL) {
  if (!missing(spca_obj)) {
    if (!missing(object)) {
      stop("Supply only one of `object` and `spca_obj`.",
           call. = FALSE)
    }
    warning("`spca_obj` is deprecated; use `object` instead.",
            call. = FALSE)
    object = spca_obj
  }

  
  if (!validate_spca(object))
    stop("aggregate_by_group requires an spca object as first argument",
         call. = FALSE)
  validate_booleans(
    only_nonzero = only_nonzero,
    contributions = contributions,
    print_table = print_table,
    return_table = return_table
  )
  if ((!is.vector(groups) && !is.factor(groups)) || anyNA(groups))
    stop("groups must be a vector or factor without missing values",
         call. = FALSE)
  if (length(groups) != nrow(.get_spca_weights(object)))
    stop("groups must have one element per variable", call. = FALSE)
  
  if (contributions)
    values = object$contributions
  else
    values = .get_spca_weights(object)
  
  out = rowsum(values, group = groups, reorder = FALSE)
  if (only_nonzero)
    out = out[rowSums(abs(out)) > 1e-4, , drop = FALSE]
  
  if (print_table) {
    if (contributions) {
      out_print = matrix(
        sprintf(paste0("%.", digits, "f%%"), round(out * 100, digits)),
        nrow = nrow(out), dimnames = dimnames(out)
      )
      out_print[out == 0] = ""
      message("Percentage contributions")
    } else {
      out_print = format(out, digits = digits, justify = "right")
      out_print[out == 0] = ""
      message("Weights")
    }
    print(out_print, quote = FALSE, right = TRUE)
  }
  
  if (return_table)
    return(out)
  invisible(out)
}




