
# is.spca==============
#' Test for spca objects
#'
#' Check whether an object has class \code{spca} and contains the core elements
#' required by the package.
#'
#' @param x An object to test.
#'
#' @details The function checks for class \code{spca} and for the presence of
#' the core elements used by the package, including loadings, contributions,
#' explained-variance summaries, component counts, cardinalities, loading lists,
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
is.spca <- function(x) {
  inherits(x, "spca") && 
  !is.null(x$loadings) &&
  !is.null(x$contributions) &&
  !is.null(x$vexp) &&
  !is.null(x$vexp_pc) &&
  !is.null(x$cvexp) &&
  !is.null(x$rvexp) &&
  !is.null(x$rcvexp) &&
  !is.null(x$n_comps) &&
  !is.null(x$cardinality) &&
  !is.null(x$loadings_list) &&
  !is.null(x$indices) 
}

#new.spca==================
#' Construct an spca object from a set of loadings
#'
#' Build an object of class \code{spca} from a loadings matrix and either a
#' covariance or correlation matrix, a data matrix, or both.
#'
#' @param A A numeric matrix of loadings.
#' @param S A numeric covariance or correlation matrix (default \code{NULL}).
#'   If \code{NULL}, \code{X} is used to estimate the covariance matrix.
#' @param X A numeric data matrix or data frame (default \code{NULL}). Used to
#'   compute \code{S} when \code{S = NULL}, and to compute scores when supplied.
#'   At least one of \code{S} or \code{X} must be provided.
#' @param method_name A character scalar or \code{NULL} (default \code{NULL}).
#'   Name of the method used to compute the loadings.
#'
#' @return An `spca` object.
#' @examples
#' set.seed(1)
#' A = round(matrix(runif(24, -1, 1), 12))
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
    stop("A must be a matrix of loadings")
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
  
  if(!isSymmetric(S))
    stop("S must be a symmetric covariance or correlation matrix")
  
  if (any((colSums(A^2) - 1) > 1e-5)){
    message("Scaling loadings to unit L2 norm")
    A = scale_columns(A, 2)
  }
  
  
  n_comps = ncol(A)
  
  # compute vexp and eigen(S)
  vexp = make_vexpSC(A, S)
  
  s_ee = eigen_sym(S)
  
  
  ind_list = apply(A, 2, function(x) which(x != 0))
  
  #cor with PCs
  cor_with_pc = numeric(n_comps)
  loadings_list = vector("list", n_comps)
  ind_list = vector("list", n_comps)
  
  for (j in seq_len(n_comps)) {
    nonzero = (A[, j] != 0)
    loadings_list[[j]] = A[nonzero, j]
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
  obj$loadings = A
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
  obj$loadings_list = loadings_list
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
  
  class(obj) = c("list", "spca")
  
  return(obj)
}

#' Change the sign of selected loadings in an spca object
#'
#' Flip the sign of selected sparse principal components in an \code{spca}
#' object.
#'
#' @param spca_obj An object of class \code{spca}.
#' @param index_to_change An integer vector of component indices whose signs
#'   should be flipped.
#'
#' @details The function multiplies by \eqn{-1} the selected columns of
#' \code{loadings} and \code{contributions}. It also updates
#' \code{loadings_list}, \code{scores}, and the corresponding rows and columns
#' of \code{spc_cor} when these elements are present. This is useful because
#' principal components and sparse principal components are defined only up to
#' sign.
#'
#' @return The modified \code{spca_obj}, with the selected components
#' sign-flipped.
#' @family spca
#' @export
change_loadings_sign_spca = function(spca_obj, index_to_change) {
  if (!is.spca(spca_obj))
    stop("The first argument must be an spca object")
  if (!is.vector(index_to_change)) {
    stop(paste("index_to_change must be an integer or vector of indices",
               index_to_change, "was passed"))
  }
  n = length(index_to_change)
  
  for (i in 1:n) {
    spca_obj$loadings[, index_to_change[i]] = 
      - spca_obj$loadings[, index_to_change[i]]
    
    spca_obj$contributions[, index_to_change[i]] = 
      - spca_obj$contributions[, index_to_change[i]]
    
    if (!is.null(spca_obj$loadings_list)) {
      spca_obj$loadings_list[[index_to_change[i]]] =
        - spca_obj$loadings_list[[index_to_change[i]]]
    }
    if (!is.null(spca_obj$scores))
      spca_obj$scores[, index_to_change[i]] =
      - spca_obj$scores[,index_to_change[i]]
    
    if (!is.null(spca_obj$spc_cor)) {
      spca_obj$spc_cor[index_to_change[i], ] =
        - spca_obj$spc_cor[index_to_change[i], ]
      spca_obj$spc_cor[, index_to_change[i]] = 
        - spca_obj$spc_cor[, index_to_change[i]]
    }
  }
  return(spca_obj)
}


## show_contributions_spca ==================
#' Show nonzero contributions by component
#'
#' Convert the nonzero loadings stored in an \code{spca} object into percentage
#' contributions for selected components.
#'
#' @param spca_obj An object of class \code{spca}.
#' @param cols An integer vector or \code{NULL} (default \code{NULL}).
#'   Components to show. If \code{NULL}, all components are shown. If a single
#'   component is supplied, a single contribution vector is produced.
#' @param print_list A logical value (default \code{TRUE}). If \code{TRUE}, the
#'   contribution list or vector is printed.
#' @param return_list A logical value (default \code{FALSE}). If \code{TRUE},
#'   the contribution list or vector is returned.
#'
#' @return If \code{return_list = TRUE}, returns the selected contributions.
#' Otherwise, returns \code{NULL} invisibly.
#' 
#' @examples
#' data(holzinger)
#' ho_cspca = spca(holzinger, n_comps = 2)
#' show_contributions_spca(ho_cspca)  
#' 
#' @family spca
#' @export 
show_contributions_spca = function(spca_obj, cols = NULL, print_list = TRUE, 
                                   return_list = FALSE) {
  test = validate_spca(spca_obj)
  if (!test)
    stop("show_loadings requires an spca object as first argument")
  
  if(is.null(cols)){
    cols = seq_along(spca_obj$vexp)
  }
  if(length(cols) > 1){
    contributions = lapply(spca_obj$loadings_list[cols], 
                           function(x) x/ sum(abs(x)))
  } else {
    contributions = spca_obj$loadings_list[[cols]]/ 
      sum(abs(spca_obj$loadings_list[[cols]]))
  }
  
  if(print_list){
    message("Percentage Contributions")
    print(contributions)
  }  
  if (return_list == TRUE)
    return(contributions)
  
  invisible()
}
#aggregate_by_group========
#' Aggregate loadings or contributions by group
#'
#' Compute group-level sums from a vector, matrix, data frame, or \code{spca}
#' object according to a grouping variable.
#'
#' @param X An \code{spca} object, numeric vector, numeric matrix, or numeric
#'   data frame containing loadings or contributions.
#' @param groups A vector or factor with one group label per variable. Its
#'   length must equal \code{length(X)} when \code{X} is a vector, or
#'   \code{nrow(X)} when \code{X} is a matrix, data frame, or \code{spca}
#'   object.
#' @param only_nonzero A logical value (default \code{TRUE}). If \code{TRUE},
#'   groups with zero total absolute contribution or loading are removed from
#'   the output.
#' @param contributions A logical value (default \code{TRUE}). If \code{TRUE},
#'   aggregate and print values as percentage contributions when possible. If
#'   \code{X} is an \code{spca} object, \code{X$contributions} is used when
#'   \code{contributions = TRUE}; otherwise, \code{X$loadings} is used.
#' @param digits An integer scalar (default \code{1} when
#'   \code{contributions = TRUE}, otherwise \code{3}). Number of digits used for
#'   rounding in the printed output.
#' @param print_table A logical value (default \code{TRUE}). If \code{TRUE},
#'   print the aggregated table.
#' @param return_table A logical value (default \code{FALSE}). If \code{TRUE},
#'   return the aggregated table.
#'
#' @details If \code{contributions = TRUE} but the input values do not sum to
#' one in absolute value, \code{contributions} is set to \code{FALSE} and
#' \code{digits} is set to 3. Aggregated sums are not expected to retain the
#' unit-norm property of the original loadings or contributions.
#'
#' @return Invisibly returns the aggregated numeric vector or matrix by default.
#' If \code{return_table = TRUE}, returns the same object visibly. Rows
#' correspond to groups and columns correspond to components when \code{X} is a
#' matrix, data frame, or \code{spca} object.
#'
#' @examples
#' data(holzinger)
#' data(holzinger_scales)
#' ho_cspca = spca(holzinger, n_comps = 2)
#' aggregate_by_group(ho_cspca, groups = holzinger_scales)
#'
#' @export
aggregate_by_group = function(X, groups, 
                              only_nonzero = TRUE, 
                              contributions = TRUE, 
                              digits = ifelse(contributions, 1, 3),
                              print_table = TRUE,
                              return_table = FALSE) 
{
##validation ===========  
  
  # chcking an spca object requires too much stack
  fun_inp = as.list(match.call(expand.dots = FALSE))[-(1:2)]
  fun_inp = lapply(fun_inp, eval, envir = environment())
  validate_no_na(arg_list = eval(fun_inp))  
  
  if(!is.spca(X))
    if (anyNA(X))
      stop("X cannot contain missing values")
  
  validate_booleans(only_nonzero= only_nonzero, contributions = 
                      contributions, print_table = print_table, return_table = return_table)
  
  if(is.spca(X)){
    if(contributions){
      X = X$contributions
    } else
      X = X$loadings
  } else {
    if (((!is.vector(X) && (!is.factor(X))) &&  (!is.matrix(X))))
      stop("X must be an spca object or a matrix or a vector")
  }
  n = ifelse((is.vector(X) || (is.factor(X))), length(X), nrow(X))
  
  if ((!is.vector(groups)) && (!is.factor(groups))){
    stop("groups must be a vector or a factor")
  } else {
    if(length(groups) != ifelse(is.vector(X), length(X), nrow(X)))
      stop("contributions and groups must have the same length")
  } 
  
  if(is.factor(groups))
    nams = levels(groups)
  else {
    if (!is.null(names(groups)))
      nams = names(groups)
    else
      nams = paste0("G", seq_along(unique(groups)))
  }
  
  #vector
  if ((is.vector(X)) || (is.factor(X))) {
    out = tapply(X, groups, sum)
    if ((abs(sum(abs(X)) - 1) > 10e-3) && contributions == TRUE){
      warning("The sum of values is not 1. Setting contributions = FALSE")
      contributions = FALSE
      digits = 3
    }
    names(out) = names(groups)
    if(only_nonzero)
      out = out[abs(out) > 10e-5]
  } 
  # matrix
  else {
    if (any(abs(colSums(abs(X)) - 1) > 10e-3) && (contributions == TRUE)){
      warning("The sum of values is not 1. Setting contributions = FALSE")
      contributions = FALSE
      digits = 3
    }
    out = apply(X, 2, function(y, ii) tapply(y, ii, sum), ii = groups)
    rownames(out) = nams
    if (only_nonzero)
      out = out[rowSums(abs(out)) > 10e-4, ]
  }
  
  if(print_table){
    if(contributions){
      print("percentage contributions")
      
      outp = matrix(
        sprintf("%.1f%%", round(out * 100, 1)),
        nrow = nrow(out),
        dimnames = dimnames(out)
      )
      outp[out == 0] = ""
      
      print(outp, quote = FALSE, right = TRUE)
    } else {
      print("loadings")
      outp = format(out, digits = 3, justify = "right")
      outp[out == 0] = ""
      
      print(outp, quote = FALSE, right = TRUE)
    }
  }
  if (return_table){
    return(out)
  }
  invisible(out)
}

# validate_spca==========================
#' Validate the structure of an spca object
#'
#' Check the core elements of an \code{spca} object, including loadings,
#' explained-variance summaries, component counts, cardinalities, loading lists,
#' indices, component correlations, PC correlations, and scores.
#'
#' @param x An object to validate as an \code{spca} object.
#' @param quiet A logical value (default \code{FALSE}). If \code{FALSE},
#'   warnings and validation messages are printed.
#' @param tol A numeric scalar (default \code{1e-4}). Numerical tolerance used
#'   in validation checks.
#'
#' @details A warning is issued for validation failures unless
#' \code{quiet = TRUE}.
#'
#' @return A logical value. Returns \code{TRUE} if validation succeeds and
#' \code{FALSE} otherwise.

#' @noRd
validate_spca = function(x, quiet = FALSE, tol = 1e-4) {
  
  # chcking a whole spca object requires too much stack
  fun_inp = as.list(match.call(expand.dots = FALSE))[-(1:2)]
  fun_inp = lapply(fun_inp, eval, envir = environment())
  validate_no_na(arg_list = eval(fun_inp))  

  needed = c(
    "loadings", "vexp_pc", "vexp", "cvexp", "rvexp", "rcvexp", "n_comps",
    "cardinality", "loadings_list", "indices", "spc_cor"
  )
  
  msg = list()
  success = TRUE
  
  add_msg = function(...) {
    msg[[length(msg) + 1]] <<- paste(..., collapse = " ")
    success <<- FALSE
  }
  
  if (!is.list(x)) {
    miss = needed
    add_msg("Object is not a list, so it cannot be an spca object")
    add_msg("Missing elements:", paste(miss, collapse = ", "))
  } else if (!is.spca(x)) {
    miss = needed[!(needed %in% names(x))]
    if (length(miss) > 0) {
      add_msg("Missing elements:", paste(miss, collapse = ", "))
    } else {
      add_msg("Object is not a valid spca object")
    }
  }
  
  is_num_vector = function(z) {
    is.atomic(z) && is.null(dim(z)) && is.numeric(z)
  }
  
  is_int_vector = function(z) {
    is_num_vector(z) && all(abs(z - round(z)) <= tol)
  }
  
  validate_num_vector = function(z,
                                 name,
                                 len = NULL,
                                 lower = NULL,
                                 upper = NULL,
                                 nondecreasing = FALSE,
                                 upper_vec = NULL) {
    ok = TRUE
    
    if (is.null(z)) {
      add_msg(name, "is missing")
      return(FALSE)
    }
    
    if (!is_num_vector(z)) {
      add_msg(name, "must be a numeric vector")
      return(FALSE)
    }
    
    if (length(z) == 0) {
      add_msg(name, "cannot be empty")
      ok = FALSE
    }
    
    if (anyNA(z)) {
      add_msg(name, "cannot contain missing values")
      ok = FALSE
    }
    
    if (!is.null(len) && (length(z) != len)) {
      add_msg(name, "must have length", len)
      ok = FALSE
    }
    
    if ((length(z) > 0) && all(abs(z) <= tol)) {
      add_msg(name, "cannot be all zeroes")
      ok = FALSE
    }
    
    if (!is.null(lower) && (length(z) > 0)) {
      if (any(z < lower - tol)) {
        add_msg(name, paste0("elements must be >= ", lower))
        ok = FALSE
      }
    }
    
    if (!is.null(upper) && (length(z) > 0)) {
      if (any(z > upper + tol)) {
        add_msg(name, paste0("elements must be <= ", upper))
        ok = FALSE
      }
    }
    
    if (nondecreasing && (length(z) > 1)) {
      if (any(diff(z) < -tol)) {
        add_msg(name, "must be nondecreasing")
        ok = FALSE
      }
    }
    
    if (!is.null(upper_vec) && is_num_vector(upper_vec)) {
      if ((length(z) == length(upper_vec)) &&
          !anyNA(z) &&
          !anyNA(upper_vec) &&
          any(z - upper_vec > tol)) {
        add_msg(
          name,
          "cannot be larger than the corresponding values of cumsum(vexp_pc)"
        )
        ok = FALSE
      }
    }
    
    ok
  }
  
  # validate_loadings ==================
  # necessary:if invalid loadings cannot proceed. if passes
  # all elements are validated to inform which aren't valid
  msg_load = list()
  load_ok = TRUE
  
  add_msg_load = function(...) {
    msg_load[[length(msg_load) + 1]] <<- paste(..., collapse = " ")
    load_ok <<- FALSE
  }
  
  if (is.null(x$loadings)) {
    add_msg_load("spca objects need a loadings matrix; loadings is missing")
  } else {
    if (is.vector(x$loadings)) {
      add_msg_load("spca objects need a loadings matrix, not a vector")
    } else if (!is.matrix(x$loadings)) {
      add_msg_load("loadings must be a matrix")
    } else if (!is.numeric(x$loadings)) {
      add_msg_load("loadings must be numeric")
    } else {
      if (any(dim(x$loadings) == 0)) {
        add_msg_load("The loadings matrix cannot have zero dimensions")
      }
      
      if (anyNA(x$loadings)) {
        add_msg_load("The loadings matrix cannot contain missing values")
      }
      
      if (any(colSums(abs(x$loadings)) <= tol)) {
        add_msg_load("No column of loadings can be all zeroes")
      }
      
      if (any(abs(colSums(x$loadings^2) - 1) > tol)) {
        add_msg_load("Each column of loadings must have unit L2 norm")
      }
    }
  }
  
  if (!load_ok) {
    success = FALSE
    msg = c(msg, msg_load)
    
    if (!quiet) {
      warning("spca objects must have a valid loadings matrix",
              call. = FALSE)
      print(msg)
    }
    
    return(FALSE)
  }
  
  p = nrow(x$loadings)
  ncomp = ncol(x$loadings)
  
  # validate_variance explained ==================
  
  validate_num_vector(
    z = x$vexp_pc,
    name = "vexp_pc",
    len = ncomp,
    lower = 0,
    upper = 1
  )
  

  vexp_ok = validate_num_vector(
    z = x$vexp,
    name = "vexp",
    len = ncomp,
    lower = 0,
    upper = 1
  )
 
  validate_num_vector(
    z = x$cvexp,
    name = "cvexp",
    len = ncomp,
    lower = 0,
    upper = 1,
    nondecreasing = TRUE,
    upper_vec = if (!is.null(x$vexp_pc) &&
                    is_num_vector(x$vexp_pc) &&
                    (length(x$vexp_pc) == ncomp) &&
                    !anyNA(x$vexp_pc)) {
      cumsum(x$vexp_pc)
    } else {
      NULL
    }
  )
  
   # rvexp 
  
  validate_num_vector(
    z = x$rvexp,
    name = "rvexp",
    len = ncomp,
    lower = 0
  )
  
  # rcvexp 
  
  validate_num_vector(
    z = x$rcvexp,
    name = "rcvexp",
    len = ncomp,
    lower = 0
  )
  
  # validate_n_comps and cardinality ==========
  
  if (is.null(x$n_comps)) {
    add_msg("n_comps is missing")
  } else {
    if (!is_num_vector(x$n_comps) || (length(x$n_comps) != 1)) {
      add_msg("n_comps must be a scalar")
    } else {
      if (anyNA(x$n_comps)) {
        add_msg("n_comps cannot contain missing values")
      }
      
      if (abs(x$n_comps - round(x$n_comps)) > tol) {
        add_msg("n_comps must be an integer")
      }
      
      if (abs(x$n_comps - ncomp) > tol) {
        add_msg("n_comps must be equal to ncol(loadings)")
      }
    }
  }
  
  # validate_cardinality 
  
  card_ref = NULL
  
  if (is.null(x$cardinality)) {
    add_msg("cardinality is missing")
  } else {
    if (!is_num_vector(x$cardinality)) {
      add_msg("cardinality must be an integer vector")
    } else {
      if (length(x$cardinality) == 0) {
        add_msg("cardinality cannot be empty")
      }
      
      if (anyNA(x$cardinality)) {
        add_msg("cardinality cannot contain missing values")
      }
      
      if (any(abs(x$cardinality - round(x$cardinality)) > tol)) {
        add_msg("cardinality must contain integers")
      }
      
      if (length(x$cardinality) != ncomp) {
        add_msg("cardinality must have length ncol(loadings)")
      }
      
      if (any(x$cardinality < 1 )) {
        add_msg("cardinality must contain positive integers")
      }
      
      if (any(x$cardinality > p )) {
        add_msg("cardinality elements must be smaller than nrow(loadings)")
      }
      
      if (!anyNA(x$cardinality) &&
          all(abs(x$cardinality - round(x$cardinality)) <= tol)) {
        card_ref = as.integer(round(x$cardinality))
      }
    }
  }
  
  # validate_loadlist ================
  
  if (is.null(x$loadings_list)) {
    add_msg("loadings_list is missing")
  } else {
    if (!is.list(x$loadings_list)) {
      add_msg("loadings_list must be a list")
    } else {
      if (length(x$loadings_list) != ncomp) {
        add_msg("loadings_list must have length ncol(loadings)")
      }
      
      for (i in seq_along(x$loadings_list)) {
        xi = x$loadings_list[[i]]
        nm = paste0("loadings_list[[", i, "]]")
        
        if (!is_num_vector(xi)) {
          add_msg(nm, "must be a numeric vector")
        } else {
          if (length(xi) == 0) {
            add_msg(nm, "cannot be empty")
          }
          
          if (anyNA(xi)) {
            add_msg(nm, "cannot contain missing values")
          }
          
          if (any(abs(xi) > 1 + tol)) {
            add_msg(nm, "elements must be in [-1, 1]")
          }
          
          if (!is.null(card_ref) &&
              (i <= length(card_ref)) &&
              (length(xi) != card_ref[i])) {
            add_msg(nm, "must have length equal to cardinality[", i, "]")
          }
          
          if ((length(xi) > 0) &&
              (abs(sum(xi^2) - 1) > tol)) {
            add_msg(nm, "must have unit L2 norm")
          }
          
          if ((length(xi) > 0) && all(abs(xi) <= tol)) {
            add_msg(nm, "cannot be all zeroes")
          }
        }
      }
    }
  }
  
  # validate_indices =================
  
  if (is.null(x$indices)) {
    add_msg("indices is missing")
  } else {
    if (!is.list(x$indices)) {
      add_msg("indices must be a list")
    } else {
      if (length(x$indices) != ncomp) {
        add_msg("indices must have length ncol(loadings)")
      }
      
      for (i in seq_along(x$indices)) {
        xi = x$indices[[i]]
        nm = paste0("indices[[", i, "]]")
        
        if (!is_int_vector(xi)) {
          add_msg(nm, "must be an integer vector")
        } else {
          if (length(xi) == 0) {
            add_msg(nm, "cannot be empty")
          }
          
          if (anyNA(xi)) {
            add_msg(nm, "cannot contain missing values")
          }
          
          if (!is.null(card_ref) &&
              (i <= length(card_ref)) &&
              (length(xi) != card_ref[i])) {
            add_msg(nm, "must have length equal to cardinality[", i, "]")
          }
          
          if (any((xi < 1 - tol) | (xi > p + tol))) {
            add_msg(
              nm,
              paste0("elements must be integers in [1, ", p, "]")
            )
          }
        }
      }
    }
  }
  if (ncol(x$loadings) > 1){
    if (is.null(x$spc_cor)) {
      add_msg("spc_cor is missing")
    } else {
      if (is.vector(x$spc_cor)) {
        add_msg("spc_cor must be a matrix, not a vector")
      } else if (!is.matrix(x$spc_cor)) {
        add_msg("spc_cor must be a matrix")
      } else if (!is.numeric(x$spc_cor)) {
        add_msg("spc_cor must be numeric")
      } else {
        if (any(dim(x$spc_cor) == 0)) {
          add_msg("spc_cor cannot have zero dimensions")
        }
        
        if (anyNA(x$spc_cor)) {
          add_msg("spc_cor cannot contain missing values")
        }
        
        if ((nrow(x$spc_cor) != ncomp) || (ncol(x$spc_cor) != ncomp)) {
          add_msg("spc_cor must have dimensions ncol(loadings) x ncol(loadings)")
        }
      }
    }
  }
  
  # validate_cor_with_pc ======================
  
  if (!is.null(x$cor_with_pc)) {
    validate_num_vector(
      z = x$cor_with_pc,
      name = "cor_with_pc",
      len = ncomp,
      lower = -1,
      upper = 1
    )
  }
  
  # validate_scores ==================
  
  if (!is.null(x$scores)) {
    if (is.vector(x$scores)) {
      add_msg("scores must be a matrix, not a vector")
    } else if (!is.matrix(x$scores)) {
      add_msg("scores must be a matrix")
    } else if (!is.numeric(x$scores)) {
      add_msg("scores must be numeric")
    } else {
      if (ncol(x$scores) == 0) {
        add_msg("scores cannot have all zero columns")
      }
      
      if (anyNA(x$scores)) {
        add_msg("scores cannot contain missing values")
      }
      
      if (any(colSums(abs(x$scores)) <= tol)) {
        add_msg("No column of scores can be all zeroes")
      }
      
      if (ncol(x$scores) != ncomp) {
        add_msg("scores must have ncol equal to ncol(loadings)")
      }
    }
  }
  
  if ((!success) && (!quiet)) {
    warning("Invalid spca object", call. = FALSE)
    print(msg)
  }
  
  success
}

#' Test whether an object contains only integers
#'
#' Check whether an object, including a list, contains only finite, non-missing
#' integer-valued numeric entries.
#'
#' @param x An object to test.
#'
#' @return A logical value.
#' @noRd
is_int = function(x) {
  fu = function(x){
    is.numeric(x) &&
      !anyNA(x) &&
      all(is.finite(x)) &&
      all(x == floor(x))
  }
  if (!is.list(x))
    fu(x)
  else
    all(sapply(x, fu))
}


#' Validate that arguments do not contain missing values
#'
#' Stop if any supplied argument contains missing values.
#'
#' @param ... Arguments to check.
#' @param arg_list An optional named list of arguments to check (default
#'   \code{NULL}). If supplied, it takes precedence over \code{...}.
#'
#' @return A logical value. Returns \code{TRUE} invisibly if validation
#' succeeds; otherwise, throws an error.
#' @noRd
validate_no_na = function(..., arg_list = NULL) {
  
  if (is.null(arg_list)) {
    args = list(...)
  }
  else 
    args = arg_list
  
  has_na = function(x) {
    if (is.null(x))
      return(FALSE)
    
    if (is.list(x))
      return(any(vapply(x, has_na, logical(1))))
    
    anyNA(x)
  }
  
  bad = vapply(args, has_na, logical(1))
  
  if (any(bad)) {
    stop(
      paste(
        paste(names(args)[bad], collapse = " and "),
        "must not contain missing values"
      ),
      call. = FALSE
    )
  }
  
  TRUE
}

#' Test for a non-missing logical scalar
#'
#' Check whether an object is exactly \code{TRUE} or \code{FALSE}. Unlike
#' \code{is.logical()}, this returns \code{FALSE} for \code{NA}.
#'
#' @param x An object to test.
#'
#' @return A logical value.
#' @noRd
is_boolean = function(x) {
  isTRUE(x) || isFALSE(x)
}

#' Validate non-missing logical scalars
#'
#' Stop if any supplied argument is not exactly \code{TRUE} or \code{FALSE}.
#'
#' @param ... Arguments to check.
#' @param arg_list An optional named list of arguments to check (default
#'   \code{NULL}). If supplied, it takes precedence over \code{...}.
#'
#' @return A logical value. Returns \code{TRUE} if validation succeeds;
#' otherwise, throws an error.
#' @noRd
validate_booleans = function(..., arg_list = NULL) {
  
  if(is.null(arg_list))
    args = list(...)
  else
    args = arg_list
  bad = !vapply(args, is_boolean, logical(1))
  
  if (!any(bad))
    return(TRUE)
  stop(
    paste(
      paste(names(args)[bad], collapse = " and "),
      "must be TRUE or FALSE"
    ),
    call. = FALSE
  )
  
  FALSE
}

#' Test for a non-missing logical scalar or NULL
#'
#' Check whether an object is \code{NULL}, \code{TRUE}, or \code{FALSE}.
#'
#' @param x An object to test.
#'
#' @return A logical value.
#' @noRd
is_boolean_or_null = function(x) {
  is.null(x) || is_boolean(x)
}

#' Validate non-missing logical scalars or NULL values
#'
#' Stop if any supplied argument is not \code{NULL}, \code{TRUE}, or
#' \code{FALSE}.
#'
#' @param ... Arguments to check.
#' @param arg_list An optional named list of arguments to check (default
#'   \code{NULL}). If supplied, it takes precedence over \code{...}.
#'
#' @return A logical value. Returns \code{TRUE} if validation succeeds;
#' otherwise, throws an error.
#' @noRd
validate_booleans_or_null = function(..., arg_list = NULL) {
  
  if (is.null(arg_list))
    args = list(...)
  else
    args = arg_list
  
  bad = !vapply(args, is_boolean_or_null, logical(1))
  
  if (!any(bad))
    return(TRUE)
  
  stop(
    paste(
      paste(names(args)[bad], collapse = " and "),
      "must be NULL, TRUE, or FALSE"
    ),
    call. = FALSE
  )
  
  FALSE
}

#' Validate an index vector
#'
#' Check whether \code{x} is a vector of integer indices, optionally bounded
#' above by \code{max_val}.
#'
#' @param x A vector of positive integer indices.
#' @param max_val A numeric scalar or \code{NULL} (default \code{NULL}).
#'   Maximum allowed index value.
#'
#' @details A warning is issued upon failure.
#'
#' @return A logical value. Returns \code{TRUE} if validation succeeds and
#' \code{FALSE} otherwise.
#' @noRd
validate_index_vector = function(x, max_val = NULL) {
  success = TRUE
  if (!is.vector(x)) {
    warning("x must be a vector")
    return(FALSE)
  }
  if (any(x - (x%/%1) != 0)) {
    warning("x must be a vector of indices")
    return(FALSE)
  }
  if ((!is.null(max_val)) && any(x > max_val)) {
    warning(paste("The indices cannot be larger than", max_val))
    return(FALSE)
  }
  success
}



#' Convert an object to a numeric matrix without missing values
#'
#' Convert a matrix or data frame to a numeric matrix and stop if it contains
#' missing values.
#'
#' @param x A matrix or data frame.
#' @param name A character scalar used in error messages.
#'
#' @return A numeric matrix.
#' @noRd
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

#' Convert an object to a numeric vector without missing values
#'
#' Validate a numeric vector and stop if it contains missing values.
#'
#' @param x A vector.
#' @param name A character scalar used in error messages.
#'
#' @return A numeric vector.
#' @noRd
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

