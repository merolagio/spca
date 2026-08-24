# validate_spca==========================
#' Validate the Structure of an SPCA Object
#'
#' Check the core elements of an \code{spca} object, including weights,
#' explained-variance summaries, component counts, cardinalities, weight lists,
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
    "vexp_pc", "vexp", "cvexp", "rvexp", "rcvexp", "n_comps",
    "cardinality", "indices", "spc_cor"
  )
  
  msg = list()
  success = TRUE
  
  add_msg = function(...) {
    msg[[length(msg) + 1]] <<- paste(..., collapse = " ")
    success <<- FALSE
  }
  
  if (!is.list(x)) {
    if(!inherits(x, "spca"))
      add_msg("Object is not of class spca")
    
    miss = needed
    add_msg("Object is not a list, so it cannot be an spca object")
    add_msg("Missing elements:", paste(miss, collapse = ", "))
  } else if (!is.spca(x)) {
    miss = needed[!(needed %in% names(x))]
    if (is.null(.get_spca_weights(x, required = FALSE)))
      miss = c(miss, "weights or legacy loadings")
    if (is.null(.get_spca_weights_list(x, required = FALSE)))
      miss = c(miss, "weights_list or legacy loadings_list")
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
  
  # validate_weights ==================
  # necessary:if invalid weights cannot proceed. if passes
  # all elements are validated to inform which aren't valid
  msg_load = list()
  load_ok = TRUE
  
  add_msg_load = function(...) {
    msg_load[[length(msg_load) + 1]] <<- paste(..., collapse = " ")
    load_ok <<- FALSE
  }
  
  weights = .get_spca_weights(x, required = FALSE)

  if (is.null(weights)) {
    add_msg_load("spca objects need a weights matrix; weights is missing")
  } else {
    if (is.vector(weights)) {
      add_msg_load("spca objects need a weights matrix, not a vector")
    } else if (!is.matrix(weights)) {
      add_msg_load("weights must be a matrix")
    } else if (!is.numeric(weights)) {
      add_msg_load("weights must be numeric")
    } else {
      if (any(dim(weights) == 0)) {
        add_msg_load("The weights matrix cannot have zero dimensions")
      }
      
      if (anyNA(weights)) {
        add_msg_load("The weights matrix cannot contain missing values")
      }
      
      if (any(colSums(abs(weights)) <= tol)) {
        add_msg_load("No column of weights can be all zeroes")
      }
      
      if (any(abs(colSums(weights^2) - 1) > tol)) {
        add_msg_load("Each column of weights must have unit L2 norm")
      }
    }
  }
  
  if (!load_ok) {
    success = FALSE
    msg = c(msg, msg_load)
    
    if (!quiet) {
      warning("spca objects must have a valid weights matrix",
              call. = FALSE)
      print(msg)
    }
    
    return(FALSE)
  }
  
  p = nrow(weights)
  ncomp = ncol(weights)
  
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
        add_msg("n_comps must be equal to ncol(weights)")
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
        add_msg("cardinality must have length ncol(weights)")
      }
      
      if (any(x$cardinality < 1 )) {
        add_msg("cardinality must contain positive integers")
      }
      
      if (any(x$cardinality > p )) {
        add_msg("cardinality elements must be smaller than nrow(weights)")
      }
      
      if (!anyNA(x$cardinality) &&
          all(abs(x$cardinality - round(x$cardinality)) <= tol)) {
        card_ref = as.integer(round(x$cardinality))
      }
    }
  }
  
  # validate weights_list ================
  
  weights_list = .get_spca_weights_list(x, required = FALSE)

  if (is.null(weights_list)) {
    add_msg("weights_list is missing")
  } else {
    if (!is.list(weights_list)) {
      add_msg("weights_list must be a list")
    } else {
      if (length(weights_list) != ncomp) {
        add_msg("weights_list must have length ncol(weights)")
      }
      
      for (i in seq_along(weights_list)) {
        xi = weights_list[[i]]
        nm = paste0("weights_list[[", i, "]]")
        
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
        add_msg("indices must have length ncol(weights)")
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
  if (ncol(weights) > 1){
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
          add_msg("spc_cor must have dimensions ncol(weights) x ncol(weights)")
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
        add_msg("scores must have ncol equal to ncol(weights)")
      }
    }
  }
  
  if ((!success) && (!quiet)) {
    warning("Invalid spca object", call. = FALSE)
    print(msg)
  }
  
  success
}

#' Test Whether an Object Contains Only Integers
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
  if (!is.list(x)) {
    fu(x)
  } else {
    all(sapply(x, fu))
  }
}


#' Validate That Arguments Do Not Contain Missing Values
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
  } else {
    args = arg_list
  }
  
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

#' Test for a Non-Missing Logical Scalar
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

#' Validate Non-Missing Logical Scalars
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
  
  if(is.null(arg_list)) {
    args = list(...)
  } else {
    args = arg_list
  }
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

#' Test for a Non-Missing Logical Scalar or NULL
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

#' Validate Non-Missing Logical Scalars or NULL Values
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
  
  if (is.null(arg_list)) {
    args = list(...)
  } else {
    args = arg_list
  }
  
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

#' Validate an Index Vector
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



#' Convert an Object to a Numeric Matrix Without Missing Values
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

#' Convert an Object to a Numeric Vector Without Missing Values
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

