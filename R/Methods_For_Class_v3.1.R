
# is.spca==============
#' Test for spca objects
#' 
#' Verifies whether all elements required for an `spca` object are present.
#' 
#' @param x Any object suspected of being of class spca.
#' 
#' @details The function carries out checks for the minimal components of an
#' \emph{spca} object. It checks the presence and mode of
#'  a matrix of loadings named \emph{loadings} 
#'  a vector of variance explained by the components named \emph{vexp} 
#'  a vector of variance explained by the PCs named \emph{vexp_pc}
#' If any of these is missing the object is deemed as not \emph{spca} and a
#' warning is issued, even if the object's class is \emph{spca}.
#' @return Logical: TRUE if object is of class spca and contains the minimal configuration, 
#' FALSE otherwise.
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
# validate_spca==========================
#' Validate the existence and correctness of core spca elements
#'
#' Checks loadings first, then explained-variance summaries, component counts,
#' cardinality, loadings_list, indices, and,  `cor_with_pc` and `scores`.
#' 
#' @param x an`spca` object
#' @param quiet if `FALSE` warnings are printed
#' @param tol the tolerance for 

#' @details A warning is issued upon each failure.
#' 
#' @returns TRUE or FALSE.

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
#new.spca==================
#' Constructs an object of class spca from a set of loadings
#' 
#' @param A real matrix, a matrix of loadings.
#' @param S the variance or correlation matrix from which the loadings where
#'  computed.
#' @param X real matrix, the data matrix from which the loadings where computed,
#'   optional. One of S or X must be valid inputs.
#' @param method_name string, the name of the method_name used to compute the
#'  loadings, optional.
#' @return An [spca_object].
#' @family spca
#' @export 
new_spca = function(A, S = NULL, X = NULL, method_name = NULL){
  
# browser()
   fun_inp = list(A = A, S = S, X = X, method_name = method_name)
  validate_no_na(arg_list = fun_inp)  
  
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
    A = standardize_data(A, center = FALSE, scale = TRUE)
  }
#browser() 
  
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
                s_ee$vec[, j, drop = FALSE])) /
      sqrt(
        vtau(A[ind_list[[j]], j], 
              S[ind_list[[j]], ind_list[[j]], drop = FALSE],
              A[ind_list[[j]], j]) *  s_ee$val[j]
      )
    
  }

  
  
  cor_with_pc = pmax(-1, pmin(1, cor_with_pc))
  
   
  obj = list()
  obj$loadings = A
  obj$contributions = make_contributions(A)
  dimnames(obj$contributions) = dimnames(A)
  obj$n_comps = n_comps
  obj$cardinality = colSums(A != 0)
  
  totv = sum(s_ee$val)
  
  vexp = make_vexp(A, S)
  obj$vexp = vexp$vexp/totv
  obj$vexp_pc = s_ee$val[1:obj$n_comps]/totv
  obj$cvexp = vexp$cvexp/totv
  obj$rvexp = vexp$vexp/s_ee$val[1:n_comps]
  obj$rcvexp = vexp$cvexp/cumsum(s_ee$val[seq_len(n_comps)])
  
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

  
# print.spca =======================
#' Prints the sparse loadings from an spca object
#' 
#' Prints sparse loadings omitting the zero ones and giving the cumulative
#' variance explained.
#' 
#' 
#' @param x An spca object.
#' @param cols A vector indicating which components should be printed.
#'    Default all. If an integer is passed, it is set to 1:cols.
#' @param only_nonzero  Logical: if = TRUE only the nonzero loadings are
#'  printed. Otherwise all loadings are printed.
#'  @param contributions Logical [TRUE]. If `TRUE`, print loadings 
#'  scaled to unit \eqn{L_1} norm as percentage contributions.
#' @param digits Integer: number of decimal figures.
#' @param thresh_card Value below which loadings are considered zero and not
#' printed.
#' @param return_table Logical: should the formatted (text) table be returned?
#' @param component_names A vector of names for the components. If NULL assigned 
#'   as#' "sPCj"
#' @param ... Further arguments; currently ignored.
#' @return If return_table = TRUE, it returns a formatted text table. 
#' so that only exact (or partial) prescribed arguments can be entered. 
#' @family spca
#' @export
#' @method print spca
print.spca = function(x, cols = NULL, only_nonzero = TRUE, contributions = TRUE, digits = 3, thresh_card = 1e-07, return_table = FALSE, component_names = NULL, ...)
  {
  
  # Validation
  dots = list(...)
  if (length(dots) > 0) {
    stop("Unused arguments: ", paste(names(dots), collapse = ", "))
  }
  #browser()
  fun_inp = as.list(match.call(expand.dots = FALSE))[-(1:2)]
  fun_inp = lapply(fun_inp, eval, envir = environment())
  validate_no_na(arg_list = eval(fun_inp))  
  
  test = validate_spca(x)
  if (!test)
    stop("print.spca requires an spca object as first argument")
  
  validate_booleans(
    only_nonzero = only_nonzero,
    contributions = contributions,
    return_table = return_table
  )
  
  if (contributions == TRUE){
      A = x$contributions
  } else  { A = x$loadings}
  
  if (is.null(cols))
    cols = 1:ncol(A)
  else
    if (length(cols) == 1L)
      cols = 1:cols
  
  if (only_nonzero){
    rows = apply(A, 1, function(x) all(abs(x) < thresh_card))
    A  = A[!rows, , drop = FALSE]
  }  
  
  A = A[, cols]
  
  ## assigns names to loadings
  if(is.null(component_names)){
    if(is.null(colnames(A)))
      component_names = paste0("sPC", ncol(A))
    else
      component_names = colnames(A)
  }
  else
    if ((length(component_names) < length(cols))){
      message("Too few component_names, default names assigned")
      component_names = paste0("sPC", ncol(A))
    }
  colnames(A) = component_names[cols]
  # # -----  formatting -
  
  # needed because sprintf reduces to vector but preserves trailing zeroes
  if (contributions == TRUE)
    fx = matrix(format(paste0(sprintf("%.1f", round(A * 100, 1)), "%")
                       ,drop0trailing = TRUE, justify = "right"), 
                ncol = length(cols))
  else
    fx = matrix(format(sprintf(paste0("%.", digits,"f"), 
                               round(A, digits)),drop0trailing = TRUE,
                       justify = "right"), ncol = length(cols))
  
  colnames(fx) = paste0("sPC", cols)
  rownames(fx) = rownames(A)
  
  nc = nchar(fx[1L], type = "c")
  fx[abs(A) < thresh_card] = paste(rep(" ", nc), collapse = "")
  
  #  ind = (abs(A)> thresh_card & abs(A) < thresh_card)
  #  fx[ind] = "--"
  fx = format(fx, justify = "right" )
  if (any(class(x) == "spca")){
    vexp = cumsum(x$vexp[cols])
    dashes = rep("-----", ifelse(is.null(ncol(fx)), 1, ncol(fx)))
    fx = rbind(fx, dashes, paste0(sprintf("%.1f", round(100*vexp,1)), "%"))
    rownames(fx)[nrow(fx) - 1] = ""     
    rownames(fx)[nrow(fx)] = "Cvexp"
  }  
  if (contributions == TRUE)
    message("Contributions (%)")
  else
    message("Loadings")
  if (ncol(A) == 1L){
    print(t(fx), quote = FALSE, right = TRUE)
  }
  else{
    print(fx, quote = FALSE, right = TRUE)
    cat(paste(" "))
  }
  if(return_table == TRUE){   
    return(fx)    
  }  
  else 
    invisible()
}

#' Change the sign of selected loadings in an `spca` object 
#' 
#' Multiplies by \eqn{-1} the loadings (including in loadings_list), contributions, 
#'   and, when present the scores and component-correlation entries 
#'   for the components listed in `index_to_change`.  
#' This is useful because (sparse) principal components are defined up to sign
#'  which could be different for Pcs and sPCS. 
#' @param spca_obj An object of class `spca`.  
#' @param index_to_change Integer vector of component indices whose sign should
#'   be flipped.  
#' @return The modified `spca_obj`, with the selected components sign-flipped.  
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
#' Shows the non-zero contributions separately for each component.
#' 
#' It just turns an spca object loadings_list into a list of loadings 
#' 
#' @param spca_obj An spca object
#' 
#' @param cols A vector containg the indices of the loadings to be shown.  Can
#'  be a single value. if missing all loadings are shown: If an integer is
#'  passed, only that dimension will be returned.
#' @param return_list Logical: if `TRUE` the list is returned
#' @family spca
#' @export 
show_contributions_spca = function(spca_obj, cols = NULL, print_list = TRUE, 
                                   return_list = FALSE)
{
  test = validate_spca(spca_obj)
  if (!test)
    stop("show_loadings requires an spca object as first argument")
#browser()  
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
#' Computes group-level sums by aggregating an input vector or the columns of an
#' input matrix/data.frame according to a grouping index (e.g., group membership
#' of variables).
#'
#' @param X An `spca` object or a numeric vector, or a numeric matrix/data.frame containing loadings
#'  or contributions. It is tolerated x as an spca object.
#' @param groups A vector or factor of length equal to `length(x)` 
#'   (if `x` is a vector) or `nrow(x)` (if `x` is a matrix/data.frame), 
#'   giving the group label for each variable.
#' @param only_nonzero Logical. If `TRUE`, groups with zero total (based on
#'   `rowSums(abs(out))`) are removed from the output.
#' @param contributions if TRUE the aggregated group contributions are returned
#'  as percentages.
#' @param digits If integer, the number of digits to which to round the
#'  aggregated values. If FALSE no rounding is done.  
#' @param print_table Logical [TRUE]. If `TRUE`, print the aggregated table.
#' @param return_table Logical [FALSE]. If `TRUE`, return the aggregated table.
#' 
#' @details
#' If loadings are passed and contributions == TRUE, contributions is turned to
#'  FALSE and digits to 3 expressing L2 unit loadings as percentages. 
#' The aggregated sums are likely not to respect the unit norm property of the
#'  individual loadings or contributions 
#' 
#' @return A numeric vector (if `X` is a vector) or numeric matrix  of sums
#'   aggregated by group. Rows correspond to groups
#'   and columns correspond to columns of `X`.
#'
#' @export
aggregate_by_group = function(X, groups, 
                              only_nonzero = TRUE, 
                              contributions = TRUE, 
                              digits = ifelse(contributions, 1, 3),
                              print_table = TRUE,
                              return_table = FALSE) 
{
  # validation ===========  
  
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
    if (((!is.vector(X) && (!is.factor(X))) ||  (!is.matrix(X))))
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
  #browser()
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


#  summary.spca==============
#' Prints summaries from an spca object
#' 
#' Prints and optionally returns a table with summary statistics for evaluation 
#' and comparisons with the full PCA solutions.
#' 
#' The summaries are printed as formatted text, if return_table = TRUE, the value
#' returned is a numerical matrix.
#' 
#' For each component the following summaries are computed: 
#' \tabular{ll}{ 
#' Vexp\tab The percentage variance explained.\cr
#' Cvexp \tab The percentage cumulative variance explained.\cr
#' Rvexp \tab The variance explained relative to the corresponding PC.\cr
#' Rcvexp \tab The cumulative variance explained relative to the 
#'  corresponding PCs.\cr
#' Card \tab The cardinality, that is the number of non zero loadings.\cr
# #'   min load/Min cont \tab Minimum absolute value of the non-zero 
# #'   loadings or contributions.\cr
# #' r\tab Correlation between sPCs and corresponding PCs.
#' }
#'
#' @param object An spca object.
#' @param cols A vector indicating which components should be included. 
#'   Default all. Can be any set of columns (e.g. c(1, 3)). 
#'   If an integer is passed, it is set to 1:cols.
#' @param contributions Logical: should the loadings be standardised to unit L1 norm
#' (and printed as percentage contributions)
#' @param variance_metrics Character vector: which variance metrics to include.
#' Options: "relative" (RVEXP), "cumulative_relative" (RCVEXP), "both", or
#'  "none". Default is "cumulative_relative".
#' @param min_load Optional: if `TRUE` the minimum loading or contributions 
#'   are printed.
#' @param cor_with_pc Optional, If `TRUE` correlations between sPCs and
#'   corresponding PCs are printed.
#' @param return_table Logical: If `TRUE` the raw summary matrix is returned.
#' @param print_table Logical: If `TRUE` the table is printed.
#' @param thresh_card [0.0001] Value below which loadings are treated as zero when computing minimum loadings or contributions.

#' @param ... Further arguments; currently ignored.
#' 
#' @return If return_table = TRUE, a numerical matrix with the summaries.
#' @seealso Examples in \code{\link{aggregate_by_group}}.
#' @family spca
#' @export
#' @method summary spca
summary.spca = function(
    object, 
    cols, 
    contributions = TRUE, 
    variance_metrics = c("both", "cumulative_relative",
                         "relative", "none"),
    min_load = FALSE, 
    cor_with_pc = FALSE,
    return_table = FALSE, 
    print_table = TRUE, 
    thresh_card = 1e-8, 
    ...) 
{
  # Validation
  dots = list(...)
  if (length(dots) > 0) {
    stop("Unused arguments: ", paste(names(dots), collapse = ", "))
  }
  
  test = validate_spca(object)
  if (!test)
    stop("summary.spca requires an spca object as first argument")
  
  # spca already validated
  fun_inp = as.list(match.call(expand.dots = FALSE))[-(1:2)]
  fun_inp = lapply(fun_inp, eval, envir = environment())
  validate_no_na(arg_list = eval(fun_inp))  
  
  # Determine columns
  if (missing(cols)) {
    cols = 1:min(ncol(object$loadings), length(object$vexp))
  } else 
    if (length(cols) == 1L) {
      cols = seq(cols)
    }
  if (any(cols) > ncol(object$loadings))
    stop("cols cannot contain values larger than the number of components available")
  
  validate_booleans(contributions = contributions, min_load = min_load,
                    cor_with_pc = cor_with_pc, return_table = return_table , print_table = print_table)
  
  if(is.vector(variance_metrics))
    variance_metrics = variance_metrics[1]
  
  match.arg(variance_metrics, choices = 
              c("relative", "cumulative_relative", "both", "none"))
  
  
  # build table =====================  
  # essential matrix
  out = rbind(
    Vexp = object$vexp,
    Cvexp = cumsum(object$vexp)
  )
  
  # Add variance comparison metrics based on user choice
  if (variance_metrics %in% c("cumulative_relative", "both")) {
    out = rbind(out, Rvexp = object$rvexp)
  }
  if (variance_metrics %in% c("cumulative_relative", "both")) {
    out = rbind(out, Rcvexp = object$rcvexp)
  }
  #browser()  
  # Add cardinality
  out = rbind(out, Card = get_card(object, thresh_card = thresh_card))
  
  # Add minimum loading/contributions if requested
  if (min_load) {
    if(contributions)
      min_vals = get_minload(object$contributions)
    else
      min_vals = get_minload(object$loadings)
    out = rbind(out, temp = min_vals)
    min_label = ifelse(contributions, "Min cont", "Min load")
    rownames(out)[nrow(out)] = min_label
  }
  
  if (cor_with_pc){
    if ((!is.null(object$cor_with_pc)))
      out = rbind(out, r = object$cor_with_pc)
    else
      warning("correlations with PCs are not available")
  }
  
  # Not all columns may be selected 
  out = as.matrix(out[, cols, drop = FALSE])
  colnames(out) = paste0("sPC", cols)
  
  if (print_table) {
    out_formatted = format_summary_matrix(out, contributions)
    print(out_formatted, quote = FALSE, right = TRUE)
  }
  
  # Return if requested
  if (return_table) {
    return(out)
  } else {
    invisible()
  }
  
}



# Format summary matrix for printing
# @keywords internal
format_summary_matrix = function(out, contributions) {
  
  # will be used for different formatting
  percentage_rows = c("Vexp", "Cvexp", "Rvexp", "Rcvexp", 
                      ifelse(contributions, "Min cont", "Min load"))
  integer_rows =  "Card"
  decimal_rows = c("min_load", "r")
  
  # chracter matrix fx will be filled with formatted 
  outp = matrix("", nrow = nrow(out), ncol = ncol(out))
  rownames(outp) = rownames(out)
  colnames(outp) = colnames(out)
  
  
  # easier to run if rows are missing   
  for (i in 1:nrow(out)) {
    row_name = rownames(out)[i]
    
    if (row_name %in% percentage_rows) {
      # Format as percentage with 1 decimal
      outp[i, ] =  format(paste0(sprintf("%.1f", round(out[i, ] * 100, 1)), 
                                 "%"), nsmall = 1, drop0trailing = FALSE,
                          justify = "right")
    } else if (row_name %in% integer_rows) {
      # Format as integer
      outp[i, ] = format(round(out[i, ]), drop0trailing = TRUE, 
                         justify = "right", trim = TRUE, nsmall = 0)
      
    } else if (row_name %in% decimal_rows) {
      # Format with 3 decimals (not percentage)
      outp[i, ] = format(round(out[i, ], 3), nsmall = 3, 
                         drop0trailing = FALSE, justify = "right")
    }
  }
  
  return(outp)
}
