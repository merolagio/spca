
#is.spca==============
#' #' Test for spca objects
#' 
#' @param x Any object suspected of being of class spca.
#' @param ... Unused.
#' 
#' @details The function carries out checks for the minimal components of an
#' \emph{spca} object. It checks the presence and mode of
#'  a matrix of loadings named \emph{loadings} 
#'  a vector of variance explained by the components named \emph{vexp} 
#'  a vector of variance explained by the PCss named \emph{vexpPC}
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
  !is.null(x$vexpPC) &&
  !is.null(x$cvexp) &&
  !is.null(x$rvexp) &&
  !is.null(x$rcvexp) &&
  !is.null(x$ncomps) &&
  !is.null(x$cardinality) &&
  !is.null(x$loadlist) &&
  !is.null(x$indices) 
}
# validate_spca==========================
#' Validate the existence and correctness of core spca elements
#'
#' Checks loadings first, then explained-variance summaries, component counts,
#' cardinality, loadlist, indices, and, if present, `r2` and `scores`.
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
    "loadings", "vexpPC", "vexp", "cvexp", "rvexp", "rcvexp", "ncomps",
    "cardinality", "loadlist", "indices", "corComp"
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
          "cannot be larger than the corresponding values of cumsum(vexpPC)"
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
    z = x$vexpPC,
    name = "vexpPC",
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
    upper_vec = if (!is.null(x$vexpPC) &&
                    is_num_vector(x$vexpPC) &&
                    (length(x$vexpPC) == ncomp) &&
                    !anyNA(x$vexpPC)) {
      cumsum(x$vexpPC)
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
  
  # validate_ncomps and cardinality ==========
  
  if (is.null(x$ncomps)) {
    add_msg("ncomps is missing")
  } else {
    if (!is_num_vector(x$ncomps) || (length(x$ncomps) != 1)) {
      add_msg("ncomps must be a scalar")
    } else {
      if (anyNA(x$ncomps)) {
        add_msg("ncomps cannot contain missing values")
      }
      
      if (abs(x$ncomps - round(x$ncomps)) > tol) {
        add_msg("ncomps must be an integer")
      }
      
      if (abs(x$ncomps - ncomp) > tol) {
        add_msg("ncomps must be equal to ncol(loadings)")
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
  
  if (is.null(x$loadlist)) {
    add_msg("loadlist is missing")
  } else {
    if (!is.list(x$loadlist)) {
      add_msg("loadlist must be a list")
    } else {
      if (length(x$loadlist) != ncomp) {
        add_msg("loadlist must have length ncol(loadings)")
      }
      
      for (i in seq_along(x$loadlist)) {
        xi = x$loadlist[[i]]
        nm = paste0("loadlist[[", i, "]]")
        
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
    if (is.null(x$corComp)) {
      add_msg("corComp is missing")
    } else {
      if (is.vector(x$corComp)) {
        add_msg("corComp must be a matrix, not a vector")
      } else if (!is.matrix(x$corComp)) {
        add_msg("corComp must be a matrix")
      } else if (!is.numeric(x$corComp)) {
        add_msg("corComp must be numeric")
      } else {
        if (any(dim(x$corComp) == 0)) {
          add_msg("corComp cannot have zero dimensions")
        }
        
        if (anyNA(x$corComp)) {
          add_msg("corComp cannot contain missing values")
        }
        
        if ((nrow(x$corComp) != ncomp) || (ncol(x$corComp) != ncomp)) {
          add_msg("corComp must have dimensions ncol(loadings) x ncol(loadings)")
        }
      }
    }
  }
  
  # validate_r2 ======================
  
  if (!is.null(x$r2)) {
    validate_num_vector(
      z = x$r2,
      name = "r2",
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
#' @param S the variance or correlation matrix from which the loadings where computed.
#' @param X real matrix, the data matrix from which the loadings where computed, optional
#' @param method_name string, the name of the method_name used to compute the loadings, optional.
#' @return An object is of class spca with several added components, 
#' FALSE otherwise.
#' @details The \code{spca} object can be used as argument for spca methods.
#'  It contains \describe{ 
#'  \item{loadings}{the matrix of loadings.}
#'  \item{contributions}{the matrix of loadings scaled to unit L1 norm.} 
#'  \item{ncomps}{the number of components.}
#'  \item{cardinality}{the vector with the number of nonzero loadings in 
#'  each each component.}
#'  \item{ind}{a list with the indices of the variables corresponding the nonzero loadings.}
#'  \item{vexp}{a vector of percent variance explained by the components.} 
#'  \item{cvexp}{a vector of percent cumulative variance explained by the components.}
#'  \item{vexpPC}{a vector of variance explained by the PCs.}
#'  \item{rvexp}{a vector of proportion of cumulative variance explained over that explained by the 
#'  corresponding PCs.}
#'  \item{loadlist}{a list with only the nonzero loadings.}
#'  \item{scores}{the components' scores, if X is passed.} 
#'  \item{corComp}{A matrix of the correlations between components.}
#'  \item{method_name}{the string passed as such, if method_name is passed.}
#' }
#' @family spca
#' @export 
new_spca = function(A, S, X = NULL, method_name = NULL){
  
  fun_inp = list(A = A, S = S, X = X, method_name = method_name)
  validate_no_na(arg_list = fun_inp)  
  
  # validation   ==============P
  
  # chcking an spca object requires too much stack
  fun_inp = as.list(match.call(expand.dots = FALSE))[-1]
  fun_inp = lapply(fun_inp, eval, envir = environment())
  validate_no_na(arg_list = eval(fun_inp))  
  
  if(is.data.frame(A)) 
    A = as.matrix(A)
  if (!is.matrix(A)){
    stop("A must be a matrix of loadings")
  }
  
  if(!isSymmetric(S))
    stop("S must be a symmetric covariance or correlation matrix")
  

  obj = list()
  obj$loadings = A
  obj$contributions = make_contributions(A)
  dimnames(obj$contributions) = dimnames(A)
  obj$ncomps = ncol(A)
  obj$cardinality = colSums(A != 0)
  
    
  
  tmp_vexp = make_vexpSC(obj$loadings, S)  
  s_ee = eigen(S)
  totv = sum(s_ee$values)
  obj$vexp = tmp_vexp$vexp/totv
  obj$vexpPC = s_ee$values[1:obj$ncomps]/totv
  obj$cvexp = tmp_vexp$cvexp/totv
  obj$rvexp = obj$vexp/obj$vexpPC
  obj$rcvexp = obj$cvexp/cumsum(obj$vexpPC)
  obj$cor_with_PC = var2corC(atdbC(A, S, s_ee$vectors[, seq_len(obj$ncomps)]))
  obj$total_variance = totv
  
  obj$indices = list()
  obj$loadlist = list()
  for(i in 1:obj$ncomps){
    obj$indices[[i]] = which(A[, i] != 0)
    obj$loadlist[[i]] = A[obj$indices[[i]], i]
      names(obj$indices[[i]]) = rownames(A)[obj$indices[[i]]]
    names(obj$loadlist[[i]]) = rownames(A)[obj$indices[[i]]]
  }
  
  if(!is.null(X)){
    obj$scores = abC(X, A)
    obj$corComp = makeCorScoresC(obj$scores)
  }
  else{
    obj$corComp = make_corComp_S(A, S)
  }
    
  if(!is.null(method_name))  
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
#' @param only.nonzero  Logical: if = TRUE only the nonzero loadings are
#'  printed. Otherwise all loadings are printed.
#' @param perc Logical: should the loadings be standardised to unit
#'  \eqn{L_1} norm (and printed as percentage contributions)?
#' @param digits Integer: number of decimal figures.
#' @param thresh Value below which loadings are considered zero and not
#' printed.
#' @param rtn Logical: should the formatted (text) table be returned?
#' @param components_names A vector of names for the components. If NULL assigned 
#'   as#' "sPCj"
#' @param ... Further arguments; currently ignored.
#' @return If rtn = TRUE, it returns a formatted text table. 
#' so that only exact (or partial) prescribed arguments can be entered. 
#' @family spca
#' @export
#' @method print spca
print.spca = function(x, cols = NULL, only.nonzero = TRUE, contributions = TRUE, digits = 3, thresh = 1e-03, rtn = FALSE, components_names = NULL, ...)
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
    stop("plot.spca requires an spca object as first argument")
  
  validate_booleans(
    only.nonzero = only.nonzero,
    contributions = contributions,
    rtn = rtn
  )
  
  if (contributions == TRUE){
      A = x$contributions
  } else  { A = x$loadings}
  
  if (is.null(cols))
    cols = 1:ncol(A)
  else
    if (length(cols) == 1L)
      cols = 1:cols
  
  if (only.nonzero){
    rows = apply(A, 1, function(x) all(abs(x) < thresh))
    A  = A[!rows, , drop = FALSE]
  }  
  
  ## assigns names to loadings
  if ((!is.null(components_names)) && (length(components_names) >= length(cols))){
    colnames(A) = components_names[cols]
  }
  else{
    if (!(is.null(components_names)) && (length(components_names) < length(cols)))
      message("the length of components_names is incorrect, automatic names assigned")
    colnames(A) = paste0("sPC", cols)
  }
  
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
  fx[abs(A) < thresh] = paste(rep(" ", nc), collapse = "")
  
  #  ind = (abs(A)> thresh & abs(A) < thresh)
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
  if(rtn == TRUE){   
    return(fx)    
  }  
  else 
    invisible()
}

#' Change the sign of selected loadings in an `spca` object 
#' 
#' Multiplies by \eqn{-1} the loadings (including in loadlist), contributions, 
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
  if (!is.vector(index_to_change))
    stop(paste("index_to_change must be an integer or vector of indices",
               index_to_change, "was passed"))
    n = length(index_to_change)
  
  for (i in 1:n) {
    spca_obj$loadings[, index_to_change[i]] = 
      - spca_obj$loadings[, index_to_change[i]]
    
    spca_obj$contributions[, index_to_change[i]] = 
      - spca_obj$contributions[, index_to_change[i]]
    
    if (!is.null(spca_obj$loadingslist)) {
      spca_obj$loadingslist[[index_to_change[i]]] =
        - spca_obj$loadingslist[[index_to_change[i]]]
    }
    if (!is.null(spca_obj$scores))
      spca_obj$scores[, index_to_change[i]] =
      - spca_obj$scores[,index_to_change[i]]
    
    if (!is.null(spca_obj$corComp)) {
      spca_obj$corComp[index_to_change[i], ] =
        - spca_obj$corComp[index_to_change[i], ]
      spca_obj$corComp[, index_to_change[i]] = 
        - spca_obj$corComp[, index_to_change[i]]
    }
  }
  return(spca_obj)
}


## show_contributions_spca ==================
#' Shows the non-zero contributions separately for each component.
#' 
#' It just turns an spca object loadlist into a list of loadings 
#' 
#' @param spca_obj An spca object
#' 
#' @param cols A vector containg the indices of the loadings to be shown.  Can
#'  be a single value. if missing all loadings are shown: If an integer is
#'  passed, only that dimension will be returned.
#' @param return_list Logical: if `TRUE` the list is returned
#' @family spca
#' @export 
show_contributions_spca = function(spca_obj, cols = NULL, return_list = FALSE)
{
  test = validate_spca(spca_obj)
  if (!test)
    stop("show_loadings requires an spca object as first argument")
  
  if(is.null(cols)){
    cols = seq_along(spca_obj$vexp)
  }
  
  message("Percentage Contributions")
  
  print(lapply(spca_obj$loadlist, function(x) x/ sum(abs(x))))
  
  
  if (return_list == TRUE)
    return(loads)
  
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
#' @param contributions if TRUE the aggregated group contributions are returned as
#'  percentages.
#' @param digits If integer, the number of digits to which to round the
#'  aggregated values. If FALSE no rounding is done.  
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
    if (any(abs(colSums(abs(X)) - 1) > 10e-3) && (contributions == T)){
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
      outp = out
      for (i in 1:nrow(out)){
        outp[i, ] =  format(paste0(sprintf("%.1f", round(out[i, ]*100, 1)), "%"),
                            nsmall = 1, drop0trailing = FALSE, justify = "right")
      }
      
      #trim = TRUE,
      print(outp, quote = F, right = TRUE)
    } else {
      print("loadings")
      print(out, digits = 3)}
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
#' The summaries are printed as formatted text, if rtn = TRUE, the value
#' returned is a numerical matrix.
#' 
#' For each component the following summaries are computed: 
#' \tabular{ll}{ 
#' Vexp\tab The percentage variance explained.\cr
#' Cvexp \tab The percentage cumulative variance explained.\cr
#' Rvexp \tab The variance explained relative to the corresponding PC.\cr
#' Rcvexp \tab The cumulative variance explained relative to the 
#' corresponding PCs.\cr
#' Card \tab The cardinality, that is the number of non zero loadings.\cr
# #'   min load/Min cont \tab Minimum absolute value of the non-zero 
# #'   loadings or contributions.\cr
# #' R2\tab Squared correlation between SPC and corresponding PC.
#' }
#'
#' @param x An spca object.
#' @param cols A vector indicating which components should be included. 
#'   Default all. Can be any set of columns (e.g. c(1, 3)). 
#'   If an integer is passed, it is set to 1:cols.
#' @param contributions Logical: should the loadings be standardised to unit L1 norm
#' (and printed as percentage contributions)
#' @param variance_metrics Character vector: which variance metrics to include.
#' Options: "relative" (RVEXP), "cumulative_relative" (RCVEXP), "both", or "none".
#' Default is "cumulative_relative".
#' @param min_load Logical: should minimum loading or contributions be included?
#' @param rtn Logical: should the raw summary matrix be returned?
#' @param prn Logical: should the table be printed?
#' @param thrsehhold_cardinality Value below which \emph{loadings} 
#'   are considered to be zero.  Default 0.001. 
#' @param ... Further arguments; currently ignored.
#' 
#' @return If rtn = TRUE, a numerical matrix with the summaries.
#' @seealso Examples in \code{\link{aggregate_by_group}}.
#' @family spca
#' @export
#' @method summary spca
summary.spca = function(
    x, 
    cols, 
    contributions = TRUE, 
    variance_metrics = c("both", "cumulative_relative",
                                              "relative", "none"),
    min_load = FALSE, 
    r_squared = FALSE,
    rtn = FALSE, 
    prn = TRUE, 
    thresh = 1e-4, 
    ...) 
{
  # Validation
  dots = list(...)
  if (length(dots) > 0) {
    stop("Unused arguments: ", paste(names(dots), collapse = ", "))
  }
  
  test = validate_spca(x)
  if (!test)
    stop("plot.spca requires an spca object as first argument")
  
  # spca already validated
  fun_inp = as.list(match.call(expand.dots = FALSE))[-(1:2)]
  fun_inp = lapply(fun_inp, eval, envir = environment())
  validate_no_na(arg_list = eval(fun_inp))  
  
  # Determine columns
  if (missing(cols)) {
    cols = 1:min(ncol(x$loadings), length(x$vexp))
  } else 
    if (length(cols) == 1L) {
      cols = seq(cols)
    }
  if (any(cols) > ncol(x$loadings))
    stop("cols cannot contain values larger than the number of components available")
  
  validate_booleans(contributions = contributions, min_load = min_load,
                    r_squared = r_squared, rtn = rtn , prn = prn)
  
  if(is.vector(variance_metrics))
    variance_metrics = variance_metrics[1]
  
  match.arg(variance_metrics, choices = 
              c("relative", "cumulative_relative", "both", "none"))
  
  
  # build table =====================  
  # essential matrix
  out = rbind(
    Vexp = x$vexp,
    Cvexp = cumsum(x$vexp)
  )
  
  # Add variance comparison metrics based on user choice
  if (variance_metrics %in% c("cumulative_relative", "both")) {
    out = rbind(out, Rvexp = x$rvexp)
  }
  if (variance_metrics %in% c("cumulative_relative", "both")) {
    out = rbind(out, Rcvexp = x$rcvexp)
  }
  #browser()  
  # Add cardinality
  out = rbind(out, Card = get_card(x, thresh = thresh))
  
  # Add minimum loading/contributions if requested
  if (min_load) {
    if(contributions)
      min_vals = get_minload(x$contributions)
    else
      min_vals = get_minload(x$loadings)
    out = rbind(out, temp = min_vals)
    min_label = ifelse(contributions, "Min cont", "Min load")
    rownames(out)[nrow(out)] = min_label
  }
  
  if (isTRUE(r_squared)){
    if ((!is.null(x$r2)))
      out = rbind(out, R2 = x$cor_with_PC)
    else
      warning("R squared coefficients are not available")
  }
  
  # Not all columns may be selected 
  out = as.matrix(out[, cols, drop = FALSE])
  colnames(out) = paste0("sPC", cols)
  
  if (prn) {
    out_formatted = format_summary_matrix(out, contributions)
    print(out_formatted, quote = FALSE, right = TRUE)
  }
  
  # Return if requested
  if (rtn) {
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
  decimal_rows = c("min_load", "R2")
  
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
