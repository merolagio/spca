# print.spca =======================
#' Print the Weights or Contributions of a \code{pca} or \code{spca} Object
#'
#' Print sparse weights, or the corresponding percentage contributions, from a
#' fitted object. By default, variables with only zero entries are omitted,
#' and cumulative explained variance is shown at the bottom of the table.
#'
#' @param x An object of class \code{spca} or \code{pca}.
#' @param cols An integer vector or \code{NULL} (default \code{NULL}).
#'   Components to print. If \code{NULL}, all components are printed. If a
#'   single integer is supplied, components \code{1:cols} are printed.
#' @param only_nonzero A logical value (default \code{TRUE}). If \code{TRUE},
#'   print only variables with at least one weight or contribution whose
#'   absolute value is greater than or equal to \code{thresh_card}.
#' @param contributions A logical value (default \code{TRUE}). If \code{TRUE},
#'   print weights scaled to unit \eqn{L_1} norm as percentage contributions;
#'   otherwise, print L2 unit weights.
#' @param digits An integer scalar (default \code{3}). Number of decimal places
#'   used when printing weights. Contributions are printed as percentages with
#'   one decimal place.
#' @param thresh_card A numeric scalar (default \code{1e-07}). Values with
#'   absolute magnitude below this threshold are treated as zero in the printed
#'   table.
#' @param return_table A logical value (default \code{FALSE}). If \code{TRUE},
#'   return the formatted character matrix.
#' @param component_names A character vector or \code{NULL} (default
#'   \code{NULL}). Optional component names. If \code{NULL}, existing column
#'   names are used when available; otherwise default names are assigned.
#' @param ... Further arguments. These are currently unused and trigger an error
#'   if supplied.
#'
#' @return If \code{return_table = TRUE}, returns the formatted character
#' matrix. Otherwise, returns \code{NULL} invisibly.
#' 
#' @examples
#' data(holzinger)
#' ho_cspca = spca(holzinger, n_comps = 4)
#' ho_cspca
#' print(ho_cspca, contributions = FALSE, digits = 4)
#' 
#' @family spca
#' @family pca
#' @exportS3Method 
print.spca = function(x, cols = NULL, only_nonzero = TRUE, contributions = TRUE, digits = 3, thresh_card = 1e-07, return_table = FALSE, component_names = NULL, ...)
{
  
  # Validation
  dots = list(...)
  if (length(dots) > 0) {
    stop("Unused arguments: ", paste(names(dots), collapse = ", "))
  }
  
  validate_no_na(arg_list = list(
    cols = cols,
    only_nonzero = only_nonzero,
    contributions = contributions,
    digits = digits,
    thresh_card = thresh_card,
    return_table = return_table,
    component_names = component_names
  ))
  
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
  } else  { A = .get_spca_weights(x)}
  
  if (is.null(cols))
    cols = 1:ncol(A)
  else
    if (length(cols) == 1L)
      cols = 1:cols
  
  if (!all(cols %in% seq_len(ncol(A))))
    stop("One of the indices provided in cols is too large or too small")
    
  if (only_nonzero){
    rows = apply(A[, cols, drop = FALSE], 1, 
                 function(x) all(abs(x) < thresh_card))
    A  = A[!rows, , drop = FALSE]
  }  
  
  A = A[, cols, drop = FALSE]
  
  ## assigns names to weights
  if(is.null(component_names)){
    if(is.null(colnames(A)))
      component_names = paste0(ifelse(is.pca(x), "PC", "sPC"), cols)
    else
      component_names = colnames(A)
  }
  else
  if (length(component_names) != ncol(A)) {
    message("Incorrect number of component_names, default names assigned")
    component_names = paste0(ifelse(is.pca(x), "PC", "sPC"), cols)
  }
  
  colnames(A) = component_names
  
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
  
  colnames(fx) = component_names
  rownames(fx) = rownames(A)
  
  nc = nchar(fx[1L], type = "c")
  fx[abs(A) < thresh_card] = paste(rep(" ", nc), collapse = "")
  
  fx = format(fx, justify = "right" )
  vexp = cumsum(x$vexp)[cols]
  dashes = rep("-----", ifelse(is.null(ncol(fx)), 1, ncol(fx)))
  fx = rbind(fx, dashes, paste0(sprintf("%.1f", round(100*vexp,1)), "%"))
  rownames(fx)[nrow(fx) - 1] = ""     
  rownames(fx)[nrow(fx)] = "Cvexp"
  
  if (contributions == TRUE)
    message("Percentage contributions")
  else
    message("Weights")
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
