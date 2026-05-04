

#' fails if any arguemnt is NA
#' ... are standard input, args takes a list, typically
#' one or the other, args takes precedence
#' @noRd
validate_no_na = function(..., arg_list = NULL) {
  
  if (is.null(arg_list)) {
    args = list(...)
  }
  else 
    args = arg_list
#browser()
  bad = vapply(args, anyNA, logical(1))
  
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


#' needed because is.logical returns TRUE also for NAs wtf
#' is.logical(NA) returns TRUE
#' is.boolean(NA) returns FALSE
#' @noRd
is.boolean = function(x) {
  isTRUE(x) || isFALSE(x)
}


#' fails if any argument is not boolean
#' @param ... are standard input, 
#' @param args takes a list, typically one or the other, args takes precedence
#' 
#' @details A warning is issued upon failure.
#' 
#' @returns TRUE or FALSE.
#' @noRd
validate_booleans = function(..., arg_list = NULL) {

  if(is.null(arg_list))
    args = list(...)
  else
    args = arg_list
  bad = !vapply(args, is.boolean, logical(1))
  
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


#' fails if x is not a vector of indices with maximum = max_val 
#' @param x a vector of positive integers
#' @param maxval the value beyond wich the indices are invalid.
#' @details A warning is issued upon failure.
#' 
#' @returns TRUE or FALSE.
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
