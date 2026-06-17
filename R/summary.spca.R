

#  summary.spca==============
#' Summarize an spca object
#'
#' Print and optionally return summary statistics for evaluating an \code{spca}
#' object and comparing it with the corresponding PCA solution.
#'
#' For each component, the following summaries can be computed:
#' \tabular{ll}{ 
#' Vexp\tab The percentage variance explained.\cr
#' Cvexp \tab The percentage cumulative variance explained.\cr
#' Rvexp \tab The variance explained relative to the corresponding PC.\cr
#' Rcvexp \tab The cumulative variance explained relative to the 
#'  corresponding PCs.\cr
#' Card \tab The cardinality, that is the number of non zero loadings.\cr
#' Min load/Min cont \tab The minimum absolute value of the nonzero loadings
#' or contributions, if requested.\cr
#' r \tab The correlation between sPCs and the corresponding PCs, if
#' requested.\cr
#' }
#'
#' @param object An object of class \code{spca}.
#' @param cols An integer vector of component indices. If missing, all
#'   available components are included. If a single integer is supplied,
#'   components \code{1:cols} are included.
#' @param contributions A logical value (default \code{TRUE}). If \code{TRUE},
#'   minimum nonzero values are computed from percentage contributions;
#'   otherwise, they are computed from loadings.
#' @param variance_metrics A character vector (default first element
#'   \code{"both"}). Controls which relative variance metrics are included.
#'   Accepted values are \code{"relative"}, \code{"cumulative_relative"},
#'   \code{"both"}, and \code{"none"}.
#' @param min_load A logical value (default \code{FALSE}). If \code{TRUE},
#'   include the minimum nonzero loading or contribution.
#' @param cor_with_pc A logical value (default \code{FALSE}). If \code{TRUE},
#'   include correlations between sPCs and the corresponding PCs when available.
#' @param return_table A logical value (default \code{FALSE}). If \code{TRUE},
#'   return the raw numeric summary matrix.
#' @param print_table A logical value (default \code{TRUE}). If \code{TRUE},
#'   print the formatted summary table.
#' @param thresh_card A numeric scalar (default \code{1e-8}). Values with
#'   absolute magnitude at or below this threshold are treated as zero when
#'   computing cardinality.
#' @param ... Further arguments. These are currently unused and trigger an error
#'   if supplied.
#'
#' @return If \code{return_table = TRUE}, returns a numeric matrix with the
#' selected summary statistics. Otherwise, returns \code{NULL} invisibly.
#' @seealso Examples in \code{\link{aggregate_by_group}}.
#' 
#' @examples
#' data(holzinger)
#' ho_cspca = spca(holzinger, n_comps = 2)
#' summary(ho_spca)
#' 
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
    stop("cols cannot contain values larger than the number of components
         available")
  
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
