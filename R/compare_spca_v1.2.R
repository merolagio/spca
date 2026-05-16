#' Compare two or more spca solutions
#' 
#' Plot loadings and print summary statistics for several \code{spca} objects
#' side by side. For the meaning of each summary statistic see
#' \code{\link{summary.spca}}. Tables and plots can optionally be returned.
#'
#' @param obj_list A list of two or more \code{spca} objects.
#' @param n_comps Integer scalar. Number of components to compare. If
#'   \code{NULL}, the minimum number of available components across objects is
#'   used.
#' @param contributions Logical. If \code{TRUE}, compare percentage
#'   contributions; otherwise compare loadings.
#' @param only_nonzero Logical. If \code{TRUE}, only variables with at least one
#'   nonzero loading/contribution are plotted or printed.
#' @param variable_groups Optional variable grouping. Can be a list of indices,
#'   a vector, or a factor with one entry per variable. Used to draw vertical
#'   group-separating lines in the loadings plot.
#' @param plot_loadings Logical. If \code{TRUE}, plot the loadings or
#'   contributions.
#' @param plot_type Character scalar. Values starting with \code{"b"} use bars;
#'   values starting with \code{"p"} use points. Other values default to bars.
#' @param methods_names Optional character vector with one label per object. If
#'   \code{NULL}, labels are \code{M1}, ..., \code{Mk}.
#' @param col_grouplines Character scalar. Color of the vertical group lines.
#' @param color_scale Character scalar. Color palette for bar plots. Accepted
#'   values are \code{"ggplot"}, \code{"cbb"}, \code{"printsafe"}, and
#'   \code{"bw"}.
#' @param equal_sign Logical. If \code{TRUE}, adjust loading signs to agree with
#'   the first object. See Details.
#' @param col_short_names Logical. If \code{TRUE}, use short component names
#'   such as \code{C1.M1}; otherwise use \code{C1.object_name}.
#' @param print_loadings Logical. If \code{TRUE}, print the loadings or
#'   contributions table.
#' @param return_tables Logical. If \code{TRUE}, return the loadings/contributions
#'   matrix and the raw summary matrix.
#' @param print_tables Logical. If \code{FALSE}, suppress table printing. Takes
#'   priority over \code{print_loadings}.
#' @param return_plot Logical. If \code{TRUE}, return the loadings plot.
#' @param show_plot Logical. If \code{TRUE}, print the loadings plot.
#' @details When \code{equal_sign = TRUE}, signs are aligned using score
#'   correlations if scores are available for all objects. Otherwise signs are
#'   aligned using correlations between the loading matrices.
#' @return Invisibly returns \code{NULL} by default. If \code{return_tables = TRUE},
#'   returns a list containing the comparison matrix and summary matrix. If
#'   \code{return_plot = TRUE}, the returned object also includes the loadings plot.
#' @seealso Examples in \code{\link{plot.spca}}.
#' @family spca
#' @export
compare_spca = function(
    obj_list,
    n_comps = NULL,
    contributions = TRUE, # change to contributions if safe 
    only_nonzero = TRUE,
    variable_groups = NULL,
    plot_loadings = TRUE,
    plot_type = c("bars", "points"),
    methods_names = NULL,
    col_grouplines = "red",
    color_scale = c("ggplot", "cbb", "printsafe", "bw"), 
    equal_sign = FALSE,
    col_short_names = TRUE,
    print_loadings = TRUE,
    return_tables = FALSE,
    print_tables = TRUE,
    return_plot = FALSE,
    show_plot = TRUE) {
  tryCatch({
  ## -------------------------------------------------------#
  ## obj_list two or more spca objects
  ##==============================================================#

  logical_args = list(
    contributions = contributions,
    only_nonzero = only_nonzero,
    plot_loadings = plot_loadings,
    equal_sign = equal_sign,
    col_short_names = col_short_names,
    print_loadings = print_loadings,
    return_tables = return_tables,
    print_tables = print_tables,
    return_plot = return_plot,
    show_plot = show_plot
  )
  logical_ok = vapply(logical_args, is.boolean, logical(1))
  if(!all(logical_ok))
    stop(paste(names(logical_ok)[!logical_ok],
               "must be TRUE or FALSE", collapse = "; "))
  
  if(!is.list(obj_list))
    stop("obj_list must be a list")
  if(length(obj_list) < 2)
    stop("obj_list must contain at least 2 spca objects")
  are_spca = sapply(obj_list, is.spca)
  if(!all(are_spca))
    stop(paste("Object number", which(!are_spca), "is not an spca object"))
    
  are_valid_spca = sapply(obj_list, validate_spca)
  if(!all(are_valid_spca))
    stop(paste("Object number", which(!are_valid_spca), "is not a valid spca object"))


  p = nrow(obj_list[[1]]$loadings)
  
# check if spca solutions are compatible
  nrow_all = sapply(obj_list, function(a) nrow(a$loadings))
  if(any(nrow_all != p))
    stop(paste("objects", which(nrow_all != nrow_all[1]) , "was built with a different dataset than the first"))
  

# n_objects is number of spca objects
  n_objects = length(obj_list)
  
  if(!is.null(methods_names)){
    if(!is.character(methods_names) || anyNA(methods_names))
      stop("methods_names must be NULL or a character vector without missing values")
    if(length(methods_names) != n_objects)
      stop("methods_names must have one name per spca object")
  }
  
  if(!is.character(plot_type) || length(plot_type) < 1 || anyNA(plot_type))
    stop("plot_type must be a non-missing character vector")
  if(!is.character(color_scale) || length(color_scale) < 1 || anyNA(color_scale))
    stop("color_scale must be a non-missing character vector")
  if(!is.character(col_grouplines) || length(col_grouplines) != 1 || anyNA(col_grouplines))
    stop("col_grouplines must be a non-missing character scalar")
  
# if n_comps not specified sets equal to minimum dim
  max_n_comps = min(sapply(obj_list,function(x) length(x$vexp_pc)))
  if (is.null(n_comps)){
    n_comps = max_n_comps
  }
  else{
    if(!is.numeric(n_comps) || length(n_comps) != 1 || is.na(n_comps))
      stop("n_comps must be NULL or a non-missing numeric scalar")
    if(n_comps < 1 || n_comps > max_n_comps)
      stop(paste("n_comps must be between 1 and", max_n_comps))
    n_comps = as.integer(n_comps)
  }
  
  if (print_tables == FALSE){
    print_loadings = FALSE
  }
  
## A is list of loadings of all objects--------------
  if(contributions)
    A = lapply(obj_list, function(x) x$contributions[, 1:n_comps])
  else
    A = lapply(obj_list, function(x) x$loadings[, 1:n_comps])
  
  ## needed to make loadings of similar sign, if required 
  #if scores not available acts on loadings directly
  
  
  if (equal_sign == TRUE)
    A = make_samesign(obj_list, A, n_comps = n_comps, quiet = FALSE)
  
  ##methods names  NEEDS FIX this ==========================
#browser() 
  if(is.null(methods_names)){
    methods_names = c(paste0("M", 1:n_objects))  
  }

  
# graphic parameters  
  pchlist = c(15:20,7:14)
  
# n_plot is number of loadings to plot

  if (isTRUE(plot_loadings)){
    n_plot = n_comps
  facets_nrows = ceiling(n_plot/3)
  facets_ncols = ceiling(n_plot/facets_nrows)
  if(grepl("^p", plot_type[1]))
    plot_type = "points"
   else
     if(grepl("^b", plot_type[1]))
       plot_type = "bars"
     else{
      warning(paste0("plot_type must be points or bars", plot_type, "was passed
                     .\nset to bars"))
      plot_type = "bars"
   }
  
  }
## plot laodings ==================

#browser()
    
# identify nonzero loadings 
    if(only_nonzero == TRUE){
      zeros = sapply(A, function(x) apply(x, 1, function(y) any(y != 0)))
      ind_nonzero = apply(zeros, 1, any)
      }    
      
    if (!is.null(rownames(obj_list[[1]]$loadings)))
      variables_name = rownames(obj_list[[1]]$loadings)
    else 
      variables_name = paste0("V", 1:p)

  if(plot_loadings){
#helpers in plot.spca    
    color_scale = spca_color_scale(color_scale)
    color_pal = spca_fill_palette(color_scale, pc_loadings = NULL)
 #browser()   
    loadings_vec = c(sapply(A, function(L, np) c(L[,1:(np)]),
                      np = n_plot))
    if(!is.null(variable_groups)){
      if (is.list(variable_groups)) variable_groups = list2fac(variable_groups)
      if (is.vector(variable_groups)) variable_groups = vec2fac(variable_groups)
      if(length(variable_groups) != p)
        stop("variable_groups must have one entry per variable")
    }
    
    df = data.frame(
      loadings = loadings_vec, 
      variable = factor(rep(1:p, n_plot * n_objects), labels = variables_name), 
      method = factor(rep(1:n_objects, each = p * n_plot), 
                      labels = methods_names), 
      Comp = factor(rep(rep(1:n_plot, each = p), n_objects), 
                    labels = paste("sPC", 1:n_plot))
      )
    
    if(only_nonzero){
      df = droplevels(df[rep(ind_nonzero, n_plot*n_objects), ])
      df$loadings[df$loadings == 0] = NA
    }
      ra = range(na.omit(loadings_vec))*(1.05)
      #ra[1] = min(0, ra[1])
      di = diff(ra)/5
      yLabels = seq(ra[1], ra[2], di)
      plab = pretty(yLabels)
      
#plot skeleton      
      pl = ggplot(
        data = df, aes(x = variable, y = loadings, label = variable)) + 
        facet_wrap(facets = vars(Comp), nrow = facets_nrows, ncol = facets_ncols, 
                   scales = "free_y") + theme_classic() + 
        ggplot2::theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
          panel.background = element_rect(fill = NA, colour = "black", 
                                      linewidth = 1, linetype = 1),
          legend.position = "bottom", 
          panel.grid.major.y = element_line(color = "grey85")) + 
        geom_abline(intercept = 0, slope = 0) +
# chck why capitalized        
#        ggplot2::scale_colour_manual(values = c("black", "red", "blue")) +
        ggplot2::xlab("variables") + 
        ggplot2::ylab(ifelse(contributions == TRUE, "contributions", "loadings"))
#browser()     
      if (plot_type == "points"){
        pl = pl + aes(colour = method, shape = method) + 
          geom_point(size = 2, alpha = 0.75) 
      
      } #end plot points
      else{#plots bars
#browser()
        pl = pl + aes(fill = method, group = method) + 
          ggplot2::geom_col(position = "dodge", color = 
                     ifelse(color_scale == "printsafe", "black", NA)
                   )
        
        if((!is.null(color_scale[1])) && (color_scale[1] != "ggplot"))
          pl = pl + scale_fill_manual(values = color_pal)
      }#end plot_loadings   
      
# adds vertical lines to separate groups----------------------      
    if (!is.null(variable_groups)){
      if(only_nonzero)
        variable_groups = droplevels(variable_groups[ind_nonzero])
      groups_table = table(variable_groups)
      cuts = cumsum(groups_table)[-length(groups_table)] + 0.5 
      pl = pl + geom_vline(xintercept = cuts, linewidth = 1,
                           colour = col_grouplines
                           )
      }#end variable_groups
      
    if (contributions == TRUE){
      pl = pl + scale_y_continuous(
        labels = function(x)  paste0(round(x*100,1),"%")
        ) 
      }
      if(show_plot)
        hw = withCallingHandlers(
          {
            plot(pl)
          },
          warning = function(w) {
            if (grepl("Removed .*", conditionMessage(w))) {
              invokeRestart("muffleWarning")
            }
          }
        )
        
##  end plot was } ==================  #
  }#end plot_loadings  
 

#tables to return ===========
##loadings_matrix======== 
# loadings of each object, grouped by their rank  
  
  loadings_matrix = matrix(0, nrow = p, ncol = n_objects *n_comps) 
  if (col_short_names == FALSE)
    col_names = paste(rep(paste0("C",1:n_comps), each = n_objects), 
                  rep(methods_names, times = n_comps), sep = "."
                  )
  else
    col_names = paste(rep(paste0("C",1:n_comps), each = n_objects), 
                  paste0("M", rep(1:n_objects, times = n_comps)), sep = "."
                  ) 
  k = 0
  for (i in 1:n_comps){
    for (j in 1:n_objects){
      k = k + 1
      loadings_matrix[,k] = A[[j]][,i]
    }
  } # 
  
  colnames(loadings_matrix) = col_names
  rownames(loadings_matrix) = variables_name
  
  ## summary table ---------------
  ## sum_list is list of summaries for all objects
#browser()
  sum_list = lapply(obj_list, summary, print_table = FALSE, return_table = TRUE, 
                    min_load = TRUE)
  
  sum_matrix = matrix(0, nrow = nrow(sum_list[[1]]), ncol = n_objects *n_comps) 
  k = 0
  for (i in 1:n_comps){
    for (j in 1:n_objects){
      k = k +1
      sum_matrix[,k] =  sum_list[[j]][,i]
    }  
    }
  colnames(sum_matrix) = col_names
  rownames(sum_matrix) = rownames(sum_list[[1]])  
  n_summat = nrow(sum_matrix)
#browser()

  rownames(sum_matrix)[n_summat] = ifelse(contributions, "Min cont", "Min load")

## printing----------------------------  
  if(print_tables == TRUE){
    if (print_loadings){
      if (only_nonzero == TRUE)
        which_rows = which(ind_nonzero)
      else
        which_rows = 1:p
#browser()      
      loadings_matrix_fmt = 
        format_loadings(loadings_matrix, cols = 1:(n_comps*n_objects), 
                        namescomp = col_names, contributions = contributions, 
                        rows = which_rows
                        )
      if (contributions == TRUE)
        print("Percentage Contributions")
      else
        print("Loadings")
      print(loadings_matrix_fmt, quote = FALSE)
      writeLines(" ")
    }
    sum_matrix_fmt = format_summaries(sum_matrix, contributions)
    print(paste("Summary statistics"), quote = FALSE)    
    print(sum_matrix_fmt, quote = FALSE, justify = "right")
  }  
    ## out list------------------------  
    if(return_tables){
      out = list(loadings = loadings_matrix, summary = sum_matrix)
      if(contributions) names(out)[1] = "contributions"
    
      if(return_plot)
        out$loadings_plot = pl
      return(out)  
    } else{
      if(return_plot)
        return(pl)
      else
        invisible()
    }
  }, error = function(e) {
    stop(paste("compare_spca:", conditionMessage(e)), call. = FALSE)
  })
}

#' changes the sign of the loadings
#' @noRd
make_samesign = function(obj_list = NULL, loadings_list, n_comps = NULL, quiet = FALSE){
  # browser() 
  if(missing(loadings_list))
    stop("loadings_list is required")
  if(!is.list(loadings_list))
    stop("loadings_list must be a list of matrices")
  if(is.null(n_comps))
    n_comps = min(sapply(loadings_list, ncol))
  
  if(!is.null(obj_list)) {
    if (all(sapply(obj_list, function(x) !is.null(x$scores)))){
      for (i in 2:length(obj_list)){
        co = diag(atbC(obj_list[[1]]$scores[, 1:n_comps], 
                       obj_list[[i]]$scores[, 1:n_comps]))
        if(any(co < 0)){
          loadings_list[[i]] = t(t(loadings_list[[i]]) * sign(co))
          if(!quiet)
            print(paste("changed sign to loadings", which(co < 0), "in object", i))
        }
      }
      return(loadings_list)
    }
  }
  for (i in 2:length(loadings_list)){
    co = diag(atbC(loadings_list[[1]], loadings_list[[i]]))
    if(any(co < 0)){
      loadings_list[[i]] = t(t(loadings_list[[i]]) * sign(co))
      if(!quiet)
        print(paste("changed sign to loadings", which(co < 0), "in object", i))      }
  }
  return(loadings_list)
}

#' internal formats the joint loadings/contribititons matrix for printing
#' @noRd
format_loadings = function(A, cols, digits = 3, rows, noprint = 1E-03,
                          rtn = FALSE, contributions = FALSE, namescomp = NULL) {
  
  if(is.vector(A)) A = matrix(A, ncol = 1)
  if(!is.matrix(A))
    stop(paste("A must be a matrix or vector, not a", class(A)))
  
  
  if (missing(cols))
    cols = seq_len(ncol(A))
  else if (length(cols) == 1L)
    cols = seq_len(cols)
  
  if (missing(rows))
    rows = seq_len(nrow(A))
  else if (length(rows) == 1L)
    rows = seq_len(rows)
  
  A = as.matrix(A[rows, cols, drop = FALSE])
  
  if (!is.null(namescomp) && length(namescomp) == ncol(A)) {
    colnames(A) = namescomp
  } else {
    if (!is.null(namescomp) && length(namescomp) != ncol(A))
      warning("the length of namescomp is incorrect, automatic names assigned")
    colnames(A) = paste("sPC", seq_len(ncol(A)), sep = "")
  }
  
  if (contributions == TRUE)
    load_matrix_fmt = format(round(A * 100, max(digits - 2, 0)),
                drop0trailing = TRUE, justify = "centre")
  else
    load_matrix_fmt = format(round(A, digits),
                drop0trailing = TRUE, justify = "centre")
  
  nc = nchar(load_matrix_fmt[1L], type = "c")
  load_matrix_fmt[abs(A) < noprint] = paste(rep(" ", nc), collapse = "")
  
  load_matrix_fmt = format(load_matrix_fmt, justify = "right")
  
  load_matrix_fmt
}

#' internal formats the joint summary matrix for printing
#' @noRd
format_summaries = function(sum_matrix, contributions)
  {
    sum_matrix_fmt = format(sum_matrix, digits = 2, drop0trailing = FALSE, 
                          justify = "right", trim = TRUE, scientific = FALSE)
  
  sum_matrix_fmt["Card",] = format(round(sum_matrix["Card",]), digits = 4,
                                   drop0trailing = FALSE, justify = "right",
                                   trim = TRUE, nsmall = 0
                                   )
  sum_matrix_fmt[c(1:4), ] = 
    format(round(sum_matrix[c(1:4),]*100, 1), digits = 1,  nsmall = 1, 
           justify = "right", drop0trailing = FALSE
           )
  for(i in 1:4) sum_matrix_fmt[i, ] = paste0(sum_matrix_fmt[i, ],"%")
  
  n_summat = nrow(sum_matrix)
  if (contributions)
    sum_matrix_fmt[n_summat, ] = 
    paste0(round(sum_matrix[n_summat, ]*100, 1), "%")
  else
    sum_matrix_fmt[n_summat, ] = 
    format(round(sum_matrix[n_summat, ],3), digits = 4, drop0trailing = FALSE,
           justify = "right", nsmall = 3, scientific = FALSE
           )
  sum_matrix_fmt = format(sum_matrix_fmt, justify = "right")
  sum_matrix_fmt
}


