
R2latex = function(x, caption, label, position = c("H", "h"), width = "0.95\\linewidth") {
  txt = capture.output(x)
  sub("NA", "", x)
  cat(paste0("\\begin{table}[",
             ifelse(position[1] == "h","!ht]\n", "H]\n")))
  cat("\\centering\n")
  cat("\\begin{minipage}{", width, "}\n", sep = "")
  cat("\\begin{verbatim}\n")
  cat(txt, sep = "\n")
  cat("\n\\end{verbatim}\n")
  cat("\\end{minipage}\n")
  cat("\\caption{", caption, "}\\label{", label, "}\n", sep = "")
  cat("\\end{table}\n")
}

#' Create comparison table from list of spca fits
#' 
#' Used to produce the comparison tables in Chapter 7
#' @param L List of spca objects to compare
#' @param ind Indices inside list (default: 1:length(L))
#' @param pRAM List of peakRAM results
#' @param par_name Name of parameter being compared
#' @param par_values Values of parameter being compared
#' 
#' #' Create comparison table from list of spca fits
#'
#' Used to produce the comparison tables in the article.
#'
#' @param L List of \class{spca} objects to compare.
#' @param ind Integer vector. Indices of `L` to include. If `NULL`, all objects
#'   in `L` are used.
#' @param pRAM List of `peakRAM` results, one for each fitted object.
#' @param par_name Character string. Name of the parameter being compared; used
#'   as the name of the first column.
#' @param par_values Character vector. Values of the parameter being compared;
#'   used as entries in the first column.
#'
#' @return A data frame with one row for each selected fit. The first column is
#'  named with 
#'  `par_name` and contains `par_values`. 
#'  `CVEXP` is the final reported relative cumulative variance explained,
#'    by the last component
#'  `Card` lists the component cardinalities, `Cor pc` lists
#'     the absolute correlations between each sPC and its corresponding PC,
#'  `max|cor|` is the largest absolute pairwise correlation among the sPCs,
#'  `Time` is the first timing/memory value stored in each `peakRAM` result.
#' @noRd
make_comparative_table = function(L = list, par_values, ind = NULL, pRAM = NULL,
                                  par_name = NULL){
  n_par = length(L) 
  
  get_val_spca = function(x, name){x[[which(names(x) == name)]]} 
  ncomps_all = sapply(L, function(x, n) length(get_val_spca(x, n)), n= "vexp")
  
  if (!all(diff(ncomps_all)) == 0)
    stop("The objects in L must have the same number of components")
  
  if(is.null(ind))  ind = seq_len(n_par)
  
  summaries = vector("list", n_par)
  for (i in seq_len(n_par)){
    summaries[[i]] = summary(L[[ind[i]]], cor_with_pc =
                               TRUE, print_table = FALSE, return_table = TRUE)
  }
  
  get_val_mat = function(x, name){x[name,]}   
  
  # TABLE ===================================================================
  comp_table = data.frame(
    blank = par_values,
    CVEXP = 
      sprintf("%.1f%%", sapply(summaries, FUN = function(x, n) 
        get_val_mat(x, n), n = "Rcvexp")[4, ]*100),
    Card = sapply(summaries, function(x)  paste0("[", paste(round(x["Card", ]),
                                                            collapse = ", "), "]")),
    "Cor with PCs" = sapply(summaries, function(x)  
      paste0("[", paste(abs(round(x["r", ], 2)),   collapse = ", "), "]"))
  )
  if(!is.null(par_name))
    colnames(comp_table)[1] = par_name
  
  ##cor sPCs ---------------------------------------------
  corspc_list = lapply(L, function(x, n) get_val_spca(x, n), n= "spc_cor")
  maxcor = function(x) max(abs(x[lower.tri(x)]))  
  comp_table$`max|cor|` = round(sapply(corspc_list, maxcor), 3)
  
  ##time ---------------------------------------------
  if(!is.null(pRAM))
    comp_table$Time = sapply(pRAM, function(x) x[1,1])
  
  comp_table
}


#data generation==============

generate_data = function(n, r, p, sigma, stand = TRUE, seed = 1){
  set.seed(seed)
  M = matrix(rnorm(n * r), n, r)       #signal
  B = matrix(rnorm(r * p, 0, 0.5), r, p)       # r x p loading matrix
  E = matrix(rnorm(n * p, 0, sigma), n, p)  # noise
  X = spca:::ab(M, B) + E
  if(stand)
    spca:::standardize_data(X, TRUE, TRUE)
  else
    spca:::standardize_data(X, TRUE, FALSE)
}

