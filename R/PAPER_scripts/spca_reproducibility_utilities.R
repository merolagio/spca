#HELPER FUNCTIONS FOR JSS ARTICLE

# reproducible_analysis.R
# Functions to reproduce tables in "An R package to compute LS-SPCA"


#' Create comparison table from list of spca fits
#' 
#' Used to produce the comparison tables in Chapter 7
#' @param L List of spca objects to compare
#' @param ind Indices inside list (default: 1:length(L))
#' @param pRAM List of peakRAM results
#' @param par_name Name of parameter being compared
#' @param par_values Values of parameter being compared
#' @noRd
paper_make_comptable = function(L = list, par_values, ind = NULL, pRAM = NULL,
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
    `Cor pc` = sapply(summaries, function(x)  
      paste0("[", paste(abs(round(x["r", ], 3)),   collapse = ", "), "]"))
  )
  
  colnames(comp_table)[1] = par_name
  
  ##cor sPCs ---------------------------------------------
  corspc_list = lapply(L, function(x, n) get_val_spca(x, n), n= "spc_cor")
  maxcor = function(x) max(abs(x[lower.tri(x)]))  
  comp_table$`max|cor|` = round(sapply(corspc_list, maxcor), 3)
  
  ##time ---------------------------------------------
  comp_table$Time = sapply(pRAM, function(x) x[1,1])
  
  comp_table
}

#RUNS elasticnet::spca in an isolated R process to avoid clashes and crashes

# library(elasticnet)

# Load data
# load("msc.rda")  # or however you provide it


#' Run elasticnet SPCA in an isolated R session
#'
#' Calls elasticnet::spca() in a separate R process to avoid namespace conflicts.
#' Both the spca and elasticnet packages define S3 methods for class 'spca',
#' which can cause method dispatch issues when both are loaded simultaneously.
#' Running elasticnet in an isolated session ensures clean comparisons.
#'
#' @param x Data matrix (n x p) passed to elasticnet::spca().
#' @param tmp_lib Character string. Path to temporary library containing elasticnet.
#' @param spca_args Named list of additional arguments for elasticnet::spca()
#'   (e.g., K = number of components, sparse = "penalty").
#' @param libpaths Character vector of library paths for the child session.
#'   Defaults to current .libPaths() plus tmp_lib.
#'
#' @return A list with components:
#'   \item{fit}{The elasticnet spca object, or NULL if error occurred}
#'   \item(pRAM){The output from peakRAM, with time taken, average and peak 
#'     RAM used.}
#'   \item{error}{Error message if fit failed, NULL otherwise}
#'   \item{warnings}{Character vector of warnings, empty if none}
#'
#' @details
#' This function uses callr::r() to spawn an isolated R session. The elasticnet
#'   package must be installed in a temporary library (specified by tmp_lib) to 
#'   avoid namespace conflicts with the spca package.
#' 
#' To set up the temporary library for running the comparison scripts:
#' \code{
#' tmp_lib <- tempdir()
#' install.packages("elasticnet", lib = tmp_lib, 
#'   repos = "https://cloud.r-project.org")
#' }
#' If elasticnet is already installed in default library, `.libPaths()` can
#'   be passed to tmp_lib, though this may cause method dispatch conflicts
#'   if both packages are loaded in the same session.
#' 
#' The script handle this setup automatically.
#'
#' @noRd

run_elasticnet_spca = function(x,
                               tmp_lib,
                               spca_args = list()) {
  
  callr::r(
    func = function(x, tmp_lib, spca_args) {
      
      library(elasticnet, lib.loc = tmp_lib)
      library(peakRAM, lib.loc = tmp_lib)
      
      warn_log = character(0)
      fit = NULL
      pRAM = tryCatch(
        withCallingHandlers(
          peakRAM({
            fit = do.call(
              elasticnet::spca,
              c(list(x = x), spca_args)
            )
          }),
          warning = function(w) {
            warn_log <<- c(warn_log, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          structure(
            list(
              fit = NULL,
              pRAM = NULL,
              error = conditionMessage(e),
              warnings = warn_log
            ),
            class = "elasticnet_spca_error"
          )
        }
      )
      
      if (inherits(pRAM, "elasticnet_spca_error"))
        return(pRAM)
      
      list(
        fit = fit,
        pRAM = pRAM[, -1, drop = FALSE],
        error = NULL,
        warnings = warn_log
      )
    },
    args = list(
      x = x,
      tmp_lib = tmp_lib,
      spca_args = spca_args
    )
  )
}

