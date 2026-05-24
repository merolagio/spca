# reproducible_analysis.R
# Code to reproduce tables in "An R package to compute LS-SPCA"

# Comparison of LS-SPCA with conventional SPCA

# library(spca)
# library(peakRAM)
# library(elasticnet)

# Load data
# load("sexual_self_concept.rda")  # or however you provide it


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

# For brevity, when needed, we refer to the solutions obtained with our package function \fct{spca} and the solutions obtained with the \pkg{elasticnet} package function \fct{spca} as \code{ls-spca} and \code{en-spca}, respectively.


#Tall matrix========
load("C:/Users/merol/Dropbox/Papers/spca_revamp/SPCA_JSS_paper/paper_supplementary/MZ_basic.Rdata")
#call it Sexual Self-Concept

##mzs-------------------------
##PCA------------------------

muffle = gc(full = TRUE)
prm_mzs_pca = peakRAM::peakRAM({
  mzs_pca = pca(mzs, n_comps = 4, screeplot = TRUE)
}
)  
prm_mzs_pca[, -1]

prm_mzs_spcadef= peakRAM::peakRAM({
  mzs_default = spca(mzs, n_comps = 4)
}
)  
mzs_default$pRAM = prm_mzs_spcadef[, -1]


##LSSPCA default-------------------------------
muffle = gc(full = TRUE)
pRAM_mzs_default = peakRAM::peakRAM({
  mzs_default = spca(mzs, n_comps = 4)
}
)  
mzs_default$pRAM = pRAM_mzs_default[, -1]

#EN------
muffle = muffle = gc(full = TRUE)
mzs_en_spcadef = run_elasticnet_spca(
  x = mzs,
  tmp_lib = tmp_lib,
  spca_args = list(
    K = 4,
    para = mzs_default$cardinality,
    type = "predictor",
    sparse = "varnum"
  )
)


mzs_en_spcadef$pRAM
mzs_default$pRAM


mzs_en_spcadef$pRAM/
  mzs_default$pRAM

mzs_en_spcadef$fit$pev


mzs_en_spcadef_obj = new_spca(mzs_en_spcadef$fit$loadings, X = mzs, method_name = "elasticnet") 

names(mzs_en_spcadef_obj)

# the percentage vexp is the leading eigenvalue of the selected variables covariance matrix and it is unrelated to the vexp of the data 
mzs_en_spcadef$fit$pev
round(mzs_en_spcadef$fit$pev/mzs_en_spcadef_obj$vexp, 3)

# A comparison of the two solutions can be easily obtained with \fnc{compare_spca}.
compare_spca(list(mzs_default , mzs_en_spcadef_obj), equal_sign = TRUE,  methods_names = c("spca", "el_net"), print_loadings = FALSE,
             color_scale = "cbb", x_axis_var_names = FALSE)

#The summary table shown in Figure \ref{tab:mzs_en_comp_sum} shows how the LS-SPCA solution CVEXP is higher that 95\%, as required, and is consistently larger than SPCA CVEXP but that this difference reduces as the number of components considered increases. The smallest SPCA absolute contributions are considerably smaller. 

## From the contributions plot, shown in Figure \ref{pl:mzs_compare} 
# we observe that the SPCA sPCs tend to load on fewer scales than the corresponding LS_SPCA sPCs. 
# This impression is confirmed by observing the contributions aggregated by scale table in Table \ref{tab:mzs_compare}. The full contributions table is available in the Appendix.  This can be explained by considering that variables in the same scale are typically highly mutually correlated.

aggregate_by_group(mzs_default, groups = ms_scalesh_fac)
aggregate_by_group(mzs_en_spcadef_obj, groups = ms_scalesh_fac)
 
#The mutual correlations between sPCs is negligible for the LS-SPCA sPCs, while it is significantly higher for the SPCA solutions, as shown in Tables \ref{tab:mzs_sPC-cor_ls} and \ref{tab:mzs_sPC-cor_sp}.
round(mzs_default$spc_cor, 2)
round(mzs_en_spcadef_obj$spc_cor, 2)

#Also the correlation between sPCs and corresponding PCs
mzs_cor_spcs = rbind(
  round(mzs_default$cor_with_pc, 2),
  round(mzs_en_spcadef_obj$cor_with_pc, 2)
)
dimnames(mzs_cor_spcs) = list(c("LS-SPCA", "SPCA"), paste0("sPC", 1:4))
mzs_cor_spcs

# pspca--------------
# prm = peakRAM::peakRAM({
#   mzs_pdefault = spca(mzs, n_comps = 4, method = "pspca")
# })  
# mzs_pdefault$pRAM = prm[, -1]
# 
# compare_spca(list(mzs_pdefault , mzs_en_spcadef_obj), equal_sign = TRUE, variable_groups =NULL, methods_names = c("pspca", "el_net"), 
#              color_scale = "cbb", print_loadings = FALSE)
# 
# round(mzs_en_spcadef_obj$spc_cor, 2)
# round(mzs_pdefault$spc_cor, 2)
# 
# round(mzs_en_spcadef_obj$cor_with_pc, 3)
# round(mzs_pdefault$cor_with_pc, 3)
# 

#FAT matrix=====================
#COLON===================================

load("C:/Users/merol/AppData/Local/Temp/RtmpG8KWqP/Rlib_90d875674bfe/plsgenomics/data/Colon.rda", verbose = T)

dim(Colon$X)
CO = Colon$X
dim(CO)
colSums(CO[, 1:10])
apply(CO[, 1:10], 2, var)

save(coc, file = "coc.rda")
save(cos, file = "cos.rda")

coc = standardize_data(CO, TRUE, FALSE)
cos = standardize_data(CO, TRUE, TRUE)
dim(cos)
co_v = ata(coc)/(nrow(cos) -1)
co_r = var2cor(co_v)
save(co_v, file = "R/Remove/co_v.Rdata")
save(co_r, file = "R/Remove/co_r.Rdata")

##scaled data========================================================

###PCA------------------
prm_cos_pca = peakRAM::peakRAM({
  cos_pca = pca(cos, fat_matrix = T, screeplot = F)
})
wachter_qqplot(cos_pca$eigenvalues, p = ncol(cos), n = nrow(cos), cor = TRUE,
               n_fitline = -4)
#The screeplot suggest to retain five components while the Wachter qq-plot four. We retain four.

##spca default---------------------
#We fit a default cspca model without setting \code{fat_matrix = T} because \fct{spca} automatically switches to the fat backend based on the dataset aspect-ratio.
muffle = muffle = gc(full = TRUE)
pRAM_cos_spcadef = peakRAM::peakRAM({
  cos_spcadef = spca(cos, n_comps = 4)
})[, -1]

cos_spcadef$pRAM = pRAM_cos_spcadef

#The summary table, shown in Figure \ref{fig:cos_sum_spcadef} shows that a small number of variables, compared to those in the dataset, is enough to explain at least 95\% CVEXP. This is to be expected, given that only 62 cases are available.
summary(cos_spcadef)


# which(rowSums(cos_spcadef$loadings !=0)>1)
# cos_spcadef$contributions[1077, ]
# The heatmap showing the contributions indicates that only in one case a variable (V1077) loads on two sPCs (with a contribution of 0.9\% for sPC1). In all other cases different variables load on each sPC.  

#table(rowSums(cos_spcadef$loadings !=0)>1)
plot(cos_spcadef, plot_type = "h",)

## elasticnet
muffle = gc(full = TRUE)
Sys.time()
cos_en_spcadef = run_elasticnet_spca(
  x = cos,
  tmp_lib = tmp_lib,
  spca_args = list(
    K = 4,
    para = cos_spcadef$cardinality,
    type = "predictor",
    sparse = "varnum"
  )
)
Sys.time()

# The \pkg{spca} package function \fct{spca} is much faster (by a factor of 138:1) and uses less RAM than \pkg{elasticnet}'s  function \fct{spca}, as shown in the output below. 

#cos_en_spcadef$fit$pev
{rbind(
  LSSPCA = cos_spcadef$pRAM,
  SPCA = cos_en_spcadef$pRAM
)
cos_en_spcadef$pRAM[1]/
  cos_spcadef$pRAM[1]
}

#The  commands below cast the en-spca results into an \code{spca} object and produce comparison statistics and a bar plot of the contributions.

cos_en_spcadef_obj = new_spca(cos_en_spcadef$fit$loadings, S = cos_r, X = cos, method_name = "elasticnet") 

compare_spca(list(cos_spcadef , cos_en_spcadef_obj), equal_sign = TRUE,
            print_loadings = FALSE,  x_axis_var_names = FALSE, methods_names = c("cspca", "el_net"), color_scale = "ggplot")

#As required, all the ls-spca sPCs explain at least 95\% CVEXP whereas the en-spca sPCs explain a much smaller proportion of the CVEXP of the PCs, only recovering an even greater proportion than the ls-spca sPCs with the fourth component.  

# The output below shows the correlation matrices of the two sets of sPCs and their correlation with corresponding PCs.

cos_cor_spcs = round(
  rbind(cos_spcadef$cor_with_pc,
        cos_en_spcadef_obj$cor_with_pc), 2) 
dimnames(cos_cor_spcs) = list(c("LS-SPCA", "SPCA"), paste0("sPC", 1:4))
cos_cor_spcs

#Clearly, the en-sPCs have low correlation with the corresponding PCs whereas that of the ls-sPCs   is very high.

round(cos_spcadef$spc_cor, 2)
round(cos_en_spcadef_obj$spc_cor, 2)

#Also in this case, the en-sPCs are significantly mutually correlated whereas the ls-sPCs are almost uncorrelated.

## overfitting====
#A peculiar property of conventional SPCA when applied to fat matrices, is that it allows to compute sPCs with cardinality larger than the number of cases.

#As an example we ran en-spca requiring to compute two sPCs of cardinality equal to 100 (the set has 62 cases). The summaries of the fit and summaries of the loadings.

dim(cos)
Sys.time()
muffle = gc(full = TRUE)
cos_en_hundred = run_elasticnet_spca(
  x = cos,
  tmp_lib = tmp_lib,
  spca_args = list(
    K = 2,
    para = rep(100, 2),
    type = "predictor",
    sparse = "varnum"
  )
)
cos_en_hundred_obj = new_spca(cos_en_hundred$fit$loadings, S = co_r, X = cos, method_name = "elasticnet") 

summary(cos_en_hundred_obj)
sapply(cos_en_hundred_obj$loadings_list, summary)

#The fact that it is possible to compute sPCs with more nonzero loadings than the rank of the data matrix (numbers of case minus one), goes against the intuition that, since the columns of the matrix are linearly dependent, any linear combination of the columns can be reduced to a linear combination of at most the rank of the matrix.  

#plot.spca(cos_en_hundred_obj, controls = list( variable_names = FALSE))

#The  output elow  show how ls-spca recovers 100% of the PCs' VEXP with cardinality 61 and 59 
cos_spca999 = spca(cos, alpha = 0.9999, n_comp = 2)
summary(cos_spca999)

#NON scaled data========================================================
#In previous examples we used variables scaled to unit variance, however in some cases, PCA is applied to unscaled variables. In this section we briefly report a comparison between ls-spca and en-spca results.

#QUESTO FAMOLO SU MZ
load(file = "R/Remove/MZ_basic.Rdata", verbose = T)
mzc = standardize_data(MZ[, 1:100], scale = FALSE)
dim(mzs)
dim(mzc)

##PCA------------------
#Diagnostic pca is computed with the following command
pRAM_mzc_pca = peakRAM::peakRAM({
  mzc_pca = pca(mzc, screeplot = T)
})
summary.spca(mzc_pca, cols = 1:4)

#Based on the diagnostic plots produced by \fct{pca}, we retain four components. The commands below compute both spca solutions with their default values and cardinality as determined by \code{cspca} with \code{alpha = 0.95%}, the defaults for \fct{spca}.

##spca default---------------------
muffle = gc(full = TRUE)
pRAM_mzc_spcadef = peakRAM::peakRAM({
  mzc_spcadef = spca(mzc, n_comps = 4)
})[, -1]
mzc_spcadef$pRAM = pRAM_mzc_spcadef

muffle = gc(full = TRUE)
mzc_en_spcadef = run_elasticnet_spca(
  x = mzc,
  tmp_lib = tmp_lib,
  spca_args = list(
    K = 4,
    para = mzc_spcadef$cardinality,
    type = "predictor",
    sparse = "varnum"
  )
)

mzc_en_spcadef$fit$pev

mzc_en_spcadef$pRAM
mzc_spcadef$pRAM

mzc_en_spcadef$pRAM/
  mzc_spcadef$pRAM

mzc_en_spcadef$fit$pev


mzc_en_spcadef_obj = new_spca(mzc_en_spcadef$fit$loadings, X = mzc, method_name = "elasticnet") 

compare_spca(list(mzc_spcadef , mzc_en_spcadef_obj), equal_sign = TRUE, variable_groups =NULL, methods_names = c("ls-spca", "el-spca"), x_axis_var_names = FALSE, color_scale = "ggplot")

round(
  rbind(mzc_spcadef$cor_with_pc,
        mzc_en_spcadef_obj$cor_with_pc), 2) 

round(mzc_spcadef$spc_cor, 2)
round(mzc_en_spcadef_obj$spc_cor, 2)

