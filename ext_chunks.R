## ----setup, include = FALSE----------------------------------------------------------------------
knitr::opts_chunk$set(
  prompt = TRUE,
  comment = NA,
  fig.path = "figures/",
  fig.width = 6,
  fig.height = 4.5
)
old_opt <- options(
  prompt = "R> ",
  continue = "+  ",
  width = 70,
  useFancyQuotes = FALSE
)
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)

library("spca")
source("Extended_vignette_material/spca_utilities_for_vignettes.R")


## ----load_all, include = FALSE-------------------------------------------------------------------
load("Extended_vignette_material/spca_JSS_article_results.rda")


## ----load_ho-------------------------------------------------------------------------------------
data("holzinger")
dim(holzinger)
data("holzinger_scales")


## ----pca, fig.cap = "Screeplot."-----------------------------------------------------------------
ho_pca = pca(holzinger, screeplot = TRUE, qq_plot = FALSE)


## ----qqplot, fig.cap = "qq-plot with a fitted line."---------------------------------------------
qqplot_spca(ho_pca, n_var = ncol(holzinger),
               n_obs = nrow(holzinger), n_fitline = -3)

screeplot_spca(ho_pca)

## ----spcadef, eval = FALSE-----------------------------------------------------------------------
  ho_spcadef = spca(M = holzinger,
                  alpha = 0.95,          # 95% CVEXP
                  n_comps = 4,           # 4 components
                  method = "c",          # cSPCA
                  var_selection = "f",   # forward
                  objective = "cvexp",   # stop on CVEXP
                  intensive = FALSE      # select by R-squared
                )


## ----print_spca----------------------------------------------------------------------------------
print(ho_spcadef, contributions = FALSE)


## ----summary-------------------------------------------------------------------------------------
summary(ho_spcadef, cor_with_pc = TRUE)


## ----cor_comps-----------------------------------------------------------------------------------
round(ho_spcadef$spc_cor, 2)


## ----barplot, fig.cap = "Bar plots of the contributions of each sPC."----------------------------
plot(ho_spcadef)


## ----circplot, fig.cap = "Circular bar plots of the contributions of each sPC."------------------
plot(ho_spcadef, n_plot = 3, plot_type = "c", controls = list(color_scale = "printsafe"))


## ----heatmap, fig.cap = "Heat maps of the contributions of each sPC compared with the corresponding PC contributions."----
plot(ho_spcadef, pc_weights = ho_pca$contributions, plot_type = "h")


## ----fixed, eval = FALSE-------------------------------------------------------------------------
ho_spcafixed = spca(holzinger, alpha = 0.95, n_comps = 4,
                 fixed_index_list = holzinger_scales)


## ----fix_load------------------------------------------------------------------------------------
ho_spcafixed


## ----fix_sum-------------------------------------------------------------------------------------
summary(ho_spcafixed, cor_with_pc = TRUE)


## ----pspca, eval = FALSE-------------------------------------------------------------------------
ho_pspca = spca(holzinger, n_comps = 4, alpha = 0.95,  method = "p",
                objective = "r2", var_selection = "b")


## ----compare, fig.cap = "Bar plots of the contributions of two different spca fits."-------------
compare_spca(list(ho_spcadef, ho_pspca), plot_weights = TRUE,
             color_scale = "c",
             print_weights = FALSE,
             col_short_names = TRUE,
             methods_names = c("cSPCA", "pSPCA"), show_plot = FALSE
             )


## ----aggr----------------------------------------------------------------------------------------
aggregate_by_group(ho_spcadef, groups = holzinger_scales)


## ----group plot, fig.cap = "Bar plots of the contributions filled by groups."---------------------
plot(ho_spcadef, variable_groups = holzinger_scales, controls =
       list( legend_position = "right")
)


## ----new_spca------------------------------------------------------------------------------------
A = cbind(ho_spcadef$weights[, 1], ho_pspca$weights[, 2])
ho_r = cor(holzinger)
ho_spcahyb = new_spca(A, ho_r, method_name = "hybrid")


## ----check_new_spca------------------------------------------------------------------------------
is.spca(ho_spcahyb)


## ----load_msc, include = FALSE-------------------------------------------------------------------
    load("Extended_vignette_material/msc.rda", verbose = FALSE)
    load("Extended_vignette_material/ms_scalesh_fac.rda", verbose = FALSE)
     mss = scale(msc, center = FALSE, scale = TRUE)


## ----comp_methods, eval = FALSE------------------------------------------------------------------
met = c("uspca", "cspca", "pspca")
mss_met_spca = vector("list", 3)

for(i in 1:3){
  mss_met_spca[[i]] = spca(mss, n_comps = 4, method = met[i])
}

mss_met_table = make_comparative_table(L = mss_met_spca, ind = 1:3,
                                       pRAM = NULL, par_name = "method",
                                       par_values = met)


## ----comp_meth, echo = FALSE---------------------------------------------------------------------
mss_met_table


## ----comp_varsel, echo = FALSE-------------------------------------------------------------------
mss_varsel_table


## ----comp_alpha, echo = FALSE--------------------------------------------------------------------
mss_alpha_table


## ----conv_sum, echo = FALSE----------------------------------------------------------------------
compare_spca(list(mss_spcadef, mss_en_spcadef_obj),
             methods_names = c("ls", "el"),  col_short_names = FALSE,
             print_weights = FALSE, plot_weights = FALSE, print_tables = TRUE, return_tables = FALSE)


## ----mss_aggr, echo = FALSE----------------------------------------------------------------------
print(mss_agg_by_scale_print, quote = FALSE)


## ----gas_print, echo = FALSE---------------------------------------------------------------------
compare_spca(list(gas_lsspca, gas_enspca_obj, gas_abspca_obj),
             x_axis_var_names = FALSE,
             methods_names = c("ls", "en", "ab"),
             col_short_names = FALSE,
             plot_weights = FALSE, print_weights = FALSE)


## ----cleanup, include = FALSE--------------------------------------------------------------------
options(old_opt)

