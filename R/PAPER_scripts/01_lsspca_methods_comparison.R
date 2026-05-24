
# reproducible_analysis.R
# Code to reproduce tables in "An R package to compute LS-SPCA"

require(spca, attach.required = FALSE)
require(peakRAM, attach.required = FALSE)


#load and scale data to unit variance

load("msc.Rds")#
mss = standardize_data(msc, center = FALSE, scale = TRUE)

#methods-------------------------------------
met = c("uspca", "cspca", "pspca")
mss_met_spca = vector("list", 3)
pkram = vector("list", 3)

for(i in 1:3){
  pkram[[i]] = peakRAM::peakRAM({
    mss_met_spca[[i]] = spca(mss, n_comps = 4, method = met[i])
  })[1, -1]
}

mss_met_table = paper_make_comptable(L = mss_met_spca, ind = 1:3, pRAM = pkram, par_name = "method", par_values = met)

mss_met_table

##varsel----------------------
mss_parlist

varsel = c("fwd", "fwd",  "bwd",  "step")
intense = c(TRUE, rep(FALSE, 3))
varsel_names = c("int.ive", "fwd",  "bwd",  "step")

mss_varsel_spca = vector("list", 4)
pkram = vector("list", 4)

for(i in 1:4){
  pkram[[i]] = peakRAM::peakRAM({
    mss_varsel_spca[[i]] = spca(mss, n_comps = 4, var_selection = varsel[i], intensive = intense[i])
  })[1, -1]
}

mss_varsel_table = paper_make_comptable(L = mss_varsel_spca, ind = 1:4, pRAM = pkram, par_name = "var sel", par_values = varsel_names)

mss_varsel_table

##objective ==================

obj = c("cvexp", "r2")

mss_obj_spca = vector("list", 2)
pkram = vector("list", 2)

for(i in 1:2){
  pkram[[i]] = peakRAM::peakRAM({
    mss_obj_spca[[i]] = spca(mss, n_comps = 4, objective = obj[i])
  })[1, -1]
}

mss_obj_table = paper_make_comptable(
  L = mss_obj_spca, pRAM = pkram,
  par_name = "objective", par_values = obj
)
mss_obj_table

#alpha ======================

alpha = c(0.90, 0.95, 0.98)

mss_alpha_spca = vector("list", 3)
pkram = vector("list", 3)

for(i in 1:3){
  pkram[[i]] = peakRAM::peakRAM({
    mss_alpha_spca[[i]] = spca(mss, alpha = alpha[i], n_comps = 4)
  })[1, -1]
}

mss_alpha_table = paper_make_comptable(
  L = mss_alpha_spca, pRAM = pkram,
  par_name = "alpha", par_values = alpha
)
mss_alpha_table


