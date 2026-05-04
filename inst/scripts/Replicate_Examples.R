## function that calls this script in utilities

library(PMlsspca)
#MSSCQ data ===========================
data(msscq)
data(ms_lookup)
data(ms_scalesh_fac)
# Loading objects:
# ms (matrix 12992 x 100. variables centered and scaled)
# ms_lookup (data.frame with questionnaire and scales)
# ms_scalesh_fac (factor with scale for each question)

ms_scalesh_names = unique(ms_lookup$scaleSH)

ms_n = nrow(msscq)
ms_p = ncol(msscq)

ms_r = cor(msscq)

plotcor(ms_r, groups = ms_scalesh_fac,   separate_groups = T, axis_labels = F, add_group_names = T, expandTop = 0.05, group_names_vjust = -0.05)


##scree and qq plots===========

ms_ret = pc_retention(ms_r, n = nrow(msscq), prn_scree = FALSE, prn_qq = F, prn_cvexp = 10, rtn_values = T)

pc_retention(ms_r, n = nrow(msscq), kaiser_line = T, rtn_values = F, prn_scree = T, prn_qq = T, nfit_line = -4)


##PCA==============

ms_pca = pca(msscq, ncomps = 4)
summary(ms_pca)

## some cardinalities are < 99 because the threshold for zero (thrsehcard) is set to 0.001

# plot contributions
plot(ms_pca, returnplot = T, vargroups = ms_scalesh_fac, varnames = F, colourscale = "ggplot", legendPosition = "right", stripnames = paste("Component", 1:4), produceplot = F)


##SPCA===================

#slow 17k obs x 100 vars hard for leaps::regsubsets

ms_pspcas95 = PMlsspca::lsspca(msscq, alpha = 0.95, ncomps = 4,  method = "p", varselection = "s", scalex = FALSE) 

summary(ms_pspcas95, variance_metrics = "both")

plot(ms_pspcas95, nplot = 4, onlynonzero = T, varnames = F, x_axis_lab = "item", legendPosition = "right", vargroups = ms_scalesh_fac, colourscale = "ggplot")  

# correlation table
round(rbind(ms_pspcas95$corComp, diag(cor(ms_pca$scores, ms_pspcas95$scores))), 2)


#CRIME=================

data("crime_data")

cr_p = ncol(crime_data)
cr_n = nrow(crime_data)

cr_r = cor(crime_data)


##Cluster variables======================
cr_hc <- stats::hclust(as.dist(1 - abs(cr_r)), method = "average")
cr_hcord <- cr_hc$order

crhc = crime_data[, cr_hcord]
crhc_r = cor(crhc)

##Correlation plot===========
crhc_cor_pl = plotcor(crhc_r, rtn = T, axis_labels = F)

#screeplot and qq-plot===========

pc_retention(cr_r, n = nrow(crime_data), kaiser_line = T, rtn_values = F, prn_scree = T, prn_qq = T, nfit_line = -4)

##PCA===============
cr_pca = pca(crime_data, ncomps = 4)
summary(cr_pca)

##SPCA===============

cr_pspcas95 = lsspca(crime_data, alpha = 0.95, ncomps = 4,  method = "p", varselection = "s", scalex = FALSE) 

##tables====================

### summaries 
su = summary(cr_pspcas95, variance_metrics = "both", rtn = T, prn = F)
cors = c(diag(cor(cr_pca$scores, cr_pspcas95$scores)))
round(rbind(su, "Cor PCs" = cors), 2)

### contributions 
print(cr_pspcas95, cols = 1:2)
