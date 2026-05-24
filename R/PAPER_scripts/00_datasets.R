


MS = read.csv(file = "R/Remove/msscq_data.csv", as.is = TRUE)
dim(MS)
names(MS)
load(file = "R/Remove/MZ_basic.Rdata", verbose = T)
mzc = standardize_data(MZ, scale = FALSE)
objects(pattern = "^mz")
colMeans(msc[, 1:10])
apply(msc[, 1:10], 2, var)

dim(MZ)
dim(msc)
msc = mzc
msc_scale_fac = ms_scale_fac

rem_ms = objects(pattern = "^ms\\_")
rm(list = rem_ms)

add_msc = list(
  msc = msc,
  msc_scale_fac = msc_scale_fac
)
names(add_msc)
dim(add_msc$msc)
all.equal(add_msc$msc, mzc)

save(list = c(
  "msc", "msc_scale_fac"), 
  file = "../SPCA_JSS_paper/paper_supplementary/msc.Rda")
