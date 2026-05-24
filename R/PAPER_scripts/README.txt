REQUIREMENTS FOR REPRODUCIBILITY
=================================

Required R packages:
- spca (this package - install from source tarball provided)
- callr (CRAN)
- peakRAM (CRAN)
- elasticnet (CRAN - will be installed in temp library by script)

Install dependencies:
  install.packages(c("callr", "peakRAM"))

The script will automatically install elasticnet in a temporary 
library to avoid namespace conflicts.