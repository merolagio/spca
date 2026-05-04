
# PMlsspca 1.0.5
- created new function pc_retantion to automate making diagnostic plots and stats
# PMlsspca 1.0.4
-cleaned documentation
-improved cpp to mapped view by reference instead of copies
-added return eigennvalues to pca 
-fixed a few deprecated arguments in ggplot

# PMlsspca 1.0.3

## package code
- fixed qq-plot line fitting, now correct number of points excluded in n_fit is negative
- removed option perc from screeplot 
- added NEWS.md


# PMlsspca 1.0.2

## User-facing changes
package changed data file format to .rds; collected data documentation to single help entry "datasets"
- Shiny app: added Intro page and provided datasets (MSSCQ, Crime, Holzinger) with optional scale vector upload.
- Shiny app: diagnostics plots can be refreshed; `nfit_line` now allows negative values.
- Results: added scale list panel (optional) and plot refresh; added CSV download for loadings.

## Bug fixes
- Fixed missing package dependencies in DESCRIPTION (e.g., `leaps`, `cowplot`) and ensured GitHub installs are reproducible.

## Internal changes
- Improved data-source handling in Shiny app (upload vs provided datasets).
- Added helper utilities for scale parsing and order-preserving factor construction.

# PMlsspca 1.0.1

## User-facing changes
- Initial Shiny companion app for pSPCA fits and diagnostics.

## Bug fixes
- Various small UI and deployment fixes.

# PMlsspca 1.0.0

## User-facing changes
- First public release of PMlsspca (pSPCA/uSPCA/cSPCA fitting + plotting + utilities).