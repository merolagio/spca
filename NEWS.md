
# spca 1.1.0

## Major changes

- Added projection SPCA (`method = "pspca"`), including a new C++ implementation.
- Added a dedicated C++ backend for fat matrices, where the number of variables exceeds the number of observations.
- Redesigned the C++ tall-matrix engine for improved speed, memory use, and maintainability.
- Refactored the C++ codebase so large objects are passed by reference where appropriate.
- Added `try`/`catch` wrappers around the main C++ entry points to improve error reporting from compiled code.

## R interface

- Reworked the main `spca()` interface while preserving the core LS-SPCA workflow.
- Rewrote `plot.spca()` with improved bar plots, circular plots, heat maps, grouping support, and plotting controls.
- Rewrote `new_spca()` to make externally computed sparse loading matrices easier to store and inspect as `spca` objects.
- Improved printing and summary methods for `spca` objects.
- Improved comparison tools for multiple sparse PCA fits.
- Added or revised helper functions for grouped summaries, diagnostics, and validation.

## Reliability and testing

- Added a `testthat` test suite.
- Fixed multiple issues in the R interface and C++ backend.
- Improved input validation and error messages.
- Fixed edge cases affecting sparse loadings, component summaries, and variance-explained diagnostics.

## Documentation

- Rewrote and expanded the package documentation.
- Rewrote the vignettes for the revamped package.
- Updated examples to reflect the current interface and available LS-SPCA methods.
- Added documentation for projection SPCA and the tall/fat backend distinction.


# spca 1.0.0

## Major rewrite

- First full release of the revamped `spca` package.
- Replaced the previous computational core with a new C++ backend.
- Added separate computational paths for tall and fat data matrices.
- Added projection SPCA as a new LS-SPCA method.
- Updated the package interface to support the redesigned backend.

## Package structure

- Reorganized internal R and C++ code.
- Improved consistency of returned `spca` objects.
- Updated exported functions and methods to use a common object structure.
- Added tests and revised examples for the new implementation.


# spca 0.9.0

## Development release

- Introduced the new C++ tall-matrix engine.
- Began migration of computational routines from the earlier implementation to the redesigned backend.
- Added preliminary support for projection SPCA.
- Improved memory handling in compiled code.
- Updated internal validation routines.


# spca 0.8.0

## Development release

- Reworked plotting methods for `spca` objects.
- Added improved graphical comparison of loadings and contributions.
- Added support for grouped variables in summaries and plots.
- Revised helper functions used by examples and vignettes.
- Improved documentation of sparse loadings, contributions, VEXP, CVEXP, and component correlations.


# spca 0.7.0

## Development release

- Began the package revamp following version 0.6.0.
- Reorganized the R interface around fitted `spca` objects.
- Improved print and summary output.
- Added early validation and diagnostic improvements.
- Fixed several bugs from the previous CRAN release.


# spca 0.6.0

- Previous CRAN release.