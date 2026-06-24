## CRAN resubmission

The package was previously archived. This submission is a substantially revised
release with new methodology, a redesigned C++ backend, tests, updated
documentation, and revised vignettes.

## Spelling

The following words are intentional:
- LSSPCA: should be LS-SPCA (fixed). It is the abbreviation for least-squares sparse principal component analysis.
- Merola: maintainer surname.

## Windows compiler flag

The Windows-only flag -Wa,-mbig-obj is required for the Rtools/MinGW-w64
toolchain to compile the C++/Eigen source files. Without it, Windows builds fail
with a too many sections assembler error. The flag is only used on Windows, so 
portability to other OS's should not be a concern.