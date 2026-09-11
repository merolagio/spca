# CLAUDE.md - spca R package

## Editing discipline
- Strictly surgical edits only: make exactly and only what was agreed.
- no unrequested additions, removals, signature changes, or restructuring,
    not even minor ones.
- If you see a possible improvement beyond what was asked, list it
    separately as a suggestion - never implement it silently.
- Always ask before making any uncertain change.
- No hidden changes of any kind, however small: this is a
  production codebase maintained alone.
-  Never edit or modify files in the "/spca" directory or its subdirectories. 
    If asked, read files but create modified
    copies and save them into the directory         
    "C:\Users\merol\Dropbox\Papers\spca_revamp\CLAUDE" creating (or using
    if it already exists) a subdirectory with current date (Vietnam time), 
    such as "2026-09-11" for example. I will give you the name to use for 
    the edited files, or ask if not.
  
## R style (hard constraints)
- assignment by `=`, never use `<-`. Exception files in the "testthat"       
    directory.
- never use pipes `%>%` or `|>`.
- do not introduce new packages if needed for improvement, ask.
- don't use tidyverse packages `dplyr`.
- never use tibbles, if a function returns tibbles warn me.
- no intermediate variables if used only once. 
- operators like `=`, `+`, `-`, etc are surrounded by white spaces.
- use my `R` internal functions and wrappers to `C++` for matrix operations in 
   Internal_utilities_and_cpp_wrappers.R  or elsewhere.
- keep code transparent but do use vectorized operaors (sapply, lapply, etc).
- line length should be maximum 80 characters. Comments and messages/warnings 
   can be longer.
- always check that the documentation covers all arguments and contains the
    default value in the signature.   
    
## C++ style (hard constraints)
- line length should be maximum 80 characters. Comments and messages/warnings 
   can be longer.
- keep code transparent.
- No iterators; no unnecessary intermediate variables.
- Plain loops with clear comments; transparent operations, not clever ones.
- No cryptic C++ (e.g. `static_cast`, `std::set`) without explanation.
- Do not add `static` to functions unless requested.
- Do not remove `using namespace` declarations unless requested.
- No trailing underscores in argument names (e.g. `force_in_` is wrong -
    use `force_in_nullable` or similar).
- Documentation is for programmers, not for users. So must be clear and
   exhaustive but essential.

## Naming conventions
- snake_case for variable names.
- camelCase with a capital C suffix for R-visible C++ functions
  (e.g. `objectVerbC()`).
- File versioning uses dot notation (`v5.3`, not `v5_3`).

## Terminology
- The package uses `weights` (not `loadings`) for the coefficients
  used to construct components, as of the JSS submission. Deprecated
  aliases exist for backward compatibility with `loadings`/`loadings_list` 
  do not remove them.

## Tests
- testthat is used for the test suite.
- they must cover all functions and methods exported by `spca`, but cannot be 
   exhaustive of all possible misbehaviour
- Run tests using `devtools::test()` not `R CMD check`