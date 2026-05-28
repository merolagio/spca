

# save(holzinger_scales, file = "data/holzinger_scales.rda")
# save(holzinger, file = "data/holzinger.rda")


# load("data/holzinger_scales.rda", verbose = T)
# load("data/hsp.rda", verbose = T)
# 
# holzinger_scales = hsp_scalesh_fac
# holzinger = hsp 
# 
# holzinger = standardize_data(holzinger, center = T, scale = T)



# holzinger data=======================
#' Holzinger--Swineford Student Ability data and scales
#'
#' This dataset is based on the classic Holzinger and Swineford (1939) Student
#'  Ability dataset.
#'  
#' We use the version distributed with the \code{psychTools} package. For
#'  comparability with previous analyses, we select 12 items and only students
#'  from the Grant--White School (see also Ferrara, Martella, and Vichi, 2019).
#'
#' The 12 items correspond to four ability scales: 
#'  spatial (SPL), verbal (VBL), speed (SPD), and mathematical (MTH). 
#'  The data provided with this package are scaled to mean zero and unit
#'  variance. The scales are available as a factor called `holzinger_scales`
#'
#' @format
#' \describe{
#' \item{holzinger}{A numeric data frame with 145 rows and 12 variables. 
#' The variables are:}
#' }
#' \describe{
#'   \item{visual}{Visual perception test (SPL).}
#'   \item{cubes}{Cubes test (SPL).}
#'   \item{flags}{Lozenges test (SPL).}
#'   \item{paragraph}{Paragraph comprehension test (VBL).}
#'   \item{sentence}{Sentence completion test (VBL).}
#'   \item{wordm}{Word meaning test (VBL).}
#'   \item{addition}{Addition test (SPD).}
#'   \item{counting}{Counting groups of dots test (SPD).}
#'   \item{straight}{Straight and curved capitals test (SPD).}
#'   \item{deduct}{Deduction test (MTH).}
#'   \item{numeric}{Numerical puzzles test (MTH).}
#'   \item{series}{Series completion test (MTH).}
#' }
#' 
#' @references
#' Holzinger, K. J., and Swineford, F. (1939). \emph{A study in factor analysis:
#'  The stability of a bi-factor solution}.
#'  Supplementary Educational Monographs, No. 48.
#'
#' Ferrara, C., Martella, F., and Vichi, M. (2019). Probabilistic disjoint
#'  principal component analysis.
#' \emph{Multivariate Behavioral Research}, 54(1), 47--61.
#'
#' @name holzinger
#' @docType data
#' @keywords datasets
##' @describeIn destination description


# 
# 
# #MSSCQ data=======================
# #' Multidimensional Sexual Self-Concept Questionnaire (MSSCQ) data
# #'
# #' The data come from an interactive version of the MSSCQ hosted by OpenPsychometrics.
# #' Sexual self-concept refers to a person's view of their own sexual behaviors and actions.
# #' The MSSCQ was created by William E. Snell, Jr. (1998) for the general study of sexuality
# #' and measures 20 scales.
# #'
# #' The dataset contains 17,685 responses to 100 statements, each rated on a 5-point Likert
# #' scale: 1 = Not at all characteristic of me, 2 = Slightly characteristic of me,
# #' 3 = Somewhat characteristic of me, 4 = Moderately characteristic of me,
# #' 5 = Very characteristic of me. Items belonging to the same scale were placed together
# #' in the questionnaire. The data in this package are ordered by scale membership. Scores
# #' for the reverse-scored items are reversed.
# #'
# #' @name msscq
# #' @docType data
# #' @aliases ms ms_lookup ms_scalesh_fac
# #'
# #' @source OpenPsychometrics raw data: \url{https://openpsychometrics.org/_rawdata/};
# #'   test page: \url{https://openpsychometrics.org/tests/MSSCQ.php}.
# #'
# #' @format
# #' \describe{
# #'   \item{ms}{A data frame with 17,685 rows (respondents) and 100 columns (items), coded on a 1--5 Likert scale.}
# #'   \item{ms_lookup}{A data frame with 100 rows and 5 variables (Item, Position, Scale, Code, Statement).}
# #'   \item{ms_scalesh_fac}{A factor of length 100 giving each item’s scale membership, with levels in questionnaire/scale order.}
# #' }
# #'
# #' @section Items:
# #' The lookup table \code{ms_lookup} reports the item identifier, position in the questionnaire,
# #' the scale name, a short scale code, and the full statement text. Items marked with `(R)`
# #' are reverse items (as indicated in the statement text).
# #'
# #' @keywords dataset
# NULL
# # Crime data===============
# #' Crime data and variable lookup
# #'
# #' \code{crime_data} combines socio-economic data from the 1990 US Census, law enforcement
# #' data from the 1990 US LEMAS survey, and crime data from the 1995 FBI UCR.
# #'
# #' In this package, the variable order was rearranged and 25 variables with missing values were removed.
# #' The variables in \code{crime_data} are scaled to mean zero and unit variance.
# #'
# #' \code{crime_var_lookup_df} provides variable definitions corresponding to the columns of
# #' \code{crime_data}.
# #'
# #' @name crime_data
# #' @docType data
# #' @aliases crime_var_lookup
# #'
# #' @source \url{http://archive.ics.uci.edu/ml/machine-learning-databases/communities/}
# #'
# #' @references
# #' Redmond, M. A. and Baveja, A. (2002). A Data-Driven Software Tool for Enabling Cooperative
# #' Information Sharing Among Police Departments. \emph{European Journal of Operational Research},
# #' 141, 660--678.
# #'
# #' @format
# #' \describe{
# #'   \item{crime_data}{A numeric data frame with 1,994 observations and 99 variables scaled to mean zero and unit variance.}
# #'   \item{crime_var_lookup_df}{A data frame giving variable definitions for \code{crime_data}.}
# #' }
# #'
# #' @keywords dataset
# NULL
# 
# 
