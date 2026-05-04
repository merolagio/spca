
#' @importFrom ggplot2 ggplot aes theme theme_minimal 
#' @importFrom ggplot2 theme_light theme_bw theme_classic
#' @importFrom ggplot2 geom_bar geom_tile geom_rect geom_line position_dodge
#' @importFrom ggplot2 geom_abline geom_vline geom_text geom_smooth geom_point
#' @importFrom ggplot2 element_rect  element_blank element_text unit element_line
#' @importFrom ggplot2 scale_y_discrete scale_y_continuous scale_x_discrete scale_x_continuous 
#' @importFrom ggplot2 scale_fill_manual scale_fill_grey scale_fill_gradientn xlab ylab labs
#' @importFrom ggplot2 facet_wrap facet_grid vars labs
#' @importFrom ggplot2 annotate coord_polar coord_flip rel expansion 
#' @importFrom graphics par
#' @importFrom stats cor
#' @noRd
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    # ggplot columns used in aes()
    "Var1", "Var2", "value",
    "x", "xend", "y", "yend",
    "expected", "observed",
    "eigenvalue",
    "id", "component", "variable", "method",
    "pos", "hjust",
    "varNum", "compNum",
    "loadings",
    "ncomps"
  ))
}