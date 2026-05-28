#library(ggplot2)
#detach("package:ggplot2", unload = TRUE)

# Define themes for plots ==============
spca_bar_theme = function(legend_position = "bottom", 
                          grid_type = "horizontal") {
  th = ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = legend_position,
      legend.title = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black",
                                           linewidth = 1) ,
      strip.text = element_text(size = 14, color = "black"),
      strip.background = 
        element_rect(fill = "white", color = "black", 
                     linetype = 1, linewidth = ggplot2::rel(1))        
    )
  if (grid_type[1] == "none") {
    th = th + ggplot2::theme(panel.grid = ggplot2::element_blank())
  } 
  else 
    if (grid_type[1] == "horizontal") {
      th = th + ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(
          linewidth = 0.1, color = "grey90"
        )
      )
    }
  th
} 


spca_circular_theme = function(legend_position, 
                               grid_type = "horizontal") {
  th = ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = legend_position,
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(rep(-1, 4), "cm")
    )
  if (grid_type == "full") {
    warning(paste("vertical grid lines are not supported for circular",
                  "bar plots, setting full grid to only horizontal"))
    grid_type = "horizontal" 
  }
  if (grid_type == "horizontal")
    th = th + theme(panel.grid.major.y = 
                      ggplot2::element_line(linewidth = 0.8, 
                                            color = "grey90")
    )
  th
}

# DEFINE COLORS =============================
spca_color_scale = function(color_scale) {
  
  color_scale = color_scale[1]
  
  if (grepl("^c", color_scale))
    return("cbb")
  if (grepl("^p", color_scale))
    return("printsafe")
  if (grepl("^b", color_scale))
    return("bw")
  if ((length(color_scale) == 1) && grepl("^g", color_scale))
    return("ggplot")
  
  warning(paste("color_scale must be one of (ggplot, cbb, printsafe,",
                "bw) setting it to ggplot"))
  return("ggplot")
}


spca_tile_palette = function() {
  c(
    "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7",
    "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC"
  )
}


spca_fill_palette = function(color_scale, pc_loadings = NULL) {
  if (color_scale[1] == "cbb") {
    pal = c(
      "#000000", "#E69F00", "#56B4E9", "#009E73",
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
    )
    if (!is.null(pc_loadings))
      pal = pal[c(1, 7)]
    return(pal)
  }
  
  if (color_scale[1] == "printsafe") {
    if (!is.null(pc_loadings))
      pal = pal[c(1, 4)]
    else{
    # if (ncolors > 8) {
    #   warning(paste("cannot use printsafe palette with more than 9",
    #                 "colours. Switching to colour_scale ggplot"))
    #   color_scale = "ggplot"
    # } else {
    pal = rev(c(
      "#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59",
      "#EF6548", "#D7301F", "#B30000", "#7F0000"
    ))
    #}
  }
    return(pal)
  }
  
  if ((color_scale[1] == "bw") || (color_scale[1] == "ggplot"))
    return(NULL)
  
  color_scale
}


spca_add_fill_scale = function(pl, color_scale, data_df,
                               variable_groups = NULL, pc_loadings = NULL) {
  if (color_scale[1] == "ggplot")
    return(pl)
  
  if (color_scale[1] == "bw")
    return(pl + ggplot2::scale_fill_grey())
  
  pal = spca_fill_palette(color_scale = color_scale, 
                          pc_loadings = pc_loadings)
  
  if (!is.null(variable_groups)) {
    nfill = nlevels(data_df$variable_groups)
    pal = rep_len(pal, nfill)
    return(
      pl + ggplot2::scale_fill_manual(
        limits = levels(data_df$variable_groups),
        values = pal
      )
    )
  }
  
  if (!is.null(pc_loadings)) {
    nfill = nlevels(data_df$method)
  } else {
    nfill = nlevels(data_df$component)
  }
  pal = rep_len(pal, nfill)
  
  pl + ggplot2::scale_fill_manual(values = pal)
}

# CREATE PLOTS =============================

create_data = function(x, n_plot, contributions, only_nonzero,  variable_groups, pc_loadings, lbl, facet_labels){
  p = nrow(x$loadings)
  
  # common part, PCloadings binded later if needed
  
  #browser()
  
  if(!is.null(pc_loadings))
    facet_labels = paste("Comp", 1:n_plot)
  
  data_df = data.frame(
    variable = factor(rep(1:p, n_plot), labels = lbl),
    component = factor(rep(1:n_plot, each = p), 
                       labels = facet_labels
                       )
    )
  if (contributions) 
    data_df$value = c(x$contributions[, 1:n_plot])
  else
    data_df$value = c(x$loadings[, 1:n_plot])
  if (!is.null(variable_groups))
    data_df$variable_groups = rep(variable_groups, n_plot)
  
  if (!is.null(pc_loadings)){
    df = data_df
    if (contributions) 
      df$value = c(make_contributions(pc_loadings[, 1:n_plot]))
    else
      df$value = c(pc_loadings[, 1:n_plot])
    
    data_df = rbind(data_df, df)
    data_df$method = factor(c(rep(1:2, each = p * n_plot)),
                            labels = c("SPCA", "PCA")
    )
  } else{
    #only_nonzero disabled if pc_loadings == TRUE
    if (only_nonzero == TRUE) {
      ind_nonzero = apply(x$loadings, 1, function(a) any(a != 0))
      data_df = droplevels(data_df[ind_nonzero, ])
    }
  }
  
  data_df
}

# Helper: Create circular barplot
plot_spca_circular = function(
    data_df, n_plot, plotlab, lbl, 
    legend_position, grid_type, 
    legend_title, variable_groups, color_scale,
    adjust_labels_circ) {
  # Set a number of 'empty bar' to add at the end of each component
  empty_bar = 4
  to_add = 
    data.frame(matrix(
      NA, empty_bar * nlevels(data_df$component), ncol(data_df)))
  colnames(to_add) = colnames(data_df)
  to_add$component = 
    rep(levels(data_df$component), each = empty_bar)
  data_df = rbind(data_df, to_add)
  data_df = data_df[order(data_df$component), ]
  data_df$id = seq(1, nrow(data_df))
  
  # Get the name and the y position of each label
  label_data = data_df
  number_of_bar = nrow(label_data)
  angle = 90 - 360 * (label_data$id - 0.5) / number_of_bar
  label_data$hjust = ifelse(angle < -90, 1, 0)
  label_data$angle = ifelse(angle < -90, angle + 180, angle)
  
  mval = max(stats::na.omit(data_df$value))
  label_data$pos = data_df$value
  label_data$pos[data_df$value == 0] = mval * 0.66
  label_data$pos[data_df$value < 0] = 0.05
  
  # Make the plot
  if (!is.null(variable_groups)) {
    pl = ggplot2::ggplot(data_df, 
                         ggplot2::aes(x = as.factor(id), y = value, 
                                      fill = variable_groups))
  } else {
    pl = ggplot2::ggplot(data_df, 
                         ggplot2::aes(x = as.factor(id), y = value, 
                                      fill = component))
  }
  pl = pl + 
    ggplot2::geom_col(position = "dodge", 
                      alpha = 0.80,
                      color = ifelse(color_scale[1] == "printsafe", 
                                     "black", NA)) +
    ggplot2::ylim(-0.5 - stats::median(abs(stats::na.omit(
      data_df$value))), mval + 0.2) +
    spca_circular_theme(legend_position = legend_position, 
                        grid_type = grid_type) + 
    ggplot2::labs(fill = legend_title) +
    ggplot2::geom_abline(slope = 0, intercept = 0)
  
  indg = (!is.na(data_df$variable))
  minloads = tapply(X = as.numeric(data_df$id[indg]), 
                    INDEX = data_df$component[indg], min)
  medloads = tapply(data_df$id[indg], 
                    INDEX = data_df$component[indg], stats::median)
  lia = min(data_df$value[indg]) - 0.05
  
  medangle = 360 * medloads / number_of_bar
  anglela = 180 - medangle
  anglela[((anglela > 90) | (anglela < -90))] = 
    anglela[((anglela > 90) | (anglela < -90))] + 180
  if (!is.null(adjust_labels_circ)) {
    if (length(adjust_labels_circ) == n_plot)
      anglela = anglela + adjust_labels_circ
    else
      warning(paste("need to pass as many values to adjustLabelCirc",
                    "as n_plot"))
  }
  
  pl = pl +
    ggplot2::annotate(
      "text", x = medloads, y = rep(lia, nlevels(data_df$component)),
      label = paste("comp", 1:n_plot), hjust = 0.5, size = 4,
      angle = anglela, fontface = "bold"
    ) +
    ggplot2::coord_polar()
  
  
  if (isTRUE(plotlab)) {
    pl = pl + ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = id, y = pos + 0.05, label = variable, 
                   hjust = hjust),
      color = "black", fontface = "bold", alpha = 0.85,
      size = 3.5, angle = label_data$angle, inherit.aes = FALSE
    )
  }
  
  pl
}

# Helper: Create standard barplot (handles both SPCA-only and with PCs)
plot_spca_bars = function(data_df, 
                          n_plot, 
                          contributions,
                          variable_groups,
                          has_pc_loadings, 
                          plotlab, 
                          lbl, 
                          color_scale, 
                          legend_position, 
                          grid_type, 
                          x_axis_lab) {
  
  # Create plot (same code for both cases)
  nrows = ceiling(n_plot / 3)
  ncols = ceiling(n_plot / nrows)
#  browser()
  #variable groups
  if (!is.null(variable_groups) && (!has_pc_loadings))
    pl = ggplot2::ggplot(data_df, 
                         ggplot2::aes(x = variable, y = value, 
                                      fill = variable_groups))
  else 
    if (has_pc_loadings){
    pl = ggplot2::ggplot(data_df, 
                         ggplot2::aes(x = variable, y = value, 
                                      fill = method))
    }
  else
    pl = ggplot2::ggplot(data_df, 
                         ggplot2::aes(x = variable, y = value, 
                                      fill = component))
  
  pl = pl +
    ggplot2::geom_col(position = "dodge", 
                      alpha = ifelse((!has_pc_loadings), 1, 0.75),
                      color = ifelse(color_scale[1] == "printsafe", 
                                     "black", NA)) +
    ggplot2::facet_wrap(facets = ggplot2::vars(component), 
                        ncol = ncols, nrow = nrows) +
    ggplot2::geom_abline(slope = 0, intercept = 0) +
    spca_bar_theme(legend_position = legend_position, 
                   grid_type = grid_type) +
    ggplot2::xlab(x_axis_lab) +
    ggplot2::ylab(ifelse(contributions == TRUE, 
                         "contributions", "loadings"))
  
  if (isTRUE(plotlab))
    pl = pl + ggplot2::theme(axis.text.x = 
                               ggplot2::element_text(angle = 90, 
                                                     vjust = 0.5, 
                                                     size = 8))
  else
    pl = pl + ggplot2::theme(axis.ticks = ggplot2::element_blank(), 
                             axis.text.x = ggplot2::element_blank())
  
  if (contributions)
    pl = pl + ggplot2::scale_y_continuous(labels = scales::percent)
  
  if (!is.null(variable_groups) && (!has_pc_loadings))
    pl = pl + ggplot2::theme(legend.position = legend_position)
  
  pl
}


# Helper: Create heatmap (handles both SPCA-only and with PCs)
plot_spca_heatmap = function(
    data_df,
    n_plot,
    contributions, 
    has_pc_loadings,
    indices,
    lbl, 
    legend_position, 
    flip_heatmap, 
    heatmap_color_range) {
  
      tile_pal = spca_tile_palette()
      
    #browser()
    
    if (heatmap_color_range == "values"){
    col_lims = range(data_df$value)
    maxlim =   ceiling( (max(abs(col_lims)*10)))/10
    col_lims[1] = -ceiling(maxlim*10)/10
    col_lims[2] = ceiling(maxlim*10)/10
    # col_lims[1] = -ceiling(col_lims*10)/10
    # col_lims[2] = ceiling(col_lims*10)/10
    } else
      col_lims = c(-1, 1)
      
    
    pl = ggplot2::ggplot(data_df, ggplot2::aes(variable, component)) +
      ggplot2::geom_tile(ggplot2::aes(fill = value),  colour = "gray75") +
      ggplot2::theme_bw() +
      ggplot2::scale_fill_gradientn(
        colours = tile_pal, limits = col_lims,
        name = ifelse(contributions, "Contributions", "Loadings")
      ) +
      ggplot2::theme(legend.position = legend_position)
 
    pl = pl +
      ggplot2::geom_abline(intercept = (1:n_plot) + 0.5, slope = 0, 
                           colour = "grey75") +
      ggplot2::geom_vline(xintercept = 
                            (seq_along(unique(data_df$variable))) + 0.5, 
                          colour = "grey75") +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, 
                                            size = 8)
      )
    
    # pl

    if(has_pc_loadings){
      #browser() 
      
      data_df$varNum = rep(rep(seq_along(lbl), n_plot), 2)
      data_df$compNum = c(rep(1:n_plot, each = length(lbl)), 
                          rep(1:n_plot, each = length(lbl)) + 0.5)
      
      lab_y = factor(1:(2 * n_plot), 
                     labels = paste0(c("sPC", "PC"), 
                                    rep(1:n_plot, each = 2)))
      
      pl = ggplot2::ggplot(
        data_df,
        ggplot2::aes(xmin = varNum, xmax = varNum + 1,
                     ymin = compNum, ymax = compNum + 0.5, 
                     fill = value)
      ) +
        ggplot2::geom_rect() +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_gradientn(colours = tile_pal, 
                                      limits = col_lims) +
        ggplot2::scale_x_continuous(
          breaks = seq(1.5, length(lbl) + 0.5, 1),
          labels = lbl, expand = c(0, 0)
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(1.25, (length(lab_y) / 2) + 1, 0.5),
          labels = lab_y, expand = c(0, 0)
        ) +
        ggplot2::geom_abline(intercept = (1:n_plot) + 1, slope = 0, 
                             colour = "black", linewidth = 1.5) +
        ggplot2::geom_abline(intercept = (1:n_plot) + 0.5, slope = 0, 
                             colour = "gray75", linewidth = 1) +
        ggplot2::geom_vline(xintercept = (seq_along(lbl)) + 1, 
                            colour = "grey75") +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.ontop = TRUE,
          panel.background = 
            ggplot2::element_rect(fill = "transparent"),
          axis.text.x = 
            ggplot2::element_text(angle = 90, vjust = 0.5, size = 8)
        )
    }
    if (flip_heatmap == TRUE)
      pl = pl + ggplot2::coord_flip()
    
  
  pl
}
#Validate inputs=================
validate_plot_inputs = function(inputs, controls, fun_formals) {

  crl_defaults = eval(fun_formals$controls, envir = parent.frame())
  inp_defaults = fun_formals[names(fun_formals) != "controls"]

  #browser()
  inputs = lapply(inputs, eval)
  
  validate_no_na(arg_list = inputs)  
  

  inputs$plot_type = match.arg(inputs$plot_type,
                               choices = eval(inp_defaults$plot_type))
  
  
  validate_booleans(contributions = inputs$contributions,
                    only_nonzero = inputs$only_nonzero,
                    return_plot = inputs$return_plot,
                    show_plot = inputs$show_plot
                    )
  
  if (!is.null(inputs$pc_loadings)) 
     if ((!is.matrix(inputs$pc_loadings)) && 
        (!is.data.frame(inputs$pc_loadings)))
       stop("pc_loadings must be a matrix or a data.frame, or NULL")
  
  if (!is.null(inputs$variable_groups))
    if ((!is.vector(inputs$variable_groups)) &&
       (!is.factor(inputs$variable_groups)))
      stop("variable_groups must be a vector or a factor, or NULL")
  
  if (!is.null(inputs$plot_title))
    if (!is.character(inputs$plot_title))
    stop("plot_title must be a character string, or NULL")
  
  
  
##validate controls================  
if (is.null(controls)) {
  controls = crl_defaults
} else {
  bad_names = setdiff(names(controls), names(crl_defaults))
  if (length(bad_names) > 0) {
    stop("unknown controls: ", paste(bad_names, collapse = ", "))
  }
  controls = modifyList(crl_defaults, controls, keep.null = FALSE)
}
  #browser()
  validate_no_na(arg_list = controls)  
  
  controls$color_scale = match.arg(controls$color_scale[1],
                                   choices = crl_defaults$color_scale)
  controls$legend_position = 
    match.arg(controls$legend_position[1], 
              choices = crl_defaults$legend_position)
  
  controls$grid_type = match.arg(controls$grid_type[1],
                                 choices = crl_defaults$grid_type)
  #browser()
  
  controls$heatmap_color_range = 
    match.arg(controls$heatmap_color_range[1],
              choices = crl_defaults$heatmap_color_range)
  

  validate_booleans(flip_heatmap = controls$flip_heatmap)

  
  list(inputs = inputs, controls = controls)
}

# plot.spca=====================
#' Plot an `spca` object
#'
#' Plots the sparse loadings (or the corresponding percent contributions)
#'   stored in an `spca` object. Plots can be produced as linear bar plots,
#'   circular bar plots, or a heatmap. For large problems, it is recommended
#'   to set only_nonzero = TRUE` and `variable_names = FALSE`.
#'
#' If `pc_loadings` is provided, the plot overlays SPCA and PCA values
#'   for comparison (circular bar plots with PCA are not implemented and
#'   standard bar plots are used instead). All loadings are plotted with this option (only_nonzero set to FALSE).
#' 
#' For character arguments defined by a default vector of allowed values, the
#'   first character of the supplied string is used to match the
#'   corresponding option.
#' 
#' @param x An object of class `spca`.
#' @param n_plot (NULL) Integer. Number of components to plot. If `NULL`, all
#'   components in `x` are plotted.
#' @param plot_type (c("bars", "circular", "heatmap")) Character. Plot type:
#'   `"bars"`, `"circular"`, or `"heatmap"`. The first character is enough.
#' @param contributions (TRUE) Logical. If `TRUE`, plot contributions
#'   (loadings scaled to sum to 100\% within each component); otherwise plot
#'   L2 unit loadings.
#' @param only_nonzero (TRUE) Logical. If `TRUE`, plot only nonzero entries.
#' @param pc_loadings (NULL) Optional numeric matrix of PCA loadings or PCA
#'   contributions with the same dimensions as `x$loadings`. If supplied, SPCA
#'   and PCA values are plotted together for comparison.
#' @param variable_groups (NULL) Optional factor or character vector of length
#'   \eqn{p} defining groups of variables. If provided, bars or tiles are
#'   colored by group instead of by component.
#' @param plot_title (NULL) Optional character. Plot title.
#' @param return_plot (FALSE) Logical. If `TRUE`, return the `ggplot2` object.
#' @param show_plot (TRUE) Logical. If `TRUE`, print the plot.
#' @param controls (list()) List of graphical controls. Supported entries are
#'   `color_scale`, `variable_names`, `legend_position`, `grid_type`,
#'   `facet_labels`, `legend_title`, `x_axis_lab`, `adjust_labels_circ`,
#'   `flip_heatmap`, and `heatmap_color_range`.
#' @param ... Further arguments; currently ignored.
#' 
#' @details
#' \itemize{
#' \item{\code{controls}}{These are common parameters to customize the plots.
#'   Advanced users can override these by returning the ggplot and applying
#'   preferred settings through \code{ggplot} functions. The variable that
#'   controls faceting is \code{component}. See the examples.}
#' \item{\code{color_scale}}{
#'   \itemize{
#'   \item \code{"cbb"} is colorblind-friendly with black.
#'   \item \code{"printsafe"} is colorblind- and printer-friendly.
#'   \item \code{"bw"} uses gray tones.
#'   \item \code{"ggplot"} uses the default ggplot2 scale.
#'   }
#'   For more than 8 colors, \code{"cbb"} and \code{"printsafe"} are
#'   automatically changed to \code{"ggplot"}.}
#' \item{\code{pc_loadings}}{Adds PC loadings or contributions side by side
#'   with those of the sPCs for comparison. \code{only_zero} is automatically
#'   set to \code{FALSE} and only two colors are used. For large matrices, the
#'   plots may become difficult to interpret.}
#' \item{Circular plots}{Circular plots are fragile and, for large sets of
#'   loadings or several components, may require adjusting the default tuning
#'   parameters with \code{adjust_labels_circ}. This option is not recommended
#'   for large matrices.}
#' \item{\code{variable_names}}{Behaves as follows:
#'   \itemize{
#'   \item \code{NULL}: the row names of the loading matrix are used, or
#'     \code{V1, ..., Vp} if missing.
#'   \item \code{"none"}: variable names are not used in plotting.
#'   \item character vector: if it is of length \code{p}, it is used as the
#'     variable names. If the length is different, it is switched to
#'     \code{NULL}.
#'   }
#' }
#' }
#'  @note When variable groups are present, `legend_position is set to "bottom".`
#'   
#' @return If `return_plot = TRUE`, the `ggplot2` object; otherwise `NULL`
#'   (invisibly).
#' @family spca
#' @references
#' Circular bar plot layouts follow examples from
#' \url{https://www.r-graph-gallery.com/all-graphs/}.
#' The `cbb` palette is adapted from
#' \url{http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/}.
#' The `printsafe` palette corresponds to `OrRd` from
#' \url{http://colorbrewer2.org/}.
#' @examples
#' \dontrun{
#' data(holzinger)
#' myspca = spca(ho)
#' myplot = plot(ho)
#' change facetting and legend position
#' myplot + facet_wrap(facets = vars(component), ncol = 4, nrow = 1) +
#'   theme(legend.position = "r")
#' }
#' @family spca
#' @export
#' @method plot spca
plot.spca = function(
    x,
    n_plot = NULL,
    plot_type = c("bars", "circular", "heatmap"),
    contributions = TRUE,
    only_nonzero = TRUE,
    pc_loadings = NULL,
    variable_groups = NULL,
    plot_title = NULL,
    return_plot = FALSE,
    show_plot = TRUE,
    controls = list(
      color_scale = c("ggplot", "cbb", "printsafe", "bw"),
      variable_names = NULL,
      legend_position = c("none", "bottom", "right", "top", "left"),
      grid_type = c("horizontal", "full", "none"),
      facet_labels = NULL,
      legend_title = NULL,
      x_axis_lab = "variables",
      adjust_labels_circ = NULL,
      flip_heatmap = FALSE,
      heatmap_color_range = c("values", "unit")
    ), 
    ...) {
  
  # Validation=============
  #needed to prevent wrong inputs to go unnoticed
  dots = list(...)
  if (length(dots) > 0) {
    stop("Unused arguments: ", paste(names(dots), collapse = ", "))
  }
  
  test = validate_spca(x)
  if (!test)
    stop("plot.spca requires an spca object as first argument")
  
  p = nrow(x$loadings)
  
  
  ## validate character inputs by initial characters   
  # 
  fun_formals = formals(sys.function())
  fun_inp = as.list(match.call(expand.dots = FALSE))[-1]
  
  #browser()
  inputs = fun_formals
  inputs[names(fun_inp)] = fun_inp
  
  inputs$x = NULL
  inputs$controls = NULL
  inputs$... = NULL

  #browser()
  
  validated = validate_plot_inputs(inputs, controls, fun_formals)
  names(validated)[1] = "inputs"

  #acquire noncontrol input======
  if (is.null(n_plot))
    n_plot = ncol(x$loadings)
  plot_type = validated$inputs$plot_type
  contributions = validated$inputs$contributions
  only_nonzero = validated$inputs$only_nonzero
  pc_loadings = validated$inputs$pc_loadings
  variable_groups = validated$inputs$variable_groups
  plot_title = validated$inputs$plot_title
  return_plot = validated$inputs$return_plot
  show_plot = validated$inputs$show_plot

  #acquire control input======
  color_scale = validated$controls$color_scale
  variable_names = validated$controls$variable_names
  facet_labels = validated$controls$facet_labels
  legend_position = validated$controls$legend_position
  grid_type = validated$controls$grid_type
  legend_title = validated$controls$legend_title
  x_axis_lab = validated$controls$x_axis_lab
  adjust_labels_circ = validated$controls$adjust_labels_circ
  flip_heatmap = validated$controls$flip_heatmap
  heatmap_color_range = validated$controls$heatmap_color_range  

  # plots take a matrix
  if ((!is.null(pc_loadings)) && (is.vector(pc_loadings))) {
      pc_loadings = matrix(pc_loadings, ncol = 1)
      }

  ## Contributions are not in minimal spca object
  if (contributions && is.null(x$contributions)) {
    x$contributions = make_contributions(x$loadings[, 1:n_plot])
  }
  
  if (!is.null(variable_groups)){
    if (length(variable_groups) != p) {
      warning(paste("variable_groups must have length equal to the 
                    number of", "variables. Ignored."))
      variable_groups = NULL
    }  else {
      ## assigning colors to groups requires a factor
      if ((!is.factor(variable_groups)) && is.vector(variable_groups))
        variable_groups = vec2fac(variable_groups)
      if (!is.factor(variable_groups)) {
        warning("variable_groups must be a character vector or a
                    factor. Ignored.")
        variable_groups = NULL
      }
    }
    if (is.null(legend_position) || (legend_position == "none")){
      warning("legend is necessary to identify the variable groups, changed its position to bottom")
      legend_position = "bottom"
    } 
  }
  
# Legend for circular plots must be on the right or plot breaks  
  if (plot_type == "circular") {
          legend_position = ifelse(legend_position == "none", "none",
                                   "right")
          warning("Legend moved to right for circular plot")
  }
# too little space in circular plots to fit PC's loadings  

  if ((plot_type == "circular") && (!is.null(pc_loadings))){
    warning(paste("Circular barplots with PCloadings are not",
                  "implemented (and probably would not be useful),",
                  "using standard barplot"))
    plot_type = "bars"
  }
  #legend is need to distinguish sPCs from Pcs
  if(!is.null(pc_loadings))
    legend_position = "bottom"
  
  color_scale = spca_color_scale(color_scale)
  
  # needed to pass to plot 
  plotlab = TRUE
  if (length(variable_names) > 2) {
    if (length(variable_names) == p)
      lbl = variable_names
    else{
      warning("variable_names must have the same length of the 
              number of variables. Switched to generic names")
      variable_names = NULL
    }
  }  
  
  if (is.null(variable_names[1])) {
    if (is.null(rownames(x$loadings))) {
      lbl = paste0("V", 1:p)
    } else 
      lbl = rownames(x$loadings)
  } else {
    lbl = paste0("V", 1:p) 
    plotlab = NULL
  }
  
  if (is.null(facet_labels)) {
    facet_labels = paste0("sPC", 1:n_plot)
  } else {
    if (length(facet_labels) < n_plot) {
      warning(paste("length of stripname must be equal to the number",
                    "of plots. Using default."))
      facet_labels = paste0("sPC", 1:n_plot)
    }
  }
  
  #Also zero PC loadings needed for comparison
  if(!is.null(pc_loadings))
    only_nonzero = FALSE
#browser()  
  data_df = create_data(x, n_plot, contributions, only_nonzero,  variable_groups, pc_loadings, lbl, facet_labels)

#circular plot ==============
  if (plot_type == "circular") {
    # Circular barplot (SPCA only - prepare data here)
    
    pl = plot_spca_circular(
      data_df = data_df,
      n_plot = n_plot,
      plotlab = plotlab,
      lbl = lbl,
      legend_position = legend_position,
      grid_type = grid_type,
      legend_title = legend_title,
      variable_groups = variable_groups,
      color_scale = color_scale,
      adjust_labels_circ = adjust_labels_circ
    )
    # barplot ================
  } else if (plot_type == "bars") {
    
    pl = plot_spca_bars(
      data_df,
      n_plot = n_plot,
      contributions = contributions,
      variable_groups = variable_groups,
      has_pc_loadings = (!is.null(pc_loadings)),
      plotlab = plotlab,
      lbl = lbl,
      color_scale = color_scale,
      legend_position = legend_position,
      grid_type = grid_type,
      x_axis_lab = x_axis_lab
    )
  ##heatmap ======================  
  } else if (plot_type == "heatmap") {
    idx = unlist(x$indices)
    if(legend_position == "none") 
      legend_position = "bottom"
    pl = plot_spca_heatmap(
    data_df,
    n_plot = n_plot,
    contributions = contributions,
    has_pc_loadings = (!is.null(pc_loadings)),
    indices = idx,
    lbl = lbl,
    legend_position = legend_position,
    flip_heatmap = flip_heatmap,
    heatmap_color_range = heatmap_color_range
    )
  }
  
  ## Add discrete fill scale (only for non-heatmap plots)
  if (plot_type != "heatmap") {
    pl = spca_add_fill_scale(
      pl = pl,
      color_scale = color_scale,
      data_df = data_df,
      variable_groups = variable_groups,
      pc_loadings = pc_loadings
    )
  }
  ## Plot title==========================
  if (!is.null(plot_title))
    pl = pl + ggplot2::labs(title = plot_title)
  
  if (show_plot == TRUE)
    print(pl)
  
  if (return_plot == TRUE)
    return(pl)
  else
    invisible()
}
