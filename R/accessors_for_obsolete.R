
# internal accessors ==================
.get_spca_weights = function(obj, required = TRUE) {
  if (!is.null(obj$weights))
    return(obj$weights)
  
  if (!is.null(obj$loadings))
    return(obj$loadings)
  
  if (required)
    stop("The spca object contains neither `weights` nor legacy `loadings`.",
         call. = FALSE)
  
  NULL
}

.get_spca_weights_list = function(obj, required = TRUE) {
  if (!is.null(obj$weights_list))
    return(obj$weights_list)
  
  if (!is.null(obj$loadings_list))
    return(obj$loadings_list)
  
  if (required) {
    stop(
      "The spca object contains neither `weights_list` nor legacy `loadings_list`.",
      call. = FALSE
    )
  }
  
  NULL
}

# Legacy internal aliases. New code should use the weights accessors above.
.get_spca_loadings = function(obj, required = TRUE) {
  .get_spca_weights(obj, required = required)
}

.get_spca_loadings_list = function(obj, required = TRUE) {
  .get_spca_weights_list(obj, required = required)
}
