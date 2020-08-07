

#' Goodness of Fit Feature Importance
#' This wrapper method for feature importance ranking calculates an importance
#' metric for each feature corresponding to the sum of ratios of model coefficients (or weights)
#' to the model goodness of fit for a set of fitted models which each exclude one feature.
#' @importClassesFrom stats lm, glm
#' @importFrom stats AIC, BIC, residuals
#' @param X
#' @param y
#' @param metric
#' @param mod_type
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
GOFFI <- function( X, y, metric = c('residual','AIC','BIC'), mod_type = 'lm', ...) {
  ncols <- dim( X )[2]
  xcols <- if( !is.null( names( X ) ) ) { names( X ) } else { 1:ncols }
  FI <- setNames( rep( 0, ncols ), xcols )
  args <- ifelse(missing(...), list(), list(...) )
  mod_spec <- models( mod_type, args )
  for( xcol in xcols ) {
    x_i <- dplyr::select( X, -xcol )
    xcols_i <- names( x_i )
    mod_data <- data.frame( y, x_i )
    mod_formula <- format_formula( names(y) , xplus = xcols_i )
    mod <- do.call( mod_spec$model, combine.lists( mod_spec$args, list( 'formula' = mod_formula, 'data' = mod_data ) ) )
    mod_metric <- metrics( metric[1], mod )
    mod_weights <- mod[[mod_spec$weight]]
    for( xcol_i in xcols_i ){
      FI[ xcol_i ] <- FI[ xcol_i ] + ( mod_weights[ xcol_i ] / mod_metric ) ^ 2
    }
  }
  return( ( FI ^ 0.5 ) / ncols )
}

models <- function( name, mod_args = list() ){
  switch( name,
          lm = list( model = lm, args = mod_args, weight = 'coefficients' ),
          glm = list( model = glm, args = mod_args, weight = 'coefficients' )
  )
}

metrics <- function( name, mod, ...) {
  switch( name,
          residual = sum( residuals( mod ) ),
          AIC = 1 / AIC( mod, ... ),
          BIC = 1 / BIC( mod, ... )
  )
}
