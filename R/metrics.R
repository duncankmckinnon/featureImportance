

#' Goodness of Fit Feature Importance
#' @description This wrapper method for feature importance ranking calculates an importance
#' metric for each feature corresponding to the sum of ratios of model coefficients (or weights)
#' to the model goodness of fit for a set of fitted models which each exclude one feature.
#' @param X the features of the dataset
#' @param y the target
#' @param metric used to determine importance ranking
#' @param mod_type type of model used for y ~ X ( lm, glm )
#' @param ... other optional arguments to the model
#'
#' @return a named vector with the relative importance for each feature in X
#' @export
#'
#' @examples
#' X <- mtcars[,-1]
#' y <- mtcars[,1]
#' GOFFI(X, y)
GOFFI <- function( X, y, metric = c( 'residual', 'AIC', 'BIC' ), mod_type = c( 'lm', 'glm' ), ... ) {
  ncols <- dim( X )[2]
  xcols <- if( !is.null( names( X ) ) ) { names( X ) } else { 1:ncols }
  FI <- stats::setNames( rep( 0, ncols ), xcols )
  args <- ifelse(missing(...), list(), list(...) )
  mod_spec <- models( mod_type[1], args )
  for( xcol in xcols ) {
    x_i <- dplyr::select( X, -dplyr::all_of( xcol ) )
    xcols_i <- names( x_i )
    mod_data <- cbind(y, x_i)
    mod_formula <- format_formula( 'y', xadd = xcols_i )
    mod <- do.call( mod_spec$model, combine.lists( list( 'formula' = mod_formula, 'data' = mod_data ), mod_spec$mod_args ) )
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
          lm = list( model = stats::lm, args = mod_args, weight = 'coefficients' ),
          glm = list( model = stats::glm, args = mod_args, weight = 'coefficients' )
  )
}

metrics <- function( name, mod, ...) {
  switch( name,
          residual = sum( stats::residuals( mod ) ),
          AIC = 1 / stats::AIC( mod, ... ),
          BIC = 1 / stats::BIC( mod, ... )
  )
}
