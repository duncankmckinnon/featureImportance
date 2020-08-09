

#' Goodness of Fit Feature Importance
#' @description This wrapper method for feature importance ranking calculates an importance
#' metric for each feature corresponding to the sum of ratios of model coefficients (or weights)
#' to the model goodness of fit for the set of all models with 1 feature excluded.
#' @param X the features of the dataset
#' @param y the target
#' @param model type of model used for y ~ X ( currently supports linear, logistic and randomforest models )
#' @param metric used to determine importance ranking
#' @param normalize whether the weights should be normalized (sum to 1), default = FALSE to maintain comparability
#' @param ... other optional arguments to the model
#'
#' @return a named vector with the sorted relative importance (weights) for each feature in X
#' @export
#'
#' @examples
#' set.seed(2020)
#' X <- mtcars[,-1]
#' y <- mtcars[,1]
#' GOFFI(X, y, model = 'lm', metric = 'error')
#' GOFFI(X, y, model = 'lm', metric = 'AIC')
#' GOFFI(X, y, model = 'lm', metric = 'BIC')
#' GOFFI(X, y, model = 'rf', metric = 'error')
GOFFI <- function( X, y, model = c( 'lm', 'glm', 'rf' ), metric = c( 'error', 'AIC', 'BIC' ), normalize = FALSE, ...) {
  ncols <- dim( X )[2]
  xcols <- if( !is.null( names( X ) ) ) { names( X ) } else { 1:ncols }
  FI <- stats::setNames( rep( 0, ncols ), xcols )
  args <- list(...)
  mod_spec <- models( name = model[1], metric = metric[1], mod_args = args )
  for( xcol in xcols ) {
    x_i <- dplyr::select( X, -dplyr::all_of( xcol ) )
    xcols_i <- names( x_i )
    mod_data <- cbind(y, x_i)
    mod_formula <- format_formula( 'y', xadd = xcols_i )
    mod <- do.call( mod_spec$model, combine.lists( list( 'formula' = mod_formula, 'data' = mod_data ), mod_spec$mod_args ) )
    mod_metric <- metrics( mod_spec, mod )
    mod_weights <- weights( mod_spec, mod )
    for( xcol_i in xcols_i ){
      FI[ xcol_i ] <- FI[ xcol_i ] + ( mod_weights[ xcol_i ] / mod_metric ) ^ 2
    }
  }
  weights <- sort( ( FI ^ 0.5 ), decreasing = TRUE )
  if( normalize == TRUE ) {
    weights <- weights / sum( weights )
  }
  return( weights )
}

#' models selector for wrapper importance methods
#' @keywords internal
models <- function( name, metric, mod_args = list() ){
  switch( name,
          lm = list( model = stats::lm, metric = metric, args = mod_args, weight = 'coefficients', error = 'residuals' ),
          glm = list( model = stats::glm, metric = metric, args = mod_args, weight = 'coefficients', error = 'residuals' ),
          rf = list( model = randomForest::randomForest, metric = 'error', args = combine.lists( list( 'importance' = TRUE, 'type' = 2 ), mod_args), weight = 'importance', error = 'rferror' ) )
}

#' metrics selector for wrapper importance methods
#' @keywords internal
metrics <- function( spec, mod, ...) {
  switch( spec$metric,
          error = switch( spec$error,
                          residuals = sum( stats::residuals( mod ) ),
                          rferror = switch( mod$type,
                                            regression = sum( mod[['mse']] ),
                                            classification = sum( mod[['err.rate']] ) ) ),
          AIC = 1 / stats::AIC( mod, ... ),
          BIC = 1 / stats::BIC( mod, ... ) )
}

#' weights selector for wrapper importance methods
#' @keywords internal
weights <- function( spec, mod, ... ) {
  switch( spec$weight,
          coefficients = stats::coefficients( mod ),
          importance = switch( mod$type,
                  regression = randomForest::importance( mod, type = 2 )[, 1],
                  classification = randomForest::importance( mod )[, spec[['args']][['type']]] ) )
}
