

#' Remove Last N Characters
#' @description utility function to remove n characters from the end of a character string or strings.
#' Extends functional tools available in stringr package for this common operation.  Also allows for
#' a set of characters (missing='' by default) to be specified to show that the string was over-truncated.
#' @param str the string or vector of strings to be truncated
#' @param n the number of characters at the end to remove
#' @param missing optional sequence to use if an entry is truncated away (default = '')
#'
#' @return the string or strings, reduced by n characters
#' @export
#'
#' @examples
#' remove_last_n('a sequence #', 2)
#' remove_last_n(c('a couple+','of sequences+'), 1, '...')
remove_last_n <- function( str, n = 0, missing = '' ) {
  retstr <- str
  if( n >= 1 ) {
    retstr <- sapply( str, FUN = function( s ) {
      strlen <- stringr::str_length( s )
      if( n < strlen ) {
        return( stringr::str_trunc( s,
                                    width = ( strlen - n ),
                                    ellipsis =  '' ) )
      } else {
        return( missing )
      }
    }, simplify = TRUE, USE.NAMES = FALSE)
  }
  return( retstr )
}


#' Format character arguments into R Formula
#' @description takes in arguments for y and x and returns formula in the form
#' y ~ x.  Capable of handling complex formulas, returning them in the form
#' y ~ x1 + x2 + x2 * x3 \* x4 + x2 ^ 2 + x3 ^ 0.5... Inputs are in the form of vectors
#' or lists of characters associated with each operation, with handling for +, * and ^.
#' @param y the target variable
#' @param xadd a vector of column name arguments to be added together (a+b...) in the formula
#' @param xmultiply a vector of column name arguments to be multiplied together (e.g. xmultiply = c(a,b,c) -> a*b\*c) or
#' a list of seperate sets of column names arguments to be multiplied and added together (e.g. xmultiply = list(c(a,b,c),c(b,c)) -> a*b\*c+b\*c )
#' @param xpower a vector of column name arguments to be elevated to the power of power (e.g. xpower = c(a,b,c), power = 2 -> a^2+b^2+c^2) or
#' a list of (column name, power) pairs to be added together (e.g. xpower = list(c(a,2),c(b,0.5)) -> a^2+b^0.5)
#' @param power an optional power to elevate all the values of xpower (default = 2)
#'
#' @return a formula in the form {y}~{xadd}{+xmultiply}{+xpower}
#' @export
#'
#' @examples
#' format_formula('y', xadd=c('a','b'))
#' format_formula('y', xmultiply=c('a','b'))
#' format_formula('y', xmultiply=list(c('a','b','c'), c('c','a')))
#' format_formula('y', xpower=c('a','b'))
#' format_formula('y', xpower=c('a','b'), pow=0.5)
#' format_formula('y', xpower=list(c('a',2), c('b',0.5)))
#' format_formula('y', xadd=c('a','b'),
#' xmultiply=list(c('a','b','c'),c('c','a')),
#' xpower=list(c('a',2), c('b',0.5)))
format_formula <- function( y, xadd = NULL, xmultiply = NULL, xpower = NULL, power = 2 ) {
  allvars <- ''
  finalform <- paste0( y, '~' )
  n <- 2
  if( !is.null( xmultiply ) ) {

    # validate xmultiply format options if present
    if( !( is.character( xmultiply ) ||
           ( is.list( xmultiply ) &&
             all( sapply( xmultiply, is.character, simplify = TRUE ) ) ) ) ){
      stop( 'Error: xmultiply format', call. = TRUE )
    }

    # add multiplied components
    n <- 1
    xmultiplyvars <- ops_util( xmultiply, '*' )
    allvars <- xmultiplyvars
  }
  if( !is.null( xpower ) ) {

    # validate xpower format options if present
    if( !( ( is.character( xpower ) &&
             is.numeric( power ) ) ||
           ( is.list( xpower ) &&
             all( sapply( xpower, function( v ) ( length( v ) == 2 ) &&
                          is.character( v[1] ) ), simplify = TRUE ) ) ) ) {
      stop( 'Error: xpower format', call. = TRUE )
    }
    n <- 1

    # reformat vector and power into list
    if( !is.list( xpower ) ) {
      xpower <- lapply( xpower, function( x ) c( x, power ) )
    }

    # add power components
    xpowervars <- ops_util( xpower, '^' )
    allvars <- ifelse( allvars == '', xpowervars, op_util( c( allvars, xpowervars ), '+' ) )
  }
  if( !is.null( xadd ) ) {

    # validate xadd format
    if( !is.character( xadd ) ){
      stop( 'Error: xadd format', call. = TRUE )
    }

    # add xadd components
    xaddvars <- op_util( c( xadd, allvars ), '+' , n )
    allvars <- xaddvars
  }

  # replace any subtraction with -
  finalform <- stringr::str_replace_all( paste0( finalform, allvars ), stringr::fixed('+-'), '-' )
  return( formula( finalform ) )
}

#' utility function for formatting sets of operations
#' @keyword internal
ops_util <- function( x, operator, n = 1 ) {
  if( is.list( x ) ) {

    # add individual operations together
    x <- op_util( sapply( x, function( o ) {
        return( op_util( o, operator, n ) )
      }
    ), '+' )
  }
  return(  op_util( x, operator, n ) )
}

#' utility function for formatting single operation
#' @keyword internal
op_util <- function( x, operator, n = 1 ) remove_last_n( paste0( x, sep = operator, collapse = '' ), n )

#' utility function for combining lists
#' @keyword internal
combine.lists <- function( list1, list2 ) {
  # Combine lists 'list1' and 'list2', giving precedence to elements found in 'list2':
  # that is, if $something is found in both 'list1' and 'list2',
  # the new (output) list will have the same values as 'list2' in $something

  # Version 1.0 (August 2017)
  #
  # Function developed by
  # Patrick Belisle
  # Division of Clinical Epidemiology
  # McGill University Hospital Center
  # Montreal, Qc, Can
  #
  # patrick.belisle@rimuhc.ca
  # http://www.medicine.mcgill.ca/epidemiology/Joseph/PBelisle/BetaParmsFromQuantiles.html


  list1.names <- names( list1 )
  list2.names <- names( list2 )

  new.list <- list1


  tmp <- match( list2.names, list1.names )
  w <- which( !is.na( tmp ) )

  if ( length( w ) > 0 ) {
    # take values from list2 in matching dimension names
    tmp <- tmp[!is.na( tmp )]
    new.list[[ tmp ]] <- list2[[ w ]]

    # append elements of 'list2' with unmatched names
    new.list <- c( new.list, list2[ -w ] )
  }
  else
  {
    new.list <- c( new.list, list2 )
  }

  new.list
} # end of combine.lists
