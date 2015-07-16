#' Method uom
#'
#' Units of Measurement
#' 
#' The \code{units} attribute of \code{FLQuant} objects provides a mechanism for
#' keeping track of the units of measurement of that particular piece of data.
#' This method in the form \code{uom()} carries out a conversion across units.
#' 
#' Arithmetic operators for \code{FLQuant} objects operate with a limited set of
#' units of measurement, and will output the right unit when two appropriate
#' objects are arithmetically combined. For example, the product of objects with
#' units of 'kg' and '1000' will output an object with units of 't' (for
#' metric tonnes).
#' 
#' Operations involving combinations of units not defined will result in the
#' \code{units} attribute simply storing a string indicating the input units of
#' measurement and the operation carried out, as in '10 * 1000'.
#' 
#' Note that no scaling or modification of the values in the object takes
#' place.
#'
#' @name uom
#' @docType methods
#' @param op The arithmetic operator to be used, one of '+', '-', '*' or '/'
#' @param u1 The units of measurement string of the first object
#' @param u2 The units of measurement string of the second object
#' @return a string with the corresponding units of measurement, a string such
#' as '10 *100' when not compatible
#' @section Recognized Units:
#' 
#' The following units of measurement are recognized by the 'Arith' operators
#' (+, -, * /). \describe{ \item{Weight}{'kg', 't'} \item{Numbers}{1 -
#' 100000000, 1e0 - 1e8, 10^0 - 10^8} \item{Mortality}{'m', 'f', 'z', 'hr'}
#' \item{Other}{'NA'} }
#' @author The FLR Team
#' @seealso \code{\linkS4class{FLQuant}} \code{\link{units,FLArray-method}}
#' @keywords function
#' @examples
#' 
#' # Conversion between weights
#'   flq1 <- FLQuant(1, units='kg')
#'   flq2 <- FLQuant(5, units='1000')
#'   flq1 * flq2
#'   uom('*', units(flq1), units(flq2))
#'
#' # Conversion between mortalities
#'   flq1 <- FLQuant(0.2, units='m')
#'   flq2 <- FLQuant(0.34, units='f')
#'   flq1 + flq2
#'   uom('+', units(flq1), units(flq2))
#' 