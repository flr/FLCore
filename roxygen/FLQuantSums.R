#' Methods FLQuantSums
#'
#' Methods to compute sums, means and variances of FLQuant objects
#' 
#' This set of methods computes three different summaries (sum, mean and
#' variance) of an \code{FLQuant} object along each of the six dimensions
#' (quant, year, unit, season, area, or iter).
#' 
#' These methods simply encapsulate a call to \code{\link[base]{apply}} with
#' the corresponding dimension and function.
#' 
#' Sums are not calculated for the \code{iter} dimension, as it is used to
#' store multiple replicates of a given array of values.
#' 
#' Methods to operate over the first dimension refer to it as the \code{quant}
#' dimension, regardless of the actual name used in the object.
#' 
#' The output object will have length=1 on the selected dimension.
#' 
#' 
#' @name FLQuantSums
#' @aliases quantSums quantSums-methods quantSums,FLQuant-method quantMeans
#' quantMeans-methods quantMeans,FLQuant-method quantVars quantVars-methods
#' quantVars,FLQuant-method yearSums yearSums-methods yearSums,FLQuant-method
#' yearMeans yearMeans-methods yearMeans,FLQuant-method yearVars
#' yearVars-methods yearVars,FLQuant-method unitSums unitSums-methods
#' unitSums,FLQuant-method unitMeans unitMeans-methods unitMeans,FLQuant-method
#' unitVars unitVars-methods unitVars,FLQuant-method seasonSums
#' seasonSums-methods seasonSums,FLQuant-method seasonMeans seasonMeans-methods
#' seasonMeans,FLQuant-method seasonVars seasonVars-methods
#' seasonVars,FLQuant-method areaSums areaSums-methods areaSums,FLQuant-method
#' areaMeans areaMeans-methods areaMeans,FLQuant-method areaVars
#' areaVars-methods areaVars,FLQuant-method iterMeans iterMeans-methods
#' iterMeans,FLQuant-method iterVars iterVars-methods iterVars,FLQuant-method
#' dimSums dimSums-methods dimSums,FLQuant-method dimMeans dimMeans-methods
#' dimMeans,FLQuant-method dimVars dimVars-methods dimVars,FLQuant-method
#' @docType methods
#' @section Generic function: quantSums(x), quantMeans(x), quantVars(x)
#' 
#' yearSums(x), yearMeans(x), yearVars(x)
#' 
#' unitSums(x), unitMeans(x), unitVars(x)
#' 
#' seasonSums(x), seasonMeans(x), seasonVars(x)
#' 
#' areaSums(x), areaMeans(x), areaVars(x)
#' 
#' iterMeans(x), iterVars(x)
#' 
#' dimSums(x), dimMeans(x), dimVars(x)
#' @author The FLR Team
#' @seealso \link{FLQuant}, \link[base]{sum}, \link[base]{mean},
#' \link[stats]{var}
#' @keywords methods
#' @examples
#' 
#'  flq <- FLQuant(rnorm(4000), dim=c(5,10,2,2,2,10), quant='age')
#'  quantSums(flq)
#'  quantMeans(flq)
#'  yearSums(flq)
#'  iterMeans(flq)
#'  dim(quantSums(flq))
#'