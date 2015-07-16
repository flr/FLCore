#' Method window
#'
#' Extract time (year) windows of an FLR object
#' 
#' This method extracts a section of, or extends an \code{FLQuant} or other FLR
#' objects along the \code{year} dimension. If a frequency is specified, the
#' new object contains data at only those year steps.
#' 
#' Although objects of class \code{\link{FLQuant}} do have another temporal
#' dimension, \code{season},  window currently only works along the \code{year}
#' dimension. To subset/extend along other dimensions, refer to
#' \link{Extract-FLCore}, \code{\link{trim}}, or \code{\link{expand}}.
#'
#' @name window
#' @aliases window,FLQuant-method window,FLComp-method window,FLlst-method
#' @docType methods
#' @section Generic function: window(x)
#' @author The FLR Team
#' @seealso \link[stats]{window}, \link{Extract-FLCore}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(50), dimnames=list(age=1:5, year=1991:2000))
#' window(flq, start=1995, end=1998)
#' window(flq, start=1990, end=2010, frequency=2)
#' 