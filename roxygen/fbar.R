#' Method fbar
#'
#' Calculates mean harvest rate or fishing mortality
#' 
#' The mean harvest rate or fishing mortality for the years between
#' \emph{minfbar} and \emph{maxfbar}, as found in the \code{range} slot,
#' is returned.
#' 
#' @aliases fbar fbar-methods fbar,FLStock-method fbar,FLBiol-method
#' @param object An FLStock or FLBiol object
#' @param ... Any extra arguments, currently unused
#' @return An object of class \code{\linkS4class{FLQuant}}
#' @author FLR Team
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' fbar(ple4)
#' 