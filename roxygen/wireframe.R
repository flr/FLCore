#' 3D plot for FLQuant objects
#' 
#' Method to plot 3D representations of FLQuant objects
#' 
#' 
#' @name wireframe
#' @aliases wireframe wireframe,FLQuant-method
#' @docType methods
#' @param x a \code{formula} formula for lattice
#' @param data a \code{FLQuant} object with the values
#' @param ... Additional argument list to be passed to \code{wireframe}
#' @return a \code{wireframe} plot
#' @examples
#' 
#' data(ple4)
#' wireframe(data~age+year, data=harvest(ple4))
#' 