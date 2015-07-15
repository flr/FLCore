#' Method spr0
#' 
#' Calculates spawners per recruit at F=0.
#' 
#' This method regresses SSB/R against fbar, and estimates the intercept as
#' \code{spr0}. The method currently does not work if any of the input objects
#' have multiple units, seasons or areas (i.e. if dim(object)[3:5] > 1).
#' 
#' @name spr0
#' @aliases spr0 spr0-methods spr0,FLQuant,FLQuant,FLQuant-method
#' spr0,FLStock,missing,missing-method spr0,FLSR,missing,FLQuant-method
#' @docType methods
#' @section Generic function: quant(ssb, rec, fbar)
#' @author The FLR Team
#' @seealso \linkS4class{FLStock}, \linkS4class{FLSR}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' spr0(ple4)
#' 