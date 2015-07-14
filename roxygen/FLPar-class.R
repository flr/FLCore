#' Class FLPar
#' 
#' A class for storing parameters of a model.
#'
#' The \code{FLPar} class is based on the array class which can store
#' Monte Carlo samples and the names of the relevant parameter vectors.
#' 
#' Methods for this class include subsetting and replacement as for the
#' \code{\linkS4class{FLQuant}} class. There are methods for extracting
#' statistics of the sample (mean, median etc.) and for plotting the parameter
#' samples.
#'
#' @name FLPar
#' @aliases FLPar-class FLPar FLPar-methods FLPar,array-method
#' FLPar,missing-method FLPar,vector-method FLPar,FLPar-method
#' @docType class
#' @section Slots: \describe{ \item{.Data}{Describe slot. \code{array}.}
#' \item{units}{Units of measurement. \code{character}.} }
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link[base]{as.data.frame},
#' \link[lattice]{densityplot}, \link[lattice]{histogram}, \link{iter},
#' \link{iter<-}, \link[base]{mean}, \link[stats]{median},
#' \link[graphics]{plot}, \link[lattice]{splom}, \link[base]{summary},
#' \link{units,FLPar-method}, \link{units<-,FLPar,character-method},
#' \link[stats]{var}
#' @keywords classes
#' @examples
#' 
#' FLPar(rnorm(4), params=c('a','b','c','sigma2'))
#'
#' FLPar(rnorm(20), dimnames=list(params=c('a','b'), year=1990:1999, iter=1),
#'   units='NA')
#'
#' # with iters
#'   FLPar(rnorm(80), params=c('a', 'b'), iter=1:40)
#'