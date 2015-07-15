#' Method range
#' 
#' Extraction and modification of the \emph{range} slot from objects of any
#' class inheriting from \code{\linkS4class{FLComp}}.
#'
#' For example, this method allows the age range used in the \code{fbar}
#' calcluation for an \code{FLStock} object (set by \code{minfbar} and
#' \code{maxfbar}), and the period of a year relevant to an \code{FLIndex}
#' object (set by \code{startf} \code{endf}) to be changed. Any other changes
#' that are structural in nature should not be made with this method, but rather
#' with methods such as \code{trim}, \code{expand} and \code{setPlusGroup}.
#'
#' The \emph{range} slots that are produced depend on the object being queried
#' and can include one of the following: \code{min} and \cod{max} relate to the
#' minimum and maximum quants, \code{minyear} and \code{maxyear} to the minimum
#' and maximum years, and \code{plusgroup} to the plusgroup. Additional slots
#' are included for \code{FLStock} (\code{minfbar} and \code{maxfbar}) and
#' \code{FLIndex} (\code{startf} and \code{endf}), as described above.
#'
#' @name range
#' @aliases range-methods range,FLComp,missing-method range,FLComp-method
#' range,FLlst-method range<- range<--methods range<-,FLComp-method
#' @docType methods
#' @section Generic function: range(x, i) range<-(x, i, value)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' data(ple4.index)
#' range(ple4)
#' range(ple4.index)
#'
# # Change the age range used in the fbar calculation for an FLStock object
#'  fbar(ple4)
#'  range(ple4, c('minfbar','maxfbar'))
#'  range(ple4, 'minfbar')<-4
#'  fbar(ple4)
#'