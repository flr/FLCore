#' Method dims
#'
#' List with information on object dimensions
#' 
#' Method \code{dims} returns a named list with information on the dimensions
#' and dimension names of a given object. The list returned could be
#' extended in the future and currently contains, depending on the class of the
#' object, some of the following: \describe{ \item{quant}{Length of the first
#' dimensions, i.e. number of ages, lengths, etc.} \item{min}{First quant}
#' \item{max}{Last quant} \item{year}{Number of years} \item{minyear}{First
#' year in series} \item{maxyear}{Last year in series} \item{cohort}{Number of
#' cohorts} \item{mincohort}{First cohort in series} \item{maxcohort}{Last
#' cohort in series} \item{unit}{Length of the third (\code{unit}) dimension}
#' \item{season}{Length of the fourth (\code{season}) dimension}
#' \item{area}{Length of the fifth (\code{area}) dimension} \item{iter}{Length
#' of the sixth (\code{iter}) dimension} } Values in the returned list are of
#' class \code{numeric}, unless dimnames are strings with no numeric
#' translation, in which case the result is \code{NA}.
#' 
#' Please note that the name of the first element in the returned list changes
#' with the name of the first dimension in the input object. Use
#' \code{\link{quant}} to obtain the name and extract the relevant element from
#' the result list.
#'
#' @name dims
#' @aliases dims dims-methods dims,FLQuant-method dims,FLPar-method
#' dims,FLCohort-method dims,FLIndex-method dims,FLComp-method
#' @docType methods
#' @section Generic function: dims(obj)
#' @author The FLR Team
#' @seealso \code{\link[base]{dimnames}}, \code{\link{FLQuant}}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(96), dim=c(3,8,1,4), quant='age')
#' dims(flq)
#' 
#' # Number of seasons
#'   dims(flq)$season
#' 
#' # Length of first dimension
#'   dims(flq)[[quant(flq)]]
#' 