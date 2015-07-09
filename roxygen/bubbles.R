#' Method Bubbles plot
#' 
#' This method plots three dimensional data such as matrices by age and year or
#' age-class, very common in fisheries. The bubbles are proportional to the
#' values in the matrix. Note that \code{bubbles} accepts an argument
#' \code{bub.scale} to control the relative size of the bubbles. Positive and
#' negative values have separate colours.
#'
#' @name bubbles
#' @aliases bubbles bubbles-methods bubbles,formula,FLQuant-method
#' bubbles,formula,FLQuants-method bubbles,formula,FLCohort-method
#' bubbles,formula,data.frame-method
#' @docType methods
#' @section Generic function: bubbles(x, data)
#' @author The FLR Team
#' @seealso \link[lattice]{lattice}, \code{\linkS4class{FLQuant}},
#' \code{\linkS4class{FLQuants}},\code{\linkS4class{FLCohort}}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' bubbles(age~year, data=catch.n(ple4))
#' bubbles(age~year, data=catch.n(ple4), bub.scale=5)
#' bubbles(age~cohort, data=FLCohort(catch.n(ple4)), bub.scale=5)
#' 
#' qt01 <- log(catch.n(ple4)+1)
#' qt02 <- qt01+rnorm(length(qt01))
#' flqs <- FLQuants(qt01=qt01, qt02=qt02)
#' bubbles(age~year|qname, data=flqs, bub.scale=1)
#' 
#' qt03 <- FLQuant(rnorm(100),dimnames=list(age=as.character(1:10),
#'   year=as.character(1:10)))
#' bubbles(age~year, data=qt03, bub.scale=7, col=c("black","red"), pch=16)
#' 