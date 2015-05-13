#' Catch-curves plot
#' 
#' Catch-curves are essential to explore the mortality carried out on a stock.
#' It shows the trends on different cohorts by age.
#' 
#' 
#' @name ccplot
#' @aliases ccplot ccplot-methods ccplot,formula,FLCohort-method
#' @docType methods
#' @section Generic function: ccplot(x,data)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' ccplot(data~age, data=FLCohort(ple4@catch.n), type="l")
#' 