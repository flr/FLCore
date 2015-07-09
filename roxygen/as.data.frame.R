#' Method as.data.frame
#' 
#' This method converts an \code{FLQuant} or any other FLR object composed of
#' FLQuants into a \link[base]{data.frame}.
#' 
#' For a single \code{\link{FLQuant}}, the \code{data.frame} returned has 7
#' columns: \code{quant}, \code{year}, \code{unit}, \code{season}, \code{area},
#' \code{iter} and \code{data}.  The last column contains the actual values
#' stored in the original object, while the first six contain the corresponding
#' dimensions. The \code{quant}, \code{year} and \code{data} columns are of
#' class \link[base]{numeric}, while the other four columns are of class
#' \link[base]{factor}.
#' 
#' When converting an \code{\linkS4class{FLCohort}} object, the \code{year}
#' column is substituted by \code{cohort}.
#' 
#' The \code{data.frame} returned for complex objects, i.e. those that inherit
#' from class \code{\link{FLComp}}, has an extra column, \code{slot}, that
#' holds the name of the slot in the original object.
#' 
#' The data.frame obtained from a \code{\linkS4class{FLQuants}} object also
#' has an extra column, named \code{qname}, that refers to the name of each
#' \code{FLQuant} object in the list.  This column is named \code{cname} when
#' an \code{\linkS4class{FLCohorts}} object is converted.
#' 
#' Objects of class \code{\linkS4class{FLQuants}} can also be converted into a
#' wide-format table, where data from the list elements are placed in separate
#' colums, using \code{\link{model.frame,FLlst-method}}.
#' 
#' 
#' @name as.data.frame
#' @aliases as.data.frame-FLCore coerce,FLArray,data.frame-method
#' as.data.frame,FLArray,missing,missing-method
#' as.data.frame,FLQuant,missing,missing-method
#' as.data.frame,FLCohort,missing,missing-method
#' as.data.frame,FLComp,missing,missing-method
#' as.data.frame,FLQuants,missing,missing-method
#' as.data.frame,FLCohorts,missing,missing-method
#' as.data.frame,FLPar,ANY,ANY-method as.data.frame,FLCohort,ANY,ANY-method
#' @docType methods
#' @section Generic function: as.data.frame(x, row.names, optional)
#' @author The FLR Team
#' @seealso \link[base]{as.data.frame}, \link[stats]{model.frame},
#' \link{model.frame,FLlst-method}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' # An FLQuant object
#'   fdf <- as.data.frame(catch.n(ple4))
#'   head(fdf)
#'   summary(fdf)
#'
#' # A more complex FLStock object that inherits from class FLComp
#'   sdf <- as.data.frame(ple4)
#'   head(sdf)
#'
#' # An FLQuants object
#'   fqs <- FLQuants(ssb=ssb(ple4), catch=catch(ple4), rec=rec(ple4),
#'     f=fbar(ple4))
#'   sdf2 <- as.data.frame(fqs)
#'   head(sdf2)
#'
#'   # Using model.frame to obtain a wide-format
#'     sdf3 <- model.frame(fqs)
#'     head(sdf3)
#'