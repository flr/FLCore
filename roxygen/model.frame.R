#' Method model.frame
#' 
#' \code{model.frame} returns a \code{\link[base]{data.frame}} with the
#' variables in a wide format, to be used by a \code{formula} in any model
#' method.
#' 
#' 
#' @name model.frame
#' @aliases model.frame,FLlst-method model.frame,FLComp-method
#' @docType methods
#' @section Generic function: model.frame(formula)
#' @author The FLR Team
#' @seealso \link[stats]{model.frame}, \linkS4class{FLQuants},
#' \linkS4class{FLlst}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' flqs <- FLQuants(stock=stock.n(ple4), catch=catch.n(ple4))
#' fmf <- model.frame(flqs)
#' head(fmf)