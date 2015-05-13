#' Calculates the sum of products correction
#' 
#' Calculates the sum of products correction for quantities such a catch,
#' discards, landings.  For example in an object of class
#' \code{\linkS4class{FLStock}} there are slots \emph{catch.n}, \emph{catch.wt}
#' and \emph{catch}. \emph{catch} should equal the products of catch.n*catch.wt
#' summed over ages. This function returns the ratio (i.e. the correction) of
#' \emph{catch.n}*\emph{catch.wt} : \emph{catch}, which can then be used to
#' correct either \emph{catch.n} or \emph{catch.wt}.
#' 
#' Can be used for any class or slot where there are the three FLQuant slots
#' \emph{foo}, \emph{foo.n} and \emph{foo.wt}, representing totals added over
#' all quants (ages), numbers by quant, and weight by quant.
#' 
#' @param stock An FLStock object
#' @param slot Name of the slot group, i.e. "catch", "landings" or "discards"
#' for an FLStock object.
#' @return Returns the ratio as an FLQuant
#' @author FLR Team
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' sop(ple4,"catch")
#' 