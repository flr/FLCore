#' Method trim
#'
#' Trim FLR objects using named dimensions
#' 
#' Subsetting of FLR objects can be carried out with dimension names by using
#' \code{trim}. A number of dimension names and selected dimensions are passed
#' to the method and those are used to subset the input object.
#' 
#' Exceptions are made for those classes where certain slots might differ in
#' one or more dimensions. If trim is applied to an FLQuant object of length 1
#' in its first dimension and with dimension name equal to 'all', values to
#' \code{trim} specified for that dimension will be ignored. For example,
#' \code{\link{FLStock}} objects contain slots with length=1 in their first
#' dimension. Specifying values to trim over the first dimension will have no
#' effect on those slots (\code{catch}, \code{landings}, \code{discards}, and
#' \code{stock}). Calculations might need to be carried out to recalculate
#' those slots (e.g. using \code{computeCatch}, \code{computeLandings},
#' \code{computeDiscards} and \code{computeStock}) if their quant-structured
#' counterparts are modified along the first dimension.
#'
#' @name trim
#' @aliases trim trim-methods trim,FLArray-method trim,FLQuant-method
#' trim,FLComp-method trim,FLStock-method trim,FLCohort-method
#' trim,FLIndex-method
#' @docType methods
#' @section Generic function: trim(x)
#' @author The FLR Team
#' @seealso \linkS4class{FLQuant}, \linkS4class{FLStock},
#' \linkS4class{FLCohort}, \linkS4class{FLIndex}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' 
#' trim(catch(ple4), year=1990:1995)
#' # which is equivalent to
#'   window(catch(ple4), start=1990, end=1995)
#'
#' trim(catch.n(ple4), year=1990:1995, age=1:2)
#' 
#' # Now on an FLStock
#'   summary(trim(ple4, year=1990:1995))
#' 
#' # If 'age' is trimmed in ple4, catch, landings and discards need to be
#' # recalculated
#'   shpl4 <- trim(ple4, age=1:4)
#'   landings(shpl4) <- computeLandings(shpl4)
#'   discards(shpl4) <- computeDiscards(shpl4)
#'   catch(shpl4) <- computeCatch(shpl4)
#'   summary(shpl4)
#' 