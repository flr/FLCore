#' Method expand
#'
#' Expand FLR objects using named dimensions
#' 
#' Expansion of FLR objects can be carried out with dimension names by using
#' \code{expand}. A number of dimension names and selected dimensions are
#' passed to the method, and those are used to expand the input object.
#'
#' The behaviour of this method differs, depending on whether the current length
#' of the dimension to be expanded is 1 or greater than 1.
#'
#' If the current length of the dimension to be expanded is 1, then the method
#' will rename the current dimname to the newly specifed ones, regardless of
#' whether the new dimnames include the current one or not, and the method will
#' copy across the current contents to the newly-expanded areas.
#'
#' If the current length of the dimension to be expanded is greater than 1,
#' and the length of the intended expansion is greater than the current length,
#' then the expanded dimnames needs to include the current ones, and the method
#' inserts \code{NA} into the newly-expanded areas. However, if the length of
#' the intended "expansion" is the same as the current length, and the
#' "expanded" dimnames completely exclude the current dimnames, then the method
#' simply replaces the current dimnames with the newly specifed ones without
#' changing the contents of the object.
#'
#' @name expand
#' @aliases expand expand-methods expand,FLArray-method expand,FLQuant-method
#' expand,FLComp-method expand,FLStock-method
#' @docType methods
#' @section Generic function: expand(x)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#'
#' # Expanding FLStock objects
#'   expand(ple4, year=1957:2013)
#'   expand(ple4, iter=1:10)
#'   expand(ple4, season=1:2)
#'
#' # Expanding along the season dimension for an FLQuant object
#'   # Expand object to four seasons
#'     catch_s <- expand(catch(ple4), season=1:4)
#'     dimnames(catch_s)
#'   # Now divide the total catch across seasons either equally...
#'     catch_seasons <- catch_s/4
#'   # ...or with different proportions by season
#'     catch_seasons <- catch_s %*% FLQuant(c(0.25, 0.2, 0.15, 0.40),
#'       dimnames=list(age='all', season=1:4), units="")
#'