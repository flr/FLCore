#' Method SRModelName
#'
#' Convenience function to identify an SR model by its formula
#' 
#' A supplied formula, representing a stock-recruitment relationship, is
#' matched against the list of all models defined in \code{FLCore} (See
#' \link{SRModels}).
#' 
#' If a match is found, a string character with the name of the model is
#' returned, otherwise \code{FALSE} is obtained.
#'
#' @param model A formula defining the model
#' @return \item{name}{A character string or NULL}
#' @author FLR Team
#' @seealso \code{\link{SRModels}}
#' @keywords models utilities
#' @examples
#' 
#' SRModelName(rec ~ a * ssb * exp(-b * ssb))
#' 