#' Stock-recruitment model function
#' 
#' The \code{sr()} function acts as a front end to the various functions
#' available that implement fitting mechanisms for various stock/recruitment
#' models.  \code{\link{fmle}} is called if a likelihood function is present in
#' the \emph{logl} slot, otherwise \code{\link{nls}} is used instead.
#' 
#' 
#' @param sr An FLSR object.
#' @param \dots Other parameters, depending on the model selected.
#' @return An object of class \code{\linkS4class{FLSR}}
#' @author The FLR Team
#' @seealso \code{\linkS4class{FLSR}}
#' @keywords models