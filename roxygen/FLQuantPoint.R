#' Class FLQuantPoint
#' 
#' The \code{FLQuantPoint} class summarizes the contents of an \code{FLQuant}
#' object with multiple iterations along its sixth dimension using a number of
#' descriptive statistics.
#' 
#' An object of this class has a set structure along its sixth dimension
#' (\emph{iter}), which will always be of length 5, and with dimnames
#' \emph{mean}, \emph{median}, \emph{var}, \emph{uppq} and \emph{lowq}. They
#' refer, respectively, to the sample mean, sample median, variance, and lower
#' (0.25) and upper (0.75) quantiles.
#' 
#' Objects of this class will be typically created from an \code{FLQuant}. The
#' various statistics are calculated along the \emph{iter} dimension of the
#' original \code{FLQuant} using \code{\link[base]{apply}}.
#' 
#' @name FLQuantPoint
#' @aliases FLQuantPoint FLQuantPoint,FLQuant-method FLQuantPoint-class
#' FLQuantPoint-methods
#' @docType class
#' @section Slots: \describe{ \item{.Data}{The main array holding the computed
#' statistics. \code{array}.} \item{units}{Units of measurement.
#' \code{character}.} }
#' @author The FLR Team
#' @seealso \link{FLQuant}
#' @keywords classes
#' @examples
#' 
#' # Create an FLQuant object with 200 iters
#'   flq <- FLQuant(rnorm(40000), dim=c(10,20,1,1,1,200))
#'
#' # Create a corresponding FLQuantPoint
#'   flqp <- FLQuantPoint(flq)
#'   summary(flqp)
#'
#'   # Output the "mean" and "var" iters
#'     mean(flqp)
#'     var(flqp)
#'
#' # The following assumes the distribution behind the object is normal, and
#' # creates an FLQuant with 200 iters using the "mean" and "var" iters as its
#' # mean and variance
#'   rnorm(200, flqp)
#' 