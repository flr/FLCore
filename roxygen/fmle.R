#' Method fmle
#'
#' Method for maximum likelihood estimation
#'
#' The \code{fmle} method fits the model specified in an \code{FLModel} object
#' using maximum likelihood estimation (MLE) by minimizing the negative of the
#' log-likelihood function given in the \code{logl} slot, through calls to the
#' \code{\link[stats]{optim}} minimizaton routine.
#' 
#' For a given model and log-likelihood function, the \code{fmle} method will
#' use the \code{optim} function in R to calculate the parameter vector which
#' maximises the log-likelihood (and, hence, the likelihood function), and
#' therefore provides the optimum parameter values for the given problem and
#' data.
#' 
#' Be advised that for non-informative or conflicting data, the maximum
#' likelihood estimate can be highly dependent on initial values, and if the
#' optimiser is given poor initial estimates, it may converge falsely. Always
#' try multiple start points to be assured that the true MLE solution is found.
#'
#' @name fmle
#' @aliases fmle fmle,ANY,missing-method fmle,FLModel,ANY-method
#' fmle,FLModel,FLPar-method fmle,FLModel-method
#' @docType methods
#' @section Generic function: fmle(object,start)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' # use an example FLModel object
#'   data(nsher)
#'   summary(nsher)
#' 
#' # inspect the logl function...
#'   logl(nsher)
#' # ...and the function providing initial values to the optimizer
#'   initial(nsher)
#' 
#' # lower and upper limits for the parameters are set, and used if method
#' # 'L-BFGS-B' is used in the call to optim, as is default in fmle
#'   lower(nsher)
#'   upper(nsher)
#' 
#' # fit it with fmle
#'   nsher <- fmle(nsher)
#' 
#' # fixed values can be chosen for any parameter...
#'   nsher_fixed_a <- fmle(nsher, fixed=list(a=125))
#' # ...and results compared, for example using AIC
#'   AIC(nsher)
#'   AIC(nsher_fixed_a)
#'