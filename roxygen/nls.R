#' Method nls
#' 
#' For a given forumla (describing a model) and data this method applies the
#' simple non- linear least squares algorithm - this calculates the parameters
#' that minimisethe sum of squares difference between the observed (data) and
#' predicted (model) values.
#' 
#' The algorithm can be sensitive to the initial values of the problem so do
#' try different start points and check they converge to the same estimates.
#' 
#' 
#' @name nls
#' @aliases
#' nls,FLModel,missing,missing,missing,missing,missing,missing,missing,missing,missing,missing,missing-method
#' nls,FLModel,missing-method nls-FLCore
#' @docType methods
#' @section Generic function:
#' nls(formula,data,start,control,algorithm,trace,subset,weights,na.action,model,lower,upper)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' # An example FLSR (FLModel) object
#' data(nsher)
#' 
#' #set bevholt model
#' model(nsher) <- bevholt
#' 
#' # fit through nls
#' nsher <- nls(nsher)
#' 
#' summary(nsher)
#' 