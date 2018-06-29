# spread.R - Make list elements available by name inside function.
# FLCore/R/spread.R

# Copyright 2003-2018 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC D02

#' A function to make available list elements inside a function or method
#'
#' Inside a function, a call to spread() will attach to the function environment,
#' sys.frame(), the elements in the list, or of the conversion to list of the
#' object (e.g. named vector or FLPar), so that they be called by name. The function 
#' environment will be deleted once the function returns, so those variables won't
#' make it to the environment from which the function was called, or further up
#' in the call stack.
#'
#' By default, spread() will not overwrite variables in the function environment
#' with the same name as any list element, unles FORCE=TRUE
#'
#' @param object A named list or vector whose elements are to be loaded into the calling environment.
#' @param FORCE Should existing variable with matching names be redefined?
#'
#' @return Invisibly the names of the variables loaded into the calling environment.
#'
#' @name spread
#' @rdname spread
#'
#' @author The FLR Team
#' @seealso \link{sys.nframe}
#' @keywords list
#' @examples
#' 
#' # EXAMPLE function
#' foo <- function (params) {
#'   a <- spread(params)
#'  print(a)
#'  x*y
#'}
#' # x and y are accesible to the internal calculation
#' foo(params=list(x=3.5, y=9))
#' 
#' # Works with FLPar
#' foo(params=FLPar(x=3L, y=0.99238))
#' 
#' # Elements in object must be named
#' \dontrun{foo(list(3, y=0.99238))}
#'
#' # If a variable is missing from the spread object, function will fail
#' \dontrun{foo(list(x=4))}
#' # Unless the variable is already defined in the calling environment,
#' #in this case <environment: R_GlobalEnv>
#' y <- 45
#' foo(params=list(x=4))

spread <- function(object, FORCE=FALSE) {

  object <- as(object, 'list')
  frame <- sys.nframe() - 1

  # CHECK all elements are names
  if(any(nchar(names(object)) < 1))
    stop("all elements in input to spread() must be named")

  # CHECK names are not in use if FORCE=TRUE
  if(!FORCE)
    if(any(names(object) %in% ls(pos=sys.frame(frame))))
      stop("variables exist in frame matching names of object and FORCE=FALSE")

  # ASSIGN elements in object to calling envir
  out <- lapply(names(object), function(e) assign(e, object[[e]],
    pos=sys.frame(frame)))

  # RETURN names of new objects
  invisible(names(object))
}
