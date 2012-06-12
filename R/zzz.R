# zzz.R
# FLCore/R/zzz.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id$


.onLoad <- function(lib,pkg) {
  packageStartupMessage("FLCore 2.5.0 development version\n", appendLF = TRUE)
#  cat("---------------------------------------------------------\n")
#  cat("* Note that FLR packages are under constant development,\n") 
#  cat("    please report bugs to flr-team@flr-project.org.\n")
#  cat("* New documentation can be found in vignetes,\n") 
#  cat("    run vignette(package=\"FLCore\") to get a list.\n")
#  cat("* For more information go to http:\\\\flr-project.org or\n")
#  cat("    subscribe the mailing list flr-list@flr-project.org.\n")
#  cat("---------------------------------------------------------\n")
}

# convertFLPar{{{
	setGeneric("convertFLPar", function(object, ...)
		standardGeneric("convertFLPar"))
setMethod('convertFLPar', signature(object='FLModel'),
  function(object)
  {
    params(object) <- convertFLPar(params(object))
    return(object)
  }
)
setMethod('convertFLPar', signature(object='FLPar'),
  function(object)
  {
    dimn <- dimnames(object)
    if(all(names(dimn) == c('iter', 'params')))
      object@.Data <- aperm(object@.Data, c(2,1))
    else if (length(dimn) > 2)
    {
      iter <- grep('iter', names(dimn))
      params <- grep('params', names(dimn))
      idx <- c(params, 3:length(dimn), iter)
      object@.Data <- aperm(object@.Data, idx)
    }
    if(validObject(object))
      return(object)
    else
      stop('FLPar object is still invalid')
  }
)
# }}}

# ac
ac <- function(x, ...)
  as.character(x, ...)

# an
an <- function(x, ...)
  as.numeric(x, ...)

# convertFLModel{{{
	setGeneric("convertFLModel", function(object, ...)
		standardGeneric("convertFLModel"))
setMethod('convertFLModel', signature(object='FLModel'),
  function(object) {

    res <- new(class(object))

    idx <- slotNames(object)
    idx <- idx[idx != "distribution"]
    
    for (i in idx) {
      slot(res, i) <- slot(object, i)
    }

    if(validObject(res))
      return(res)
    else
      stop('FLModel object is still invalid')
  }
)
# }}}
