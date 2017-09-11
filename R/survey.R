# survey.R - DESC
# FLCore/R/survey.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# cpue {{{

#' cpue, a method to generate an observation of a CPUE index of abundance
#'
#' Description: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#'
#' Details: Aliquam sagittis feugiat felis eget consequat. Praesent eleifend dolor massa, 
#' vitae faucibus justo lacinia a. Cras sed erat et magna pharetra bibendum quis in 
#' mi. Sed sodales mollis arcu, sit amet venenatis lorem fringilla vel. Vivamus vitae 
#' ipsum sem. Donec malesuada purus at libero bibendum accumsan. Donec ipsum sapien, 
#' feugiat blandit arcu in, dapibus dictum felis. 
#'
#' @param object The object on which to draw the observation
#'
#' @return An FLQuant for the index of abundance
#'
#' @name cpue
#' @rdname cpue
#' @aliases cpue cpue-methods
#'
#' @genericMethods
#' 
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
#' @examples
#'
#' data(ple4)
setGeneric("cpue", function(object, ...) standardGeneric("cpue"))

#' @rdname cpue
#' @aliases cpue,FLStock-method
setMethod('cpue',   signature(object='FLStock'),
  function(object, sel=catch(object) %=% 1, effort = c("f","h"), mass = TRUE) {

    # EFFORT from F or HR
    if (effort[1] == "h")
      E <- catch(object) %/% stock.n(object)
    else  
      E <- fbar(object)
    
    cpue <- (catch.n(object) %*% sel) %/% E

    if (mass)
      cpue <- cpue * catch.wt(object)

  return(cpue)
  }
) # }}}

# survey {{{

#' survey, a method to generate an observation of abundance at age
#'
#' Description: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#'
#' Details: Aliquam sagittis feugiat felis eget consequat. Praesent eleifend dolor massa, 
#' vitae faucibus justo lacinia a. Cras sed erat et magna pharetra bibendum quis in 
#' mi. Sed sodales mollis arcu, sit amet venenatis lorem fringilla vel. Vivamus vitae 
#' ipsum sem. Donec malesuada purus at libero bibendum accumsan. Donec ipsum sapien, 
#' feugiat blandit arcu in, dapibus dictum felis. 
#'
#' @param object The object on which to draw the observation
#'
#' @return An FLQuant for the index of abundance
#'
#' @name cpue
#' @rdname cpue
#' @aliases cpue cpue-methods
#'
#' @genericMethods
#' 
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
#' @examples
#'
#' data(ple4)

setGeneric("survey", function(object, ...) standardGeneric("survey"))

setMethod("survey",   signature(object="FLStock"),
  function(object, sel=stock.n(object) %=% 1, timing = 0.5, mass = FALSE) {
  
    # timing MUST BE 0 - 1
    timing <- pmax(pmin(timing, 1.0), 0.0)

    # CORRECT abundnaces for timing
    stock.n <- stock.n(object) * exp(-(harvest(object) * timing - m(object) * timing))
 
    # APPLY survey selectivity
    survey <- stock.n %*% sel

    # SET units as stock.n
    units(survey) <- units(stock.n)
  
    if (mass)
      survey <- survey * stock.wt(object)

    return(survey)
  }
) # }}}

# hyperstability {{{

hyperstability <- function(object, omega=1, ref=yearMeans(object)) {
  return(ref %*% ((object %/% ref) ^ omega))
} # }}}

# bias {{{

bias <- function(object, bias=0.02){
  return(FLQuant(cumprod(1 + rep(c(bias), dim(object)[2])), dimnames=dimnames(object)))
}

biased <- function(object, bias=0.02){
  return(object) * bias(object, bias=bias)
} # }}}
