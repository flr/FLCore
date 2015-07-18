#' Method Arith
#'
#' Arithmetic methods for FLQuant objects
#'
#' The \code{Arith} group of methods, comprising addition, substraction,
#' product, division, exponentiation, modulus and integer division (\code{+},
#' \code{-},\code{*}, \code{/}, \code{^}, \code{%%} and \code{%/%}).
#' These methods work exactly as in an object of class \code{array}, but always
#' return an \code{FLQuant} object.
#'
#' @name Arith
#' @aliases Arith,FLArray,FLArray-method Arith,numeric,FLArray-method
#' Arith,FLArray,numeric-method Arith,FLQuant,FLQuant-method
#' Arith,FLCohort,FLCohort-method Arith,FLQuants,FLQuants-method
#' @docType methods
#' @section Generic function: Arith(e1,e2)
#' @author The FLR Team
#' @seealso \code{\link[base]{Arithmetic}}, \code{\link[methods]{Arith}}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(10), dim=c(2,5))
#' fl2 <- FLQuant(2, dim=c(2,5))
#' flq*fl2
#' flq/fl2
#'