# uom.R - DESC
# uom.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# uom {{{

uoms <- c(
	'1','10','100','1000','10000','100000','1000000','10000000','100000000', '1000000000',
	'10^0', '10^1', '10^2', '10^3', '10^4', '10^5', '10^6', '10^7', '10^8', '10^9',
	'1e0', '1e1', '1e2', '1e3', '1e4', '1e5', '1e6', '1e7', '1e8', '1e9',
	'kg', 't', 'm', 'f', 'z', 'hr', 'prop', 'NA')
nums <- c(1:30)
snums <- c(1,2,3,4,25,26,27,28,29,30)

uomTable <- array('NA', dimnames=list(op=c('*', '/', '+', '-'), e1=uoms, e2=uoms), dim=c(4, length(uoms), length(uoms)))

# NUMBERS

# N +- N = N
diag(uomTable['+',nums,nums]) <- rep(uoms[snums], 3)
diag(uomTable['-',nums,nums]) <- rep(uoms[snums], 3)

# 1 * N = N
uomTable['*', c('1', '1e0', '10^0'), nums] <- rep(rep(uoms[snums], 3), each=3)
uomTable['*', nums, c('1', '1e0', '10^0')] <- rep(uoms[snums], 9)

# N * N = NN
# 10
uomTable['*', c(2, 12, 22), nums[-c(1, 11, 21, 10, 20, 30)]] <- 
	rep(uoms[snums][-c(1,2)], each=3)
uomTable['*', nums[-c(1, 11, 21, 10, 20, 30)], c(2, 12, 22)] <-
	rep(uoms[snums][-c(1,2)], 9)
# 100
uomTable['*', c(3, 13, 23), nums[-c(1, 11, 21, 2, 12, 22, 9, 19, 29, 10, 20, 30)]] <- 
	rep(uoms[snums][-c(1,2,3,4)], each=3)
uomTable['*', nums[-c(1, 11, 21, 2, 12, 22, 9, 19, 29, 10, 20, 30)], c(3, 13, 23)] <- 
	rep(uoms[snums][-c(1,2,3,4)], 9)



# NN / N = N

# *
# kg * 1000 = t
uomTable['*', 'kg', c('1000', '1e3', '10^3')] <- 't'
uomTable['*', c('1000', '1e3', '10^3'), 'kg'] <- 't'

# kg * 100 = kg*100
uomTable['*', 'kg', c('100', '1e2', '10^2')] <- 'kg*100'
uomTable['*', c('100', '1e2', '10^2'), 'kg'] <- 'kg*100'

# kg * 10 = kg*10
uomTable['*', 'kg', c('10', '1e1', '10^1')] <- 'kg*10'
uomTable['*', c('10', '1e1', '10^1'), 'kg'] <- 'kg*10'

# kg * Numbers = t * Numbers/1000
uomTable['*', 'kg', c('10000', '1e4', '10^4')] <- 't*10'
uomTable['*', c('10000', '1e4', '10^4'), 'kg'] <- 't*10'
uomTable['*', 'kg', c('100000', '1e5', '10^5')] <- 't*100'
uomTable['*', c('100000', '1e5', '10^5'), 'kg'] <- 't*100'
uomTable['*', 'kg', c('1000000', '1e6', '10^6')] <- 't*1000'
uomTable['*', c('1000000', '1e6', '10^6'), 'kg'] <- 't*1000'
uomTable['*', 'kg', c('10000000', '1e7', '10^7')] <- 't*1e4'
uomTable['*', c('10000000', '1e7', '10^7'), 'kg'] <- 't*1e4'
uomTable['*', 'kg', c('100000000', '1e8', '10^8')] <- 't*1e5'
uomTable['*', c('100000000', '1e8', '10^8'), 'kg'] <- 't*1e5'

# NA
uomTable[, 'NA', ] <- rep(uoms, each=4)
uomTable[, , 'NA'] <- rep(uoms, each=4)

# prop
uomTable[, 'prop', ] <- rep(uoms, each=4)
uomTable[, , 'prop'] <- rep(uoms, each=4)

# prop NA
uomTable[, 'prop', 'NA'] <- 'prop'
uomTable[, 'NA', 'prop'] <- 'prop'

# z, m, f = 1/timestep
uomTable['+', 'f', 'm'] <- 'z'
uomTable['+', 'm', 'f'] <- 'z'
uomTable['-', 'z', 'f'] <- 'm'
uomTable['-', 'z', 'm'] <- 'f'
uomTable['/', c('z','f','m'), c('z','f','m')] <- 'prop'

uomTable[c('*', '/'), nums, 'f'] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), nums, 'm'] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), nums, 'z'] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), nums, 'hr'] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), 'f', nums] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), 'm', nums] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), 'z', nums] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), 'hr', nums] <- rep(rep(uoms[snums], each=2), 3)

# /
# kg * 1000 = t
uomTable['/', 't', c('1000', '1e3', '10^3')] <- 'kg'

# +, -
uomTable[c('+','-'), 'kg', 'kg'] <- 'kg'
uomTable[c('+','-'), 't', 't'] <- 't'
uomTable[c('+','-'), 'm', 'm'] <- 'm'
uomTable[c('+','-'), 'f', 'f'] <- 'f'
uomTable[c('+','-'), 'z', 'z'] <- 'z'
uomTable[c('+','-'), 'hr', 'hr'] <- 'hr'

#' uom Units of Measurement
#' 
#' The 'units' attribute of FLQuant objects provides a mechanism for keeping track
#' of the units of measurement of that particular piece of data. 
#' 
#' Arithmetic operators for 'FLQuant' objects are aware of a limited set of units
#' of measurement and will output the right unit when two object are arithmetically
#' combined. For example, the product of object with units of 'kg' and '1000' will
#' output an object with 'units' of 't' (for metric tonnes).
#'
#' Operations involving combinations of units not defined will issue a warning, and
#' the resulting 'units' attribute will simply keep a string indicating the input
#' units of measurement and the operation carried out, as in '10 * 1000'.
#'
#' Note that no scaling or modification of the values in the object takes place.
#'
#' Conversion across units is carried out by the \code{uom()} function
#'
#' @param op The arithmetic operator to be used, one of '+', '-', '*' or '/'
#' @param u1 The units of measurement string of the first object
#' @param u2 The units of measurement string of the second object
#' @return a string with the corresponding units of measurement, a string such as '10 *100' when not compatible
#' 
#' @section Recognized Units:
#' The following units of measurement are recognized by the 'Arith' operators
#' (+, -, * /).
#' \describe{
#'    \item{Weight}{'kg', 't'}
#'    \item{Numbers}{1 - 100000000, 1e0 - 1e8, 10^0 - 10^8}
#'    \item{Mortality}{'m', 'f', 'z', 'hr'}
#'    \item{Other}{'prop', 'NA'}
#' }
#'
#' @name uom
#' @aliases uom
#' @docType methods
#' @author The FLR Team
#' @seealso \code{\linkS4class{FLQuant}} \code{\link{units,FLArray-method}}
#' @keywords function
#' @examples
#'
#' # Conversion between weights
#' FLQuant(1, units='kg') * FLQuant(1000, units='1')
#'
#' # Conversion between mortalities
#' FLQuant(0.2, units='m') + FLQuant(0.34, units='f')

uom <- function(op, u1, u2) {

	u <- c(u1, u2)
	
	# max length of string, max(nchar(FLCore:::uoms))
	if(any(nchar(u) > 10))
		return(sprintf("%s %s %s", u1, op, u2))

	# ""
	if(!all(nzchar(u)))
		return(sprintf("%s %s %s", u1, op, u2))

	idx <- match(u, FLCore:::uoms)

	# undefined unit
	if(any(is.na(idx)))
		return(sprintf("%s %s %s", u1, op, u2))

	# use uomTable
	res <- FLCore:::uomTable[op, idx[1], idx[2]]
	
	# incompatible units ('NA')
	if(res == 'NA') {
		warning('incompatible units of measurements in FLQuant objects: ',
			sprintf("%s %s %s", u1, op, u2))
		
		return(sprintf("%s %s %s", u1, op, u2))
	}
	return(res)
}
# }}}
