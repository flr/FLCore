# uom.R - DESC
# /uom.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# uomTable --
#
# uoms = c(nums, nnums)
#
# 1 --------- nn -----|
# |   nums    | nnums |
# |           |       |
# |           |       |
# nn -------- | ------|

#' Table for conversions and operations between units of measurement
#'
#' - uom defaults to NA unless defined below.
#' - unit +/- itself, returns the same unit (e.g. kg + kg = kg)
#' - numeric unit * 1 returns same unit (e.g. 1e4 * 1 = 1e4)
#' - numeric unit * numeric unit returns product (e.g. 10 * 100 = 1000)
#' - unit / unit returns "" (e.g. 100 / 100 = "")
#' - numeric unit / smaller numeric unit returns division (e.g. 100 / 10 = 10)
#' - 100 times kg returns t
#' - numeric unit * 'kg' returns the product in tonnes (e.g. kg * 1e4 = t * 10)
#' - units with divisions are parsed (e.g. days/boat * boat = days)
#' -
#' -
#' 
#' @docType data
#' @keywords datasets
#' @format An object of class array
#' @name uomTable
#' @rdname uomTable
NULL

# uomTable {{{

uoms <- c(
	'1','10','100','1000','10000','100000','1000000','10000000','100000000', '1000000000',
	'10^0', '10^1', '10^2', '10^3', '10^4', '10^5', '10^6', '10^7', '10^8', '10^9',
	'1e0', '1e1', '1e2', '1e3', '1e4', '1e5', '1e6', '1e7', '1e8', '1e9',
	'1e+00', '1e+01', '1e+02', '1e+03', '1e+04', '1e+05', '1e+06', '1e+07', '1e+08', '1e+09',
	'g', 'kg', 't', '1000 t', 'tonnes', 'thousands', 'm', 'f', 'z', 'hr', 'NA', '',
  'EUR', 'eur', '\u20AC', 'USD', 'usd', '\u0024',
  'd', 'h', 'vessel', 'boat', 'cm')
puoms <- seq(length(uoms))
# numeric units
nums <- c(1:40)
# non-numeric units
nnums <- seq(max(nums) + 1, length(uoms))
# prefered versions of numeric units
snums <- c(1, 2, 3, 4, 25, 26, 27, 28, 29, 30)
# no unit (nu)
nu <- which(uoms == "")
# No. numbers styles
nn <- length(nums) / 10

# NA: not available or unitless, default
uomTable <- array('NA', dimnames=list(op=c('*', '/', '+', '-'),
  e1=uoms, e2=uoms), dim=c(4, length(uoms), length(uoms)))

# numeric unit +/- previous, a + b
for(i in seq(length(uoms))) {
  uomTable['+', i, -i] <- paste(uoms[i], '+', uoms[-i])
  uomTable['-', i, -i] <- paste(uoms[i], '+', uoms[-i])
}

# N +- N = N
diag(uomTable['+',nums,nums]) <- rep(uoms[snums], nn)
diag(uomTable['-',nums,nums]) <- rep(uoms[snums], nn)
diag(uomTable['+',nnums,nnums]) <- uoms[nnums]
diag(uomTable['-',nnums,nnums]) <- uoms[nnums]

# 1 * N = N
uomTable['*', c('1', '1e0', '10^0', '1e+00'), nums] <- rep(rep(uoms[snums], nn), each=nn)
uomTable['*', nums, c('1', '1e0', '10^0', '1e+00')] <- rep(uoms[snums], nn^2)

# N * N = NN TODO Turn into loop
# 10
uomTable['*', c(2, 12, 22, 32), nums[-c(1, 11, 21, 31, 10, 20, 30, 40)]] <- 
	rep(uoms[snums][-c(1,2)], each=nn)
uomTable['*', nums[-c(1, 11, 21, 31, 10, 20, 30, 40)], c(2, 12, 22, 32)] <-
	rep(uoms[snums][-c(1,2)], nn ^ 2)
# 100
uomTable['*', c(3, 13, 23, 33), nums[-c(1, 11, 21, 31, 2, 12, 22, 32, 9, 19,
  29, 39, 10, 20, 30, 40)]] <-  rep(uoms[snums][-c(1,2,3,4)], each=nn)
uomTable['*', nums[-c(1, 11, 21, 31, 2, 12, 22, 32, 9, 19, 29, 39, 10, 20, 30,
  40)], c(3, 13, 23, 33)] <- rep(uoms[snums][-c(1,2,3,4)], nn ^ 2)
# 1000
uomTable['*', c(4, 14, 24, 34), nums[-c(1, 11, 21, 31, 2, 12, 22, 32, 3, 13, 23,
  33, 8, 18, 28, 38, 9, 19, 29, 39, 10, 20, 30, 40)]] <-
    rep(uoms[snums][-c(1,2,3,4,5,6)], each=nn)
uomTable['*', nums[-c(1, 11, 21, 31, 2, 12, 22, 32, 3, 13, 23, 33, 8, 18, 28, 38,
  9, 19, 29, 39, 10, 20, 30, 40)], c(4, 14, 24, 34)] <- 
	rep(uoms[snums][-c(1,2,3,4,5,6)], nn ^ 2)
# 1e4
uomTable['*', c(5, 15, 25, 35), nums[-c(1, 11, 21, 31, 2, 12, 22, 32, 3, 13, 23,
  33, 4, 14, 24, 34, 7, 17, 27, 37, 8, 18, 28, 38, 9, 19, 29, 39, 10, 20, 30, 40)]] <- 
	rep(uoms[snums][-c(1,2,3,4,5,6,7,8)], each=nn)
uomTable['*', nums[-c(1, 11, 21, 31, 2, 12, 22, 32, 3, 13, 23, 33, 8, 18, 28, 38,
  9, 19, 29, 39, 10, 20, 30, 40)], c(4, 14, 24, 34)] <- 
	rep(uoms[snums][-c(1,2,3,4,5,6)], nn ^ 2)
# 1e5
uomTable['*', c(6, 16, 26, 36), nums[-c(1, 11, 21, 31, 2, 12, 22, 32, 3, 13, 23, 33,
  4, 14, 24, 34, 6, 16, 26, 36, 7, 17, 27, 37, 8, 18, 28, 38, 9, 19, 29, 39,
  10, 20, 30, 40)]] <- rep(uoms[snums][-c(1,2,3,4,5,6,7,8,9)], each=nn)
uomTable['*', nums[-c(1, 11, 21, 31, 2, 12, 22, 32, 3, 13, 23, 33, 4, 14, 24, 34,
  6, 16, 26, 36, 7, 17, 27, 37, 8, 18, 28, 38, 9, 19, 29, 39, 10, 20, 30, 40)],
  c(6, 16, 26, 36)] <- rep(uoms[snums][-c(1,2,3,4,5,6,7,8,9)], nn ^ 2)

# U / U = ""
diag(uomTable['/', puoms, puoms]) <- ""

# Equivalent numbers
diag(uomTable['/', 1:10, 11:20]) <- ""
diag(uomTable['/', 1:10, 21:30]) <- ""
diag(uomTable['/', 1:10, 31:40]) <- ""
diag(uomTable['/', 11:20, 1:10]) <- ""
diag(uomTable['/', 11:20, 21:30]) <- ""
diag(uomTable['/', 11:20, 31:40]) <- ""
diag(uomTable['/', 21:30, 1:10]) <- ""
diag(uomTable['/', 21:30, 11:20]) <- ""
diag(uomTable['/', 21:30, 31:40]) <- ""
diag(uomTable['/', 31:40, 1:10]) <- ""
diag(uomTable['/', 31:40, 11:20]) <- ""
diag(uomTable['/', 31:40, 21:30]) <- ""

# 1000 / 100 = 10
for(i in c(0, 10, 20, 30))
  for(j in c(0, 10, 20, 30))
    diag(uomTable['/', i + (3:10), j + (2:9)]) <- "10"

# 1000 / 10 = 100
for(i in c(0, 10, 20, 30))
  for(j in c(0, 10, 20, 30))
    diag(uomTable['/', i + (4:10), j + (2:8)]) <- "100"

# 10000 / 10 = 1000
for(i in c(0, 10, 20, 30))
  for(j in c(0, 10, 20, 30))
    diag(uomTable['/', i + (5:10), j + (2:7)]) <- "1000"

# U / 1 = U
uomTable['/', puoms, c('1', '10^0', '1e0')] <- uoms[puoms]

# NA /*+- NA = NA
uomTable[,'NA', 'NA'] <- 'NA'

# "" /*+- "" = ""
uomTable[,nu, nu] <- ""

# U */ "" = U
uomTable[c('/', '*'), puoms, nu] <- uoms

# "" */ U = U
uomTable[c('/', '*'), nu, puoms] <- uoms

# g * 1000 = kg
uomTable['*', 'g', c('1000', '1e3', '10^3')] <- 'kg'
uomTable['*', c('1000', '1e3', '10^3'), 'g'] <- 'kg'

# kg * 1000 = t
uomTable['*', 'kg', c('1000', '1e3', '10^3')] <- 't'
uomTable['*', c('1000', '1e3', '10^3'), 'kg'] <- 't'

# kg / 1000 = g
uomTable['/', 'kg', c('1000', '1e3', '10^3')] <- 'g'

# t / 1000 = kg
uomTable['/', 't', c('1000', '1e3', '10^3')] <- 'kg'

# kg * 100 = kg*100
uomTable['*', 'kg', c('100', '1e2', '10^2')] <- 'kg * 100'
uomTable['*', c('100', '1e2', '10^2'), 'kg'] <- 'kg * 100'

# kg * 10 = kg*10
uomTable['*', 'kg', c('10', '1e1', '10^1')] <- 'kg * 10'
uomTable['*', c('10', '1e1', '10^1'), 'kg'] <- 'kg * 10'

# kg * 1 = kg
uomTable['*', 'kg', c('1', '1e0', '10^0')] <- 'kg'
uomTable['*', c('1', '1e0', '10^0'), 'kg'] <- 'kg'

# kg * Numbers = t * Numbers/1000
uomTable['*', 'kg', c('10000', '1e4', '10^4', '1e+04')] <- 't * 10'
uomTable['*', c('10000', '1e4', '10^4', '1e+04'), 'kg'] <- 't * 10'
uomTable['*', 'kg', c('100000', '1e5', '10^5', '1e+05')] <- 't * 100'
uomTable['*', c('100000', '1e5', '10^5', '1e+05'), 'kg'] <- 't * 100'
uomTable['*', 'kg', c('1000000', '1e6', '10^6', '1e+06')] <- 't * 1000'
uomTable['*', c('1000000', '1e6', '10^6', '1e+06'), 'kg'] <- 't * 1000'
uomTable['*', 'kg', c('10000000', '1e7', '10^7', '1e+07')] <- 't * 1e4'
uomTable['*', c('10000000', '1e7', '10^7', '1e+07'), 'kg'] <- 't * 1e4'
uomTable['*', 'kg', c('100000000', '1e8', '10^8', '1e+08')] <- 't * 1e5'
uomTable['*', c('100000000', '1e8', '10^8', '1e+08'), 'kg'] <- 't * 1e5'
uomTable['*', 'kg', c('1000000000', '1e9', '10^9', '1e+09')] <- 't * 1e6'
uomTable['*', c('1000000000', '1e9', '10^9', '1e+09'), 'kg'] <- 't * 1e6'

# t * 1000 = Mt
uomTable['*', 't', c('1000', '1e3', '10^3', '1e+03')] <- '1000 t'
uomTable['*', c('1000', '1e3', '10^3', '1e+03'), 't'] <- '1000 t'

uomTable['*', c('1000000', '1e6', '10^6', '1e+06'), 'kg'] <- '1000 t'
uomTable['*', 'kg', c('1000000', '1e6', '10^6', '1e+06')] <- '1000 t'

uomTable['*', 't', c('100', '1e2', '10^2', '1e+02')] <- 't * 100'
uomTable['*', c('100', '1e1', '10^2', '1e+02'), 't'] <- 't * 100'

uomTable['*', 't', c('10', '1e1', '10^1', '1e+01')] <- 't * 10'
uomTable['*', c('10', '1e1', '10^1', '1e+01'), 't'] <- 't * 10'

# Mt / 1000 = t
# DEBUG
uomTable['/', '1000 t', c('1000', '1e3', '10^3', '1e+03')] <- 't'
uomTable['/', '1000 t', c('1000000', '1e6', '10^6', '1e+06')] <- 'kg'

# tonnes / 1000 = kg
uomTable['/', 'tonnes', c('1000', '1e3', '10^3', '1e+03', 'thousands')] <- 'kg'
uomTable['/', 'tonnes', c('1000', '1e3', '10^3', '1e+03', 'thousands')] <- 'kg'
	
# U */ "" = U
uomTable[c('*','/'), nu, nums] <- rep(rep(uoms[snums], each=2), nn)
uomTable[c('*','/'), nums, nu] <- rep(rep(uoms[snums], each=2), nn)
uomTable[c('*','/'), nnums, nu] <- rep(uoms[nnums], each=2)
uomTable[c('*','/'), nu, nnums] <- rep(uoms[nnums], each=2)

# z, m, f = 1/timestep
uomTable['+', 'f', 'm'] <- 'z'
uomTable['+', 'm', 'f'] <- 'z'
uomTable['-', 'z', 'f'] <- 'm'
uomTable['-', 'z', 'm'] <- 'f'

# m,f,z
uomTable['/', c('z','f','m'), c('z','f','m')] <- 'NA'
uomTable[c('*', '/'), nums, 'f'] <- rep(rep(uoms[snums], each=2), nn)
uomTable[c('*', '/'), nums, 'm'] <- rep(rep(uoms[snums], each=2), nn)
uomTable[c('*', '/'), nums, 'z'] <- rep(rep(uoms[snums], each=2), nn)
uomTable[c('*', '/'), nums, 'hr'] <- rep(rep(uoms[snums], each=2), nn)
uomTable[c('*', '/'), 'f', nums] <- rep(rep(uoms[snums], each=2), nn)
uomTable[c('*', '/'), 'm', nums] <- rep(rep(uoms[snums], each=2), nn)
uomTable[c('*', '/'), 'z', nums] <- rep(rep(uoms[snums], each=2), nn)
uomTable[c('*', '/'), 'hr', nums] <- rep(rep(uoms[snums], each=2), nn)

# EUR * 1000 = 1000 EUR
uomTable['*', c('EUR', 'eur', '\u20AC'), c('1000', '1e3', '10^3', '1e+03')] <- '1000 EUR'
uomTable['*', c('1000', '1e3', '10^3', '1e+03'), c('EUR', 'eur', '\u20AC')] <- '1000 EUR'

# USD * 1000 = 1000 USD
uomTable['*', c('USD', 'usd', '\u0024'), c('1000', '1e3', '10^3', '1e+03')] <- '1000 USD'
uomTable['*', c('1000', '1e3', '10^3', '1e+03'), c('USD', 'usd', '\u0024')] <- '1000 USD'

# TODO Check if this is OK
uomTable['+', 'NA',] <- uoms
uomTable['+', , 'NA'] <- uoms
uomTable['-', 'NA',] <- uoms
uomTable['-', , 'NA'] <- uoms

# thousands
uomTable['*', 'thousands', c('1', '10^0', '1e0', '1e+00')] <- 'thousands'
uomTable[c('+','-'), 'thousands', 'thousands'] <- 'thousands'
uomTable['*', 'thousands', 'thousands'] <- '1e6'
uomTable['/', 'thousands', 'thousands'] <- ''
uomTable['*', 'thousands', 'kg'] <- 'tonnes'
uomTable['*', 'kg', 'thousands'] <- 'tonnes'

# }}}

# uom {{{

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
#' @return \code{uom} returns a string with the corresponding units of measurement, or a character vector, showing the operation carried out, when units are not known to \code{uom} or not compatible, e.g. "100 * d".
#' @name uom
#' @aliases uom
#' @rdname uom
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
	
  # BUG power
  if(op == "^")
		return(sprintf("%s %s %s", u1, op, u2))

  # REMOVE trailing and leading spaces and ENSURE string
  u <- trimws(c(u1, u2))
	idx <- match(u, uoms)

    # PARSE and SOLVE if '/' in u and op = '*'
  if(any(grepl("/", u)) && op == "*") {
    
    # FIND u with more parenthesis
    mtcs <- lengths(regmatches(u, gregexpr("/", u)))
    pos <- which(mtcs == max(mtcs))

    # FIND position of '/'
    sla <- unlist(gregexpr(pattern = "/", u[pos]))[mtcs[pos] - mtcs[-pos]]
    # EXTRACT denominator
    den <- gsub(" ", "", substr(u[pos], sla[1] + 1, nchar(u[pos])))
    # EXTRACT numerator
    num <- gsub("[[:space:]]*$", "", substr(u[pos], 1, sla[1] - 1))
    # IF u2 is in u1 AND right hand side equal u1,
    if(identical(den, u[-pos]))
      # RETURN left hand side
      return(num)
  }

  # PARSE and SOLVE number products
  if(any(grepl("*", u, fixed=TRUE)) && op == "*") {
    us <- unlist(lapply(u, function(x) gsub("[[:space:]]", "",
      unlist(strsplit(x, "*", fixed=TRUE)))))
    idp <- us %in% uoms[nums]
    # FIX for as.numeric not handling 10^*
    val <- prod(as.numeric(uom('+', us[idp], us[idp])))
    # RETURN
    return(uom(op, as.character(val), us[!idp]))
  }

  # undefined unit (not in uoms)
	if(any(is.na(idx))) {
	  return(sprintf("%s %s %s", u1, op, u2))
  }

  # use uomTable
	res <- uomTable[op, idx[1], idx[2]]
	
	return(res)
}

uom <- compiler::cmpfun(uom)
# }}}

# uomUnits {{{

#' @rdname uom
#' @aliases uomUnits
#' @details The list of units known to \code{uom} is stored internally but can be queried by calling \code{uomUnits()} with no arguments. If a character vector is provided, a logical is returned telling whether the string is included or not in that table.
#' @param unit A character vector for one or more units to be compared with those known to \code{uom}.
#' @return \code{uomUnits} returns TRUE or FALSE if \code{unit} is given, otherwise a character vector with all units known to \code{uom}.
#' @examples
#'
#' # Check if units are known
#' uomUnits('kg')
#' uomUnits('kell')

uomUnits <- function(unit=missing) {

  if(missing(unit))
    return(dimnames(uomTable)$e1)
  
  return(unit %in% dimnames(uomTable)$e1)
} # }}}
