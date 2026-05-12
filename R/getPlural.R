# getPlural.R - DESC
# getPlural.R

# Copyright 2003-2015 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03
# Notes:


# ANY -> list
#' @rdname getPlural
#' @aliases getPlural,ANY-method
setMethod('getPlural', signature(object='ANY'),
	function(object) {
		return('list')})

# FLQuant -> FLQuants
#' @rdname getPlural
#' @aliases getPlural,FLQuant-method
setMethod('getPlural', signature(object='FLQuant'),
	function(object) {
		return('FLQuants')})

# FLCohort -> FLCohorts
#' @rdname getPlural
#' @aliases getPlural,FLCohort-method
setMethod('getPlural', signature(object='FLCohort'),
	function(object) {
		return('FLCohorts')})

# FLStock -> FLStocks
#' @rdname getPlural
#' @aliases getPlural,FLS-method
setMethod('getPlural', signature(object='FLS'),
	function(object) {
		return('FLStocks')})

# FLIndex -> FLIndices
#' @rdname getPlural
#' @aliases getPlural,FLI-method
setMethod('getPlural', signature(object='FLI'),
	function(object) {
		return('FLIndices')})

# FLBiol -> FLBiols
#' @rdname getPlural
#' @aliases getPlural,FLBiol-method
setMethod('getPlural', signature(object='FLBiol'),
	function(object) {
		return('FLBiols')})

# FLSR -> FLSRs
#' @rdname getPlural
#' @aliases getPlural,FLSR-method
setMethod('getPlural', signature(object='FLSR'),
	function(object) {
		return('FLSRs')})

# FLModelSim -> FLModelSims
#' @rdname getPlural
#' @aliases getPlural,FLModelSim-method
setMethod('getPlural', signature(object='FLModelSim'),
	function(object) {
		return('FLModelSims')})
