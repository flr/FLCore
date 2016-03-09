# getPlural.R - DESC
# FLCore/R/getPlural.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

#' FLCore-internal
#' 
#' FLCore-internal
#'
#' @aliases ac convert6d convertFLPar convertFLPar-methods
#' convertFLPar,FLPar-method convertFLPar,FLModel-method getPlural
#' @author The FLR Team
#' @keywords methods
#'

# ANY -> list
setMethod('getPlural', signature(object='ANY'),
	function(object) {
		return('list')})

# FLQuant -> FLQuants
setMethod('getPlural', signature(object='FLQuant'),
	function(object) {
		return('FLQuants')})

# FLCohort -> FLCohorts
setMethod('getPlural', signature(object='FLCohort'),
	function(object) {
		return('FLCohorts')})

# FLStock -> FLStocks
setMethod('getPlural', signature(object='FLStock'),
	function(object) {
		return('FLStocks')})

# FLIndex -> FLIndices
setMethod('getPlural', signature(object='FLIndex'),
	function(object) {
		return('FLIndices')})

# FLBiol -> FLBiols
setMethod('getPlural', signature(object='FLBiol'),
	function(object) {
		return('FLBiols')})

# FLSR -> FLSRs
setMethod('getPlural', signature(object='FLSR'),
	function(object) {
		return('FLSRs')})

# FLModelSim -> FLModelSims
setMethod('getPlural', signature(object='FLModelSim'),
	function(object) {
		return('FLModelSims')})
