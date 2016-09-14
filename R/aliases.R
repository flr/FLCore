# aliases.R - Short aliases for most FLCore classes construction methods
# FLCore/R//aliases.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' Short aliases for most FLCore classes construction methods
#'
#' When working interactively, the naming convention of the FLCore classes can
#' become tiresome. This set of short, lowercase aliases make calling the class
#' construction methods a bit simpler.
#'
#' We recomend you use the full name of classes and methods when developing code
#' that you intend to keep or distribute. A script written using these aliases
#' can be later 'corrected' by substituting the aliases used in your editor.
#'
#' @usage flq(...) for FLQuant(...)
#' flqp(...) for FLQuantPoint(...)
#' flqd(...) for FLQuantDistr(...)
#' flc(...) for FLCohort(...)
#' flp(...) for FLPar(...)
#' fls(...) for FLStock(...)
#' flsl(...) for FLStockLen(...)
#' fli(...) for FLIndex(...)
#' flib(...) for FLIndexBiomass(...)
#' flsr(...) for FLSR(...)
#' flqs(...) for FLQuants(...)
#' flcs(...) for FLCohorts(...)
#' flss(...) for FLStocks(...)
#' flis(...) for FLIndices(...)
#' flps(...) for FLPars(...)
#' flb(...) for FLBiol(...)
#' flbs(...) for FLBiols(...)
#' flms(...) for FLModelSim(...)
#' flmss(...) for FLModelSims(...)
#' pm(...) for predictModel(...)
#'
#' @return An object of the requested class
#'
#' @name aliases
#' @rdname aliases
#' @aliases flq flqp flqd flc flp fls flsl fli flib flsr flqs flcs flss
#' @aliases flis flps flb flbs flms flmss pm 
#'
#' @author The FLR Team
#' @seealso \link{FLQuant}, \link{FLQuantPoint}, \link{FLQuantDistr}, \link{FLCohort}, \link{FLPar},
#' \link{FLStock}, \link{FLStockLen}, \link{FLIndex}, \link{FLIndexBiomass}, \link{FLSR},
#' \link{FLQuants}, \link{FLCohorts}, \link{FLStocks}, \link{FLIndices}, \link{FLPars}, 
#' \link{FLBiol}, \link{FLBiols}, \link{FLModelSim}, \link{FLModelSims}, \link{predictModel}
#' @keywords methods
#' @examples
#'
#' flq <- flc(1:10, units="kg")


flq <- FLQuant
flqp <- FLQuantPoint
flqd <- FLQuantDistr
flc <- FLCohort
flp <- FLPar

fls <- FLStock
flsl <- FLStockLen
fli <- FLIndex
flib <- FLIndexBiomass

flsr <- FLSR

flqs <- FLQuants
flcs <- FLCohorts
flss <- FLStocks
flis <- FLIndices
flps <- FLPars

flb <- FLBiol
flbs <- FLBiols

flms <- FLModelSim
flmss <- FLModelSims

pm <- predictModel

