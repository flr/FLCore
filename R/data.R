# data.R - DESC
# FLCore/R/data.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' FLCore datasets
#'
#' Example datasets for the classes defined in FLCore.
#'
#' \itemize{
#'    \item{\code{ple4}, \code{\link{FLStock}}}{A dataset for North Sea (ICES Area IV)
#'      plaice. Catch, landings, discards, natural mortality, weight-at-age and maturity,
#'      together with the VPA estimated abundances and fishing mortalities.}
#' \item{\code{ple4sex}, \code{\link{FLStock}}}{A dataset of North Sea (ICES
#'      Area IV) plaice disaggregated by sex. Catch, yield, landings, discards,
#'      natural mortality, weight-at-age and maturity, together with the VPA
#'      estimated abundances and fishing mortalities.}
#' \item{ple4.index, \code{\link{FLIndex}}}{A dataset of North Sea (ICES Area IV) plaice
#'      survey catch per unit effort, index and index variance.}
#' \item{ple4.indices, \code{\link{FLIndices}}}{A dataset of three North Sea (ICES Area IV)
#'      plaice survey catch per unit effort series. Index and index variance.}
#' \item{ple4.biol, \code{\link{FLBiol}}}{A dataset of the North Sea plaice population.
#'      Numbers, natural mortality, mass and fecundity-at-age.}
#' \item{nsher , \code{\link{FLSR}}}{Stock and recruit data and fitted relationship for
#'      autumn spawning North Sea herring.}
#' }
#'
#' Datasets can be loaded by issuing the \code{data} command, like in
#' \code{data(ple4)}.
#'
#' @name datasets
#' @aliases ple4 ple4sex ple4.biol ple4.index ple4.indices nsher
#' @seealso \linkS4class{FLStock}, \linkS4class{FLSR}, \linkS4class{FLIndex},
#' \linkS4class{FLStock}, \linkS4class{FLIndex}, \linkS4class{FLBiol}
#' @references ICES.
#' @keywords datasets
#' @examples
#'
#' data(ple4)
#' summary(ple4)
#'
#' data(nsher)
#' is(nsher)
#'
NULL
