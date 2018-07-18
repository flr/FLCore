# load.R - DESC
# /load.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(FLCore)

# File from ICES WGNSSK. 16 June 2018

load('ple.27.420_stock_object.Rdata')

# Objects

ple4 <- ass.stockOrigDisc
ple4.indices <- indices

# FLStock

verify(ple4)

summary(ple4)

# CHANGE units
units(ple4) <- list(catch="t", catch.n="1000", discards="t", discards.n="1000",
  landings="t", landings.n="1000", stock="t", stock.n="1000", m="m", mat="",
  harvest.spwn="", m.spwn="")

# CHANGE name and desc
name(ple4) <- "PLE"
desc(ple4) <- "Plaice in IV. ICES WGNSSK 2018. FLAAP"

# FLIndices

summary(ple4.indices)

# FLIndex

ple4.index <- ple4.indices[[1]]

# FLBiol
ple4.biol <- as(ple4, "FLBiol")

save(ple4, file="../data/ple4.RData", compress="xz")
save(ple4.indices, file="../data/ple4.indices.RData", compress="xz")
save(ple4.index, file="../data/ple4.index.RData", compress="xz")
