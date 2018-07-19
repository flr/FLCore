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

# GET RID of zeroes in @index
ple4.indices <- lapply(ple4.indices, function(x) {
  idx <- index(x) == 0
  index(x)[idx] <- min(index(x)) / 10
  return(x)
  })

ple4.indices[["BTS-Combined (all)"]] <- ple4.indices[["BTS-Combined (all)"]][-1,]
ple4.indices[["IBTS_Q3"]] <- ple4.indices[["IBTS_Q3"]][-1,]

# COMPARE AAP, XSA & a4a
library(FLXSA)
res <- FLXSA(ple4, ple4.indices)

library(ggplotFL)
plot(ple4, ple4+res)

library(FLa4a)
fit <- sca(ple4, ple4.indices)
plot(FLStocks(AAP=ple4, XSA=ple4+res, A4A=ple4+fit))

# FLIndex
ple4.index <- ple4.indices[[1]]

# FLBiol

ple4.biol <- as(ple4, "FLBiol")

# rec
ple4sr <- fmle(transform(as.FLSR(ple4, model=ricker), ssb=ssb/100, rec=rec/100))
params(ple4sr)['b',] <- params(ple4sr)['b',] / 100
ple4sr <- transform(ple4sr, ssb=ssb*100, rec=rec*100)
bre <- as(ple4sr, 'predictModel')
units(params(bre)) <- c("","")

rec(ple4.biol) <- bre

summary(ple4.biol)

# ---

save(ple4, file="../data/ple4.RData", compress="xz")
save(ple4.indices, file="../data/ple4.indices.RData", compress="xz")
save(ple4.index, file="../data/ple4.index.RData", compress="xz")
save(ple4.biol, file="../data/ple4.biol.RData", compress="xz")
