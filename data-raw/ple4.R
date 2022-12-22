# load.R - DESC
# /load.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(FLCore)

# LOAD file from ICES WGNSSK. 16 June 2018 AAP

load('ple.27.420_stock_object.Rdata')

ple <- mget(load('ple.27.420_stock_object.Rdata'))

# SELECT objects

ple4 <- ass.stockOrigDisc
ple4.indices <- indices


# --- CONSTRUCT FLStock

verify(ple4)

summary(ple4)

# CHANGE units
units(ple4) <- list(catch="t", catch.n="1000", discards="t", discards.n="1000",
  landings="t", landings.n="1000", stock="t", stock.n="1000", m="m", mat="",
  harvest.spwn="", m.spwn="")

# SET wt > 0

# CHANGE name and desc
name(ple4) <- "PLE"
desc(ple4) <- "Plaice in IV. ICES WGNSSK 2018. FLAAP"


# --- CONSTRUCT FLIndices

summary(ple4.indices)

# GET RID of zeroes in @index
ple4.indices <- lapply(ple4.indices, function(x) {
  idx <- index(x) == 0
  index(x)[idx] <- min(index(x)) / 10
  return(x)
  })

# DROP age 0
ple4.indices[["BTS-Combined (all)"]] <- ple4.indices[["BTS-Combined (all)"]][-1,]
ple4.indices[["IBTS_Q3"]] <- ple4.indices[["IBTS_Q3"]][-1,]

# ADD index.q, sel.pattern and catch.wt

ple4.indices <- Map(function(x, y) {
  sel.pattern(x)[] <- y[dimnames(x)$age,] / max(y[dimnames(x)$age,])
  index.q(x)[] <- quantMeans(y)
  catch.wt(x) <- stock.wt(ple4)[dimnames(x)$age, dimnames(x)$year]
  return(x)
  }, x=ple4.indices, y=resultsOrigDisc@q.hat)

# --- COMPARE AAP, XSA & a4a

library(FLXSA)
res <- FLXSA(ple4, ple4.indices)

library(FLa4a)
fit <- sca(ple4, ple4.indices, fit="assessment")

vpa <- VPA(ple4)

library(ggplotFL)

plot(FLStocks(AAP=ple4, XSA=ple4+res, A4A=ple4+fit, VPA=ple4+vpa))


# --- BUILD FLIndex

ple4.index <- ple4.indices[["BTS-Combined (all)"]]


# --- BUILD FLBiol

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
