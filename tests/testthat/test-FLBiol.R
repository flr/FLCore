# test-FLBiol.R - DESC
# /test-FLBiol.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

data(ple4.biol)

object <- ple4.biol

# rec(FLBiol)

# RETURN observation
rec(object)

# RETURN calculation
rec.hat(object)

# rec<-(FLBiol)

# rec(FLBiol) <- predictModel
sr(object) <- model(sr(object))
# model(rec(FLBiol)) <- formula
model(sr(object)) <- model(sr(object))

# rec(FLBiol) <- FLPar
sr(object) <- params(sr(object))
# params(rec(FLBiol)) <- FLPar
params(sr(object)) <- params(sr(object))

# rec(FLBiol) <- FLQuant
rec(object) <- rec(object)
rec(object, 'rec') <- rec(object)

# rec(FLBiol) <- predictModel
rec(object, 'rec') <- rec(object)

# rec(FLBiol) <- FLQuants
rec(object) <- FLQuants(rec=rec(ple4.biol))

# rec(FLBiol) <- list(FLQuant(s), formula, FLPar)
rec(object) <- list(rec=rec(ple4.biol)/1000, model=~rec+ssb+nao)
rec(object) <- list(rec=rec(ple4.biol), nao=rec(ple4.biol) / 1000, model=~rec * ssb + nao)

