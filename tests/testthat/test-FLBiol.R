# test-FLBiol.R - DESC
# /test-FLBiol.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

data(ple4.biol)

object <- ple4.biol

# rec(FLBiol)

# RETURN calculation
rec(object)

# RETURN predictModel slot
rec(object, FALSE)

# rec<-(FLBiol)

# rec(FLBiol) <- predictModel
rec(object) <- model(rec(object, FALSE))
# model(rec(FLBiol)) <- formula
model(rec(object, FALSE)) <- model(rec(object, FALSE))

# rec(FLBiol) <- FLPar
rec(object) <- params(rec(object, FALSE))
# params(rec(FLBiol)) <- FLPar
params(rec(object, FALSE)) <- params(rec(object, FALSE))

# rec(FLBiol) <- FLQuant
rec(object) <- rec(object, FALSE)
rec(object, 'rec') <- rec(object)

# rec(FLBiol) <- predictModel
rec(object, 'rec') <- rec(object, FALSE)

# rec(FLBiol) <- FLQuants
rec(object) <- FLQuants(rec=rec(ple4.biol))

# rec(FLBiol) <- list(FLQuant(s), formula, FLPar)
rec(object) <- list(rec=rec(ple4.biol)/1000, model=~rec+ssb+nao)
rec(object) <- list(rec=rec(ple4.biol), nao=rec(ple4.biol) / 1000, model=~rec * ssb + nao)

