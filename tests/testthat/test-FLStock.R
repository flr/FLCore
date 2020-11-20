# test-FLStock.R - DESC
# /test-FLStock.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# dimnames.FLStock for an stock in biomass #42 

data(ple4)

stk <- ple4[1,]

dimnames(stk) <- list(age='all')

validObject(stk)

expect_validclass(ssb(ple4), "FLQuant")
