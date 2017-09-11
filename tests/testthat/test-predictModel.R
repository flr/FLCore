# test-predictModel.R - DESC
# /test-predictModel.R

# Copyright 2015 Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) 1.1.
#
# Notes:

data(ple4)

# EXAMPLE mobject
model <- new("predictModel", FLQuants(ssb=ssb(ple4)),
  model=~ssb * a / b, params=FLPar(a=20, b=5))

# window
window(model, end=2000)
