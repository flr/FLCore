# FLCompPM.R - DESC
# /FLCompPM.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# iter

data(ple4)

object <- as(propagate(ple4, 50), 'FLBiol')


summary(object)

summary(iter(object, 1))
