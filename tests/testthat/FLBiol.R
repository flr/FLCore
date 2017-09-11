# FLBiol.R - DESC
# FLBiol.R

# Copyright 2015 Iago Mosqueira. Distributed under the GPL 2.
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

data(ple4)

ple4sr <- fmle(transform(as.FLSR(ple4, model=ricker), ssb=ssb/100, rec=rec/100))
params(ple4sr)['b',] <- params(ple4sr)['b',] / 100
ple4sr <- transform(ple4sr, ssb=ssb*100, rec=rec*100)

# CREATE objects

# COERCE objects

# window

ple4bio <- as(ple4, 'FLBiol')
rec(ple4bio) <- predictModel(model=model(ple4sr), params=params(ple4sr))

summary(window(ple4bio, end=2000))
