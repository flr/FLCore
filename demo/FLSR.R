# FLSR - «Short one line description»
# FLCore/demo/FLSR.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

library(FLCore)
data(ple4)

# Let's create a new FLSR model for an FLStock
fsr <- as.FLSR(ple4)

# Now we use the bevholt() function to set it up as a
# Beverton & Holt SR model, loading lkhd, model
model(fsr) <- bevholt()

# To fit this dataset we need to re-scale the recruitment and ssb FLQuants
fsr <- transform(fsr, rec=rec/10000, ssb=ssb/10000)

# Let's look at the contents of the object
summary(fsr)

# Model fitting can now be carried out using fmle(). results are stored in the object itself
fsr <- fmle(fsr)

# A second look at the object to check results...
summary(fsr)

# ... and a plot
plot(fsr)

# Let's now try with a different dataset and model
data(nsher)
model(nsher) <- ricker()
nsher <- fmle(nsher)

# And see what we get
plot(nsher)

# This gives us Akaike's An Information Criterion (AIC)
AIC(nsher)

# A fixed parameter value can be given to fmle
nsher <- fmle(nsher, fixed=list(a=130))
AIC(nsher)
