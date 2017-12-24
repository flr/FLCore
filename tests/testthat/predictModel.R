# predictModel.R - DESC
# /predictModel.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the General Public Licence (GPL) V.2.0


# predictModel CLASS

pm <- new('predictModel')

validObject(pm)


pm <- predictModel(a=FLQuant(1:10), model=res~a*b, params=FLPar(b=2))

pm <- predictModel(a=FLQuant(1:10), model=~a*b, params=FLPar(b=2))

predict(pm)


# --- propagate(predictModel)

pm <- predictModel(a=FLQuant(1:10), model=res~a*b, params=FLPar(b=2))

propagate(pm, 20)

pm <- predictModel(a=FLQuant(1:10), b=FLQuant(1:10), model=res~a*b, params=FLPar(b=2))

propagate(pm, 20)
