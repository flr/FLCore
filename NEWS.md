# CHANGES IN FLCore VERSION 2.6

## NEW FEATURES
- FLModelSim and FLModelSims new classes
- [<- FLArray now accepts an input FLQuant and keeps the structure, still recyling as appropriate.
- New FLStockLen class for length-based stock data and results.

## USER-VISIBLE CHANGES
- model.frame(FLComp) now has an mcf=TRUE argument to correct slots of different dim
- computeLogLik method added to FLModel. Will return a logLik object calculated with the data and params in the object
- New iterMedians(FLQuant) method to applky median along iter dim
- coerce method for data.frame to FLPar now assumes a 2D structure with iters as rows and params as columns
- rlnorm(FLPar, ...) and rnorm(FLPar, ...) now available
- mvrnorm(numeric, FLQuant) and mvrnorm(numeric, FLQuant, matrix) now available
- propagate(FLComp) now propagates FLPar slots as well
- FLFleet, FLMetier and FLCatch classes (plus their plurals) and methods have been moved to the FLFleet package
- ifelse has now been definedx for test="FLQuant"
- as.data.frame for FLQuant(s) and FLComp now has an option cohort=FALSE to return a column named cohort, calculated as year-age. Ony for age-quanted objects
- getPlural is now an S4 method dispatching on singular classes and returning the name of the correspoding FLlst-based class
- as.data.frame(FLQuants) returns qname as factor with levels in the order of the input object
- modified quantile(FLQuant) so dimnames in iter follow quantile(array), e.g. 50%

## BUG FIXES
- iterMeans(FLQuant) was not operating along the 6th dim
- coerce from FLPar to data.frame now works as expected
- Added check for range names to FLComp class validity. Elements must be named with non-empty strings and contain, at least, min, max, minyear and maxyear
- coerce from data.frame to FLPar now handles correctly iters
- apply(FLArray) did not keep the object units
- rnorm, rlnorm and rpois did not keep tje object units
- code2name for FLSR did not reocngize properly bevholtSV
	
## UTILITIES

## DOCUMENTATION

## DEPRECATED & DEFUNCT
- FLFleet, FLMetier and FLCatch classes (plus their plurals) and methods have been moved to the FLFleet package