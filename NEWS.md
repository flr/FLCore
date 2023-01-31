# FLCore 2.6.20

## NEW FEATURES

- ffwd projects an FLStock for a mean fishing mortality (fbar) target.
- yearMedians method to calculate median across years.

# FLCore 2.6.19

## NEW FEATURES

- New method for vb(FLBiol, sel=FLQuant)
- mbar(FLStock) returns the mean natural mortality, M, over the fully selected age 
  range.
- window(FLlst) will lapply window over all elements.
- computeQ(FLindices, FLStock, FLQuants) computes index.q from indices and fit.
- comine() method now available for all (or most) classes.
- New SRR model, bevholtisg, implements a sigmoidal Beverton-Holt model.
- Class FLStockR contains slots for reference points (@refpts, FLPar) and stock 
  recruitment relationship (@sr, predictModel).

## BUG FIXES

- simplify(FLStock) recoded in a simpler way supported by noseason/unit/area functions.
- Ensure harvest(FLQuant) returns 0 when catch or n are 0.
- Further improvements to simplify. nounit, noseason and noarea do nmot call harvest().
- qapply now works properly in noseason.
- fwdWindow(FLBiol) did not respect residuals FLQuant sometimes.

## USER-VISIBLE CHANGES

- FLStock() constructor defaults to 'age' as quant.
- append(FLQuant, FLQuant) will propagate the first object if required.
- noseason() can either use weighted or unweighted wt means.
- Accessor and replacement methods sr and sr<- operate directly on the predictModel in FLBiol@rec. rec() returns the observation, from @n[1,], and rec.hat the prediction from the model params.

# FLCore 2.6.18

## BUG FIXES

- survey(FLStock, FLIndex) returns full FLIndex object.
- rbind(FLPar, FLPar) will keep dimnames names to those of first element, e.g.
  'refpts'

# FLCore 2.6.17

## BUG FIXES

- qname in data.frame from FLPars is now a factor to not brake geom_flpar.  
- sel as argument in survey(FLStock, FLIndex).
- vb(FLStock) now uses directly catch.sel().
- segreg simplified to play better with plots.

## USER-VISIBLE CHANGES

- Added g (gram) as recognized uom
- Added uoms operations for g (gram).
- New fwd(FLStock) method projects from first year N for F and M.
- Renamed fwd(FLStock) as adjust.
- Reformulated survey() methods, now works for both FLIndex and FLIndexBiomass.
- oem(biomass=TRUE) returns an aggregated index ib biomass.

# FLCore 2.6.16

## BUG FIXES

- uom() does not strip spaces inside the string, makes '1000 t' work.
- Products of t and multiples of 10 return correct units #62 
- getPlural ignored derived classes, like FLIndexBiomass, now dispatches on
  FLI and FLS.
- harvest(FLQ) now runs optimize for one year objects, handles well iters.
- computeCatch(FLStock) propagates slots if not matching. Logical subsetting
  fails otherwise.
- runstest made safer.
- vb(FLStock) now uses directly catch.sel().

## USER-VISIBLE CHANGES

- uom() is not called anymore on FLQuant - numeric operations.

## NEW FEATURES

- survivors(FLStock) function calculates the population survivors and returns
  them as an FLQuant.
- sr(FLBiol) to access full @rec slot.
- Methods have been defined for aithmetic operations element by element between
  two FLQuants objects, and for dividing each of an FLQuants by each of an FLPars
  of the same length.
- fwdWindow(FLStock) method to extend with same syntax as for other classes.
- fwd(FLStock) method projects forward from iniial timestep to reconstruct
  abundances from F and M.

# FLCore 2.6.15

## USER-VISIBLE CHANGES

- FLCore now imports ggplot2 to avoid the clash over the %+% operator veings defined in 
  both packages. FLCore now uses ggplot::%+% as basis for the method generic. Only this 
  function is imported, as FLCore does not use any other ggplot2 method.
- qapply() now has a simplify argument to call unlist() on the output and
  return a vector. Deafults to simplify=FALSE
- windows accepts a negative value for end and will substract that number
  of years from the object.
- metrics() can now handle formulas combining object functions and vector or
  FLPar elements, e.g. metrics(ple4, ~ssb/Blim, FLPar(Blim=300000))

## NEW FEATURES

- mohnMatrix to construct a table of metrics to compute Mohn's rho from an
  FLStocks object obtained from a restrospective analysis.
- readVPAInterCatch creates an FLQuant from a VPA file in the format exported
  by ICES Intercatch system.
- fwd(FLQuant) to move one year forward a population vector.
- fwd(FLStock) calculates survivors and places them one year one age ahead.
- qapply can simplify output, calling unlist.
- divide() separates object along iters into list.
- cv(FLQuantPoint) returns std/mean.
- New as.data.frame method for FLQuantPoint.
- '1000 t' is now accepted by uom().

## BUG FIXES

- iter<- method for FLBiol and predictModel.
- logrstandard can now deal with NaN

# FLCore 2.6.14

## NEW FEATURES

- intersect method called on two FLQuants returns them subset to only the common
  dimension names.
- standardUnits provides a possible set of standard units of measurement in
  complex object. Set now for FLS/FLStock, using 1000, kg, t, m, and f, as in
  data(ple4).
- append method for FLQuant and FLStock adss an object to another by year
  dimnames or ar year specified by 'after'.

# FLCore 2.6.12

## DEPRECATED & DEFUNCT

- flc2flq has been deprecated. Use as(object, 'FLQuant') instead.

## BUG FIXES

- Fix to iter(vector) after addition of iterators dependency.

# FLCore 2.6.11

## NEW FEATURES
- dimnames(FLComp) method defined, returns dimnames corresponding to maximum length per dimension.

## USER-VISIBLE CHANGES
- combine(FLQuant) and combine (FLComp) can now accept multiple objects, useful for using with foreach's .multicombine.
- Replacement method for FLlst elements ([[<-) now allows extending it.
- Dependency added on iterators package. iter() method was overwritten when iterators was loaded.

## BUG FIXES
- FLStock@mat conversion to FLBiol@mat/fec now works with NAs

# FLCore 2.6.10

## NEW FEATURES
- window(FLQuantDistr) method added to deal with @var slot
- Method %*% for FLQuants,FLPar added
- Better handling of currency units, so far for EUR and USD. Symbols are accepted
- show() for FLComp-derived classes will call summary instead of printDefault. print() will still show the full object

## BUG FIXES
- propagate(FLPar) placed the original object in the matching dimname$iter,
  instead of iter=1
- Methods for %^% and ^ did not deal properly with units. Exponentation now returns
  the units of the base

# FLCore 2.6.9

## NEW FEATURES

- survSRR implements Taylor et al (2013, doi: 10.1016/j.fishres.2012.04.018) model for low fecundity species.
- ssf(FLStock) method returns the Spawning Stock Fecundity, as stock.n times mat, where mat holds fecundity by individual
- spread() function to make available inside a function a list of parameters

## USER-VISIBLE CHANGES

- Added calcF=TRUE to simplify(), to speed it up when harvest is not needed

# FLCore 2.6.7

## NEW FEATURES

-  Added iterProb(FLQuant) method

## USER-VISIBLE CHANGES

- writeADMB now accepts append=TRUE/FALSE
- Added generics from mpb and FLRP
- Improved simplify(FLStock)
- predict(predictModel) now accepts numeric vectors and FLPar objects in ...

## BUG FIXES

- Corrected simplify(FLStock) to use unweighted means for wts
- Added ... to msy et al generics
- Added numeric code 23 for bevholtss3
- BUG in '[', wrong pgroup if i=-1
- Corrected simplify(FLStock) to use unweighted means for wt 
- Bug in uom for FLQ,numeric when numeric was of length > 1
- Bug when dims was only one of unit or area
- Convert numeric to uom in Arith only if length==1

# FLCore 2.6.6

## NEW FEATURES

- window(FLStocks) method to apply same window call across all elements.
- combine(FLStocks) method collapses all elements into a single FLStock object.
  with as many iters as elements in the list, must all have 1 iter.
- combine(FLQuants, FLQuants) merges them one element at a time.
- drop(FLQuant) returns the result of a call to base::drop on @.Data.
- New verify() method for complex classes evaluates object against a set of
  rules. Checks content rather than structure, as validity functions do.
- New ruleset method to return standard verify rules per class.
- window() and expand() methods for FLPar.
- iter() method for FLlst, lapplies it along the list

## USER-VISIBLE CHANGES

- propagate(FLPar) to return object if iter == iters #45.
- Ratios of numbers written in alternative ways now also return an empty character.

## BUG FIXES

- dimnames<- for FLStock with no ages failed when changing age name #42.
- uom can now handle better products with units containing divisions.
- coerce(FLBiol, FLBiolcpp) will now extend srparam to have as many years as the
  object, to allow params per time period to be passed and used.
- coerce(FLBiol, FLBiolcpp) now windows FLPar if year is given.
- trim(FLBiol) is able to handle predictModel slots.
- [[<- and $<- FLlst now work on extended classes, like predictModel.
- predict(predictModel) now handles correctly iters and extra FLPar dimensions.

## DEPRECATED & DEFUNCT
- catchSel is to be substituted by catch.sel, use the later instead
- wt<-(FLStock) is to be deprecated, use the individual accessor methods instead

# FLCore 2.6.5

## NEW FEATURES

- New harvest method for stock.n, catch.n and m, by minimizing diff in catch.
- survey and cpue methods to generate observations from an operating model
- knit_print.FLQuant method for the printr package, better printing of FLQuant
  objects in knitr documents

## USER-VISIBLE CHANGES

- FLStock creator uses empty object for sizing, keeps iters but drops them if
  object(s) are named.

## BUG FIXES

- Add tiny value to landings and discards to ensure no division by zero in
  computeCatch.
- Added reduced version to print(FLPar, reduced=TRUE) to reuse in summary methods
 
# FLCore 2.6.4

## NEW FEATURES
- `%=%` operator to modify on thed fly the content of an FLQuant/FLArray. Object
  structure can be reused with contents being changed, e.g.
  FLQuant(2, dim=c(3,10)) %=% 0 will return the object with zeros.
- FLModelSim and FLModelSims new classes
- [<- FLArray now accepts an input FLQuant and keeps the structure, still recyling as appropriate.
- New FLStockLen class for length-based stock data and results.
- New tS and tS<- method to extract and replace individual time steps in an object with
  years and seasons
- tsp method for FLQuant returns 'tsp' attribute: start, end and frequency (seasons)
- Arith method for FLQuant is now unit-aware, see ?units
- wireframe method from lattice now available for FLQuant
- [ method for FLQuantDistr
- FLIndexBiomass class for biomass-based indices of abundance.
- Class FLBiolcpp for interfacing  with CPP
- New catch.sel method for FLStock, computed as proportion across ages of harvest
- predictMosdel class for use in FLBiol
- A set of aliases for the class creators is now available. Should be used only when working interactively.
- Added dim method to FLQuant and FLStock, ensures an unnamed vector is returned
- Added $ method for FLQuant, operates along 'quant' dimension
- New method metrics to compute from list or function
- Added vb method for vulnerable biomass

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
- getPlural is now an S4 method dispatching on singular classes and returning the name of the corresponding FLlst-based class
- as.data.frame(FLQuants) returns qname as factor with levels in the order of the input object
- modified quantile(FLQuant) so dimnames in iter follow quantile(array), e.g. 50%
- tail method for FLQuant, operates by default along the year dimension.
- deprecated sr function is now a method to return aligned stock/recruits FLQuants from an FLStock
- propagate(FLComp) accepts extending an object to existing nu mber of iters for all other slots of length 1.
- trim(FLS) u[pdates min/maxfbar if needed
- transform(FLComp) now accepts a named FLQuants object as substitution argument (...)
- FLQuants() now accepts an FLCOMP object and a list of names/functions to be used to extract individual FLQuant(s)
- FLPar validity now checks that content is numeric. Default frist dimname is now 'params'
- FLIndices class can take both FLIndex and FLIndexBiomass objects
- mean.lifespan renamed as meanLifespan to avoid S3 conflict
- uom() returns kg for 1 * kg
- expand() now stops if new dimnames do not contain old ones and these were of length greater than 1
- seasonSums is now based on colSums and aperm, 75 times faster
- new lattice-based plot(FLStocks)
- Added dimensions line to summary(FLComp)
- Defining fwd generic here so it can be used by mpb and Flash(er)
- Validity for FLlst is now more flexible to accomodate children classes like FLIndexBiomass
- Dropped redundant Sums and Products methods for FLQuants, use Reduce("+", ...) instead

## BUG FIXES
- coerce of data.frame to FLPar deals rightly with multiple params and no iters in wide format.
- iterMeans(FLQuant) was not operating along the 6th dim
- coerce from FLPar to data.frame now works as expected
- Added check for range names to FLComp class validity. Elements must be named with non-empty strings and contain, at least, min, max, minyear and maxyear
- coerce from data.frame to FLPar now handles correctly iters
- apply(FLArray) did not keep the object units
- rnorm, rlnorm and rpois did not keep tje object units
- code2name for FLSR did not reocngize properly bevholtSV
- fmle() can now use method="Brent" for one parameter problems
- New ple4.biol with data up to 2008. No integer value
- prototype object for FLComp VIRTUAl class has name as character(1)
- as.FLQuant(df) failed on objects with no 'year' column
- propagate(FLQuant) now accepts iter == dim[6]
- Faulty comparison in expand(FLArray) fixed #7
- Wrong test in validity for FLModelSim
- Fixed bug in FLPar %% FLPar when objects were actually equal in dims, always returned product
- quantile(FLQuant) returned oject with wrong dimensions
- FLPar(missing) now creates objects with right number of iters, works when used with call()
- lapply(FLst) works on zero length object, does nothing
- square operators for FLlst return the right names, desc and lock
- Attributes of FLPar maintained by [ and [<-
- as(data.frame, 'FLPar') transposed objects without need
- [<- FLlst now respects names when list elements are added
- rbind(FLPar) should work better now
- predictModel@model slot now has by default an emptyenv()
- uom is now safer to non-strings by using sprintf() for conversion
- Quick fix for readIndicesAdapt dimensions
- apply(FLArray) did not return an FLQuant if function did not change the dimensions of the object

## DOCUMENTATION

- All of the documentation has been converted to roxygen2.

## DEPRECATED & DEFUNCT
- FLFleet, FLMetier and FLCatch classes (plus their plurals) and methods have been moved to the FLFleet package
