<div id="main" class="col-md-9" role="main">

# A class for model prediction

<div class="ref-description section level2">

Object of the predictModel class are used in various FLR classes to
allow flexible modelling of the dynamics of different biological and
technological processes.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLQuants,formula'
predictModel(object, model, params = FLPar())

# S4 method for class 'FLQuants,missing'
predictModel(object, params = FLPar())

# S4 method for class 'FLQuants,character'
predictModel(object, model, params = FLPar())

# S4 method for class 'FLQuants,function'
predictModel(object, model, params = FLPar())

# S4 method for class 'FLQuants,list'
predictModel(object, model, params = FLPar())

# S4 method for class 'missing,ANY'
predictModel(object, model, ...)
```

</div>

</div>

<div class="section level2">

## Details

The dependency of life history processes, such as maturity and
fecundity, to biological and environmental factors, can be represented
in objects of this class via a simple model (represented by a `formula`)
and the corresponding paramaters (`FLPar`) and inputs (`FLQuants`).

</div>

<div class="section level2">

## Slots

-   .Data:

    Inputs to the model not found in enclosing class (`FLQuants`).

-   model:

    Model representation (`formula`).

-   params:

    Model paramaters (`FLPar`).

</div>

<div class="section level2">

## Validity

-   VALIDITY:

    Neque porro quisquam est qui dolorem ipsum.

You can inspect the class validity function by using
`getValidity(getClassDef('predictModel'))`

</div>

<div class="section level2">

## Accessors

All slots in the class have accessor and replacement methods defined
that allow retrieving and substituting individual slots.

The values passed for replacement need to be of the class of that slot.
A numeric vector can also be used when replacing FLQuant slots, and the
vector will be used to substitute the values in the slot, but not its
other attributes.

</div>

<div class="section level2">

## Constructor

A construction method exists for this class that can take named
arguments for any of its slots. All slots are then created to match the
requirements of the class validity.

</div>

<div class="section level2">

## Methods

Methods exist for various calculations based on values stored in the
class:

-   METHOD:

    Neque porro quisquam est qui dolorem ipsum.

</div>

<div class="section level2">

## See also

<div class="dont-index">

`FLQuants` `FLPar` `FLBiol`

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
fec <- FLQuants(fec=FLQuant(rlnorm(10, 20, 5),
  dimnames=list(year=2000:2009), units='1'))
predictModel(fec, model=~fec)
#> $ fec 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 2000     2001     2002     2003     2004     2005     2006     2007    
#>   all 4.36e+09 1.23e+11 4.08e+06 3.76e+06 4.20e+08 1.81e+05 1.89e+10 9.10e+05
#>      year
#> quant 2008     2009    
#>   all 2.54e+11 1.83e+10
#> 
#> units:  1 
#> 
#> model:  
#> ~fec
#> <environment: R_EmptyEnv>
#> 
#> params:  
#> An object of class "FLPar"
#> params
#>    
#> NA 
#> units:  NA 
predictModel(fec)
#> $ fec 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 2000     2001     2002     2003     2004     2005     2006     2007    
#>   all 4.36e+09 1.23e+11 4.08e+06 3.76e+06 4.20e+08 1.81e+05 1.89e+10 9.10e+05
#>      year
#> quant 2008     2009    
#>   all 2.54e+11 1.83e+10
#> 
#> units:  1 
#> 
#> model:  
#> ~NA
#> <environment: R_EmptyEnv>
#> 
#> params:  
#> An object of class "FLPar"
#> params
#>    
#> NA 
#> units:  NA 
predictModel(fec, model="bevholt")
#> $ fec 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 2000     2001     2002     2003     2004     2005     2006     2007    
#>   all 4.36e+09 1.23e+11 4.08e+06 3.76e+06 4.20e+08 1.81e+05 1.89e+10 9.10e+05
#>      year
#> quant 2008     2009    
#>   all 2.54e+11 1.83e+10
#> 
#> units:  1 
#> 
#> model:  
#> rec ~ a * ssb/(b + ssb)
#> <environment: R_EmptyEnv>
#> 
#> params:  
#> An object of class "FLPar"
#> params
#>    
#> NA 
#> units:  NA 
predictModel(fec, model=bevholt)
#> $ fec 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 2000     2001     2002     2003     2004     2005     2006     2007    
#>   all 4.36e+09 1.23e+11 4.08e+06 3.76e+06 4.20e+08 1.81e+05 1.89e+10 9.10e+05
#>      year
#> quant 2008     2009    
#>   all 2.54e+11 1.83e+10
#> 
#> units:  1 
#> 
#> model:  
#> rec ~ a * ssb/(b + ssb)
#> <environment: R_EmptyEnv>
#> 
#> params:  
#> An object of class "FLPar"
#> params
#>    
#> NA 
#> units:  NA 
predictModel(fec, model=bevholt())
#> $ fec 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 2000     2001     2002     2003     2004     2005     2006     2007    
#>   all 4.36e+09 1.23e+11 4.08e+06 3.76e+06 4.20e+08 1.81e+05 1.89e+10 9.10e+05
#>      year
#> quant 2008     2009    
#>   all 2.54e+11 1.83e+10
#> 
#> units:  1 
#> 
#> model:  
#> rec ~ a * ssb/(b + ssb)
#> <environment: R_EmptyEnv>
#> 
#> params:  
#> An object of class "FLPar"
#> params
#>    
#> NA 
#> units:  NA 
predictModel(model=rec~a*ssb, params=FLPar(a=1.234))
#> An object of class "FLQuants": EMPTY
#> model:  
#> rec ~ a * ssb
#> <environment: R_EmptyEnv>
#> 
#> params:  
#> An object of class "FLPar"
#> params
#>    a 
#> 1.23 
#> units:  NA 
predictModel(model=bevholt, params=FLPar(a=1.234))
#> An object of class "FLQuants": EMPTY
#> model:  
#> rec ~ a * ssb/(b + ssb)
#> <environment: R_EmptyEnv>
#> 
#> params:  
#> An object of class "FLPar"
#> params
#>    a 
#> 1.23 
#> units:  NA 
predictModel(model="bevholtss3", params=FLPar(a=1.234))
#> An object of class "FLQuants": EMPTY
#> model:  
#> rec ~ (4 * s * R0 * ssb)/(v * (1 - s) + ssb * (5 * s - 1))
#> <environment: R_EmptyEnv>
#> 
#> params:  
#> An object of class "FLPar"
#> params
#>    a 
#> 1.23 
#> units:  NA 
```

</div>

</div>

</div>
