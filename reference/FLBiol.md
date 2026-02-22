# Class FLBiol

A class for modelling age / length or biomass structured populations.

## Usage

``` r
FLBiol(object, ...)

# S4 method for class 'FLQuant'
FLBiol(object, plusgroup = dims(object)$max, ...)
```

## Arguments

- object:

  FLQuant object used for sizing

- ...:

  Other objects to be assigned by name to the class slots

- plusgroup:

  Plusgroup age, to be stored in range

## Details

The `FLBiol` class is a representation of a biological fish population.
This includes information on abundances, natural mortality and
fecundity.

## Slots

- n:

  Numbers in the population. `FLQuant`.

- m:

  Mortality rate of the population. `FLQuant`.

- wt:

  Mean weight of an individual. `FLQuant`.

- mat:

  `predictModel`.

- fec:

  `predictModel`.

- rec:

  `predictModel`.

- spwn:

  Proportion of time step at which spawning ocurrs. `FLQuant`.

- name:

  Name of the object. `character`.

- desc:

  Brief description of the object. `character`.

- range:

  Named numeric vector describing the range of the object. `numeric`.

## Accessors

All slots in the class have accessor and replacement methods defined
that allow retrieving and substituting individual slots.

The values passed for replacement need to be of the class of that slot.
A numeric vector can also be used when replacing FLQuant slots, and the
vector will be used to substitute the values in the slot, but not its
other attributes.

## Constructor

A construction method exists for this class that can take named
arguments for any of its slots. All slots are then created to match the
requirements of the class validity. If an unnamed `FLQuant` object is
provided, this is used for sizing but not stored in any slot.

## Validity

- Dimensions:

  All FLQuant slots must have iters equal to 1 or 'n'.

- Iters:

  The dimname for iter1 should be '1'.

- Dimnames:

  The name of the quant dimension must be the same for all FLQuant
  slots.

## See also

as.FLBiol, as.FLSR, [coerce](https://rdrr.io/r/methods/setAs.html),
[plot](https://rdrr.io/r/graphics/plot.default.html), [ssb](ssb.md)
catch.n,FLBiol-method

## Author

The FLR Team

## Examples

``` r
# An FLBiol example dataset
data(ple4.biol)
#> Warning: namespace ‘colorspace’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘Rcpp’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘dplyr’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘data.table’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘remotes’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘xtable’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘shiny’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘htmlwidgets’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’

summary(ple4.biol)
#> An object of class "FLBiol"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear 
#>  1   10  10  1957    2017    
#> 
#> mat           ~ mat 
#>   mat         : [ 10 61 1 1 1 1 ], units =   
#>   NA          : [ 1 1 ], units =  NA 
#> fec           ~ fec 
#>   fec         : [ 10 61 1 1 1 1 ], units =   
#>   NA          : [ 1 1 ], units =  NA 
#> rec           ~ rec a * ssb * exp(-b * ssb) 
#>   residuals   : [ 1 60 1 1 1 1 ], units =   
#>   a, b        : [ 2 1 ], units =    
```
