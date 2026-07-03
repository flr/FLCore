# Generate simulated survey observations of abundance

Creates a simulated observation of abundance-at-age or biomass index
from an `FLStock` object representing the operating model (OM),
replicating the sampling process of a scientific survey. The result is
returned as an `FLIndex` or `FLIndexBiomass` object with the `index`
slot populated.

## Usage

``` r
survey(object, index, ...)

# S4 method for class 'FLStock,FLIndex'
survey(
  object,
  index,
  sel = sel.pattern(index),
  ages = dimnames(index)$age,
  timing = mean(range(index, c("startf", "endf"))),
  index.q = index@index.q,
  stability = 1
)

# S4 method for class 'FLStock,FLIndexBiomass'
survey(
  object,
  index,
  sel = sel.pattern(index),
  ages = ac(seq(range(index, c("min")), range(index, c("max")))),
  timing = mean(range(index, c("startf", "endf"))),
  catch.wt = index@catch.wt,
  index.q = index@index.q,
  stability = 1
)

# S4 method for class 'FLStock,missing'
survey(
  object,
  sel = catch.sel(object),
  ages = dimnames(sel)$age,
  timing = 0.5,
  index.q = 1,
  biomass = FALSE,
  stability = 1
)

# S4 method for class 'FLStock,FLIndices'
survey(object, index, ...)
```

## Arguments

- object:

  An `FLStock` operating model from which to draw the survey.

- index:

  An `FLIndex`, `FLIndexBiomass`, or `FLIndices` defining the survey
  design; if missing, a generic survey is constructed.

- ...:

  Additional arguments passed to methods.

- sel:

  Selectivity-at-age pattern to apply; defaults to `sel.pattern(index)`.

- ages:

  Character vector of ages to include; defaults to all ages in `index`.

- timing:

  Fraction of the year at which the survey takes place (0–1); defaults
  to the midpoint of the `startf`/`endf` range entries.

- index.q:

  Catchability of the survey; defaults to `index@index.q`.

- stability:

  Hyperstability/hyperdepletion exponent applied to abundance; 1
  (default) gives a linear response.

- catch.wt:

  Mean weight-at-age used to convert numbers to biomass for
  `FLIndexBiomass` surveys; defaults to `index@catch.wt`.

- biomass:

  Logical; if `TRUE` and `index` is missing, returns an `FLIndexBiomass`
  object; otherwise an `FLIndex`.

## Value

An `FLIndex` or `FLIndexBiomass` object with the `index` slot populated
by the simulated survey observations, or a list of such objects when
`index` is of class `FLIndices`.

## Generic function

survey(object, index, ...)

## See also

[FLIndex](FLIndex.md), [FLIndexBiomass](FLIndexBiomass.md),
[FLStock](FLStock.md), [cpue](cpue.md), [index](accessors.md)

## Author

The FLR Team

## Examples

``` r
data(ple4)
data(ple4.index)
# CONSTRUCT a survey from stock and index
survey(ple4, ple4.index)
#> An object of class "FLIndex"
#> 
#> Name: BTS-Combined (all) 
#> Description: Plaice in IV                                             . Imported from VPA file. 
#> Type :  numbers 
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  22  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   10  1   1996    2017    0.6453376   0.6453376   
#> 
# Create FLIndexBiomass
ple4.biom <- as(ple4.index, "FLIndexBiomass")
survey(ple4, ple4.biom)
#> An object of class "FLIndexBiomass"
#> 
#> Name: BTS-Combined (all) 
#> Description: Plaice in IV                                             . Imported from VPA file. 
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  22  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   10  1   1996    2017    0.6453376   0.6453376   
#> 
data(ple4)
survey(ple4)
#> An object of class "FLIndex"
#> 
#> Name:  
#> Description:  
#> Type :  number 
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   10  10  1957    2017    0.5 0.5 
#> 
survey(ple4, biomass=TRUE)
#> An object of class "FLIndexBiomass"
#> 
#> Name:  
#> Description:  
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   10  10  1957    2017    0.5 0.5 
#> 
```
