# Class FLIndex

A class for modelling abundance indices.

## Usage

``` r
FLIndex(object, ...)

# S4 method for class 'FLQuant'
FLIndex(object, plusgroup = dims(object)$max, ...)

# S4 method for class 'missing'
FLIndex(object, ...)
```

## Details

The `FLIndex` object holds data and parameters related to abundance
indices.

## Slots

- type:

  Type of index (`character`).

- distribution:

  Statistical distribution of the index values (`character`).

- index:

  Index values (`FLQuant`).

- index.var:

  Variance of the index (`FLQuant`).

- catch.n:

  Catch numbers used to create the index (`FLQuant`).

- catch.wt:

  Catch weight of the index (`FLQuant`).

- effort:

  Effort used to create the index (`FLQuant`).

- sel.pattern:

  Selection pattern for the index (`FLQuant`).

- index.q:

  Catchability of the index (`FLQuant`).

- name:

  Name of the stock (`character`).

- desc:

  General description of the object (`character`).

- range:

  Named numeric vector containing the quant and year ranges, the
  plusgroup, and the period of the year, expressed as proportions of a
  year, that corresponds to the index (`numeric`).

## See also

[computeCatch](compute.md), [dims](dims.md), [iter](iter.md),
[plot](https://rdrr.io/r/graphics/plot.default.html),
[propagate](propagate.md),
[summary](https://rdrr.io/r/base/summary.html),
[transform](https://rdrr.io/r/base/transform.html), [trim](trim.md),
[window](https://rdrr.io/r/stats/window.html), [FLComp](FLComp.md)

## Author

The FLR Team

## Examples

``` r
# Create an FLIndex object.
fli <- FLIndex(index=FLQuant(rnorm(8), dim=c(1,8)), name="myTestFLindex")
summary(fli)
#> An object of class "FLIndex"
#> 
#> Name: myTestFLindex 
#> Description:  
#> Type :   
#> Distribution :   
#> Quant: quant 
#> Dims:  quant     year    unit    season  area    iter
#>  1   8   1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  NA  NA  NA  1   8   NA  NA  
#> 
index(fli)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1      2      3      4      5      6      7      8     
#>   all -0.174 -0.222 -1.010  0.481  1.604 -1.515 -1.416  0.877
#> 
#> units:  NA 

# Creat an FLIndex object using an existing FLQuant object.
  data(ple4)
  # Create a perfect index of abundance from abundance at age
    fli2 <- FLIndex(index=stock.n(ple4))
  # Add some noise around the signal
    index(fli2) <- index(fli2)*exp(rnorm(1, index(fli2)-index(fli2), 0.1))
```
