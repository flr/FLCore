# Adds two FLStock objects

Combines two FLStock objects by adding their abundances and catches, and
computing weighted mean biological parameters.

## Usage

``` r
# S4 method for class 'FLStock,FLStock'
e1 + e2
```

## Arguments

- e1:

  An object of class FLStock

- e2:

  An object of class FLStock

## Value

A summed-up FLStock object

## See also

[FLStock](FLStock.md)

## Author

FLR Team

## Examples

``` r
data(ple4)
# Addc ple4 to itself
ple4 + ple4
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   10  10  1957    2017    2   6   
#> 
#> Metrics: 
#>   rec: 734900 - 8607360  (1000) 
#>   ssb: 406782 - 1826579  (NA) 
#>   catch: 156721 - 630489  (t) 
#>   fbar: 0.20 - 0.72  (f) 
```
