# Method trim

Trim FLR objects using named dimensions

## Usage

``` r
trim(x, ...)

# S4 method for class 'FLArray'
trim(x, ...)

# S4 method for class 'FLComp'
trim(x, ...)

# S4 method for class 'FLS'
trim(x, ...)

# S4 method for class 'FLBiol'
trim(x, ...)
```

## Details

Subsetting of FLR objects can be carried out with dimension names by
using `trim`. A number of dimension names and selected dimensions are
passed to the method and those are used to subset the input object.

Exceptions are made for those classes where certain slots might differ
in one or more dimensions. If trim is applied to an FLQuant object of
length 1 in its first dimension and with dimension name equal to 'all',
values to `trim` specified for that dimension will be ignored. For
example, [`FLStock`](FLStock.md) objects contain slots with length=1 in
their first dimension. Specifying values to trim over the first
dimension will have no effect on those slots (`catch`, `landings`,
`discards`, and `stock`). Calculations might need to be carried out to
recalculate those slots (e.g. using `computeCatch`, `computeLandings`,
`computeDiscards` and `computeStock`) if their quant-structured
counterparts are modified along the first dimension.

## Generic function

trim(x)

## See also

[FLQuant](FLQuant.md), [FLStock](FLStock.md), [FLCohort](FLCohort.md),
[FLIndex](FLIndex.md)

## Author

The FLR Team

## Examples

``` r
flq <- FLQuant(rnorm(90), dimnames=list(age=1:10, year=2000:2016))

trim(flq, year=2000:2005)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2000    2001    2002    2003    2004    2005   
#>   1   0.0529  1.6361  0.3161 -0.4363  1.7327 -0.8571
#>   2  -0.1381  2.7415 -0.5440  1.0475  0.0277  2.5716
#>   3  -1.3367  0.0507 -0.0883  0.5798  0.7351 -0.5541
#>   4   0.2721  2.2964  0.4471 -2.6108 -2.0194 -0.5680
#>   5   1.0749  1.0242  1.3863 -0.7511  1.0797 -0.0571
#>   6  -1.0040  0.5294  0.0484  0.4667 -0.7335 -0.4961
#>   7   2.0302  0.5982 -1.3606 -1.2876  1.2960 -0.4555
#>   8   0.2995 -0.8931 -0.8720 -0.7952  0.2310  1.8457
#>   9   0.2337  0.2457 -0.5593  1.1870  1.6679 -0.3088
#>   10 -0.7766 -1.1027 -1.5514 -0.8880  1.0687  0.9808
#> 
#> units:  NA 
# which is equivalent to
window(flq, start=2000, end=2005)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2000    2001    2002    2003    2004    2005   
#>   1   0.0529  1.6361  0.3161 -0.4363  1.7327 -0.8571
#>   2  -0.1381  2.7415 -0.5440  1.0475  0.0277  2.5716
#>   3  -1.3367  0.0507 -0.0883  0.5798  0.7351 -0.5541
#>   4   0.2721  2.2964  0.4471 -2.6108 -2.0194 -0.5680
#>   5   1.0749  1.0242  1.3863 -0.7511  1.0797 -0.0571
#>   6  -1.0040  0.5294  0.0484  0.4667 -0.7335 -0.4961
#>   7   2.0302  0.5982 -1.3606 -1.2876  1.2960 -0.4555
#>   8   0.2995 -0.8931 -0.8720 -0.7952  0.2310  1.8457
#>   9   0.2337  0.2457 -0.5593  1.1870  1.6679 -0.3088
#>   10 -0.7766 -1.1027 -1.5514 -0.8880  1.0687  0.9808
#> 
#> units:  NA 

trim(flq, year=2000:2005, age=1:2)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2000    2001    2002    2003    2004    2005   
#>   1  0.0529  1.6361  0.3161 -0.4363  1.7327 -0.8571
#>   2 -0.1381  2.7415 -0.5440  1.0475  0.0277  2.5716
#> 
#> units:  NA 


# Now on an FLStock
data(ple4)
summary(trim(ple4, year=1990:1995))
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  6   1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   10  10  1990    1995    2   6   
#> 
#> Metrics: 
#>   rec: 550376 - 1083810  (1000) 
#>   ssb: 222203 - 396458  (t) 
#>   catch: 132629 - 250604  (t) 
#>   fbar: 0.60 - 0.61  (f) 

# If 'age' is trimmed in ple4, catch, landings and discards need to be
# recalculated
  shpl4 <- trim(ple4, age=1:4)
  landings(shpl4) <- computeLandings(shpl4)
  discards(shpl4) <- computeDiscards(shpl4)
  catch(shpl4) <- computeCatch(shpl4)
  summary(shpl4)
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  4   61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   4   NA  1957    2017    2   4   
#> 
#> Metrics: 
#>   rec: 367450 - 4303680  (1000) 
#>   ssb: 91016 - 316967  (t) 
#>   catch: 36219 - 253152  (t) 
#>   fbar: 0.22 - 0.71  (f) 
```
