# Methods iter

Select or modify iterations of an FLR object

## Usage

``` r
iter(obj, ...)

# S4 method for class 'FLArray'
iter(obj, iter)
```

## Details

To extract or modify a subset of the iterations contained in an FLR
object, the `iter` and `iter<-` methods can be used.

In complex objects with various `FLQuant` slots, the `iter` method
checks whether individual slots contain more than one iteration, i.e.
`dims(object)[6] > 1`. If a particular slot contains a single iteration,
that is returned, otherwise the chosen iteration is selected. This is in
contrast with the subset operator `[`, which does not carry out this
check.

For objects of class [`FLModel`](FLModel.md), iters are extracted for
slots of classes `FLQuant`, `FLCohort` and `FLPar`.

## Generic function

iter(object) iter\<-(object,value)

## See also

[FLComp](FLComp.md), [FLQuant](FLQuant.md)

## Author

The FLR Team

## Examples

``` r
# For an FLQuant
  flq <- FLQuant(rnorm(800), dim=c(4,10,2), iter=10)
  iter(flq, 2)
#> An object of class "FLQuant"
#> , , unit = 1, season = all, area = unique
#> 
#>      year
#> quant 1        2        3        4        5        6        7        8       
#>     1  0.44861 -1.80732 -3.12104  1.04894  0.35291 -0.79984 -1.17849 -0.82991
#>     2 -0.12884 -0.44989 -0.07412 -0.94269 -0.83743 -0.72827 -2.09244 -0.98048
#>     3 -0.48173  0.41880  0.65368  0.43004 -0.89013  0.23078 -1.03453  0.05107
#>     4  0.01495  0.63450 -0.87725 -0.73877  1.32645 -0.47491  1.26357 -0.06401
#>      year
#> quant 9        10      
#>     1  0.85175  1.50461
#>     2  0.93286 -0.00824
#>     3 -0.67544 -1.19696
#>     4 -0.68055 -0.43611
#> 
#> , , unit = 2, season = all, area = unique
#> 
#>      year
#> quant 1        2        3        4        5        6        7        8       
#>     1  2.93648  0.28910 -0.35996  0.57121 -1.09793 -0.64504  0.19597  1.31955
#>     2  1.34920 -0.25513  0.12575 -0.09613 -0.46458 -1.86819 -1.76829 -0.28430
#>     3  0.58926  0.17104 -2.21800  0.76989  0.65670 -0.95729  0.98283  0.95124
#>     4 -0.65806 -0.34922  0.85102 -0.28460  0.65688 -0.76969 -0.29883 -0.16706
#>      year
#> quant 9        10      
#>     1 -0.79301  2.56339
#>     2  1.24188  1.49565
#>     3 -0.94970  0.73387
#>     4 -0.86386 -0.76909
#> 
#> units:  NA 
# For the more complex FLStock object
  data(ple4)
  fls <- propagate(ple4, 10)
  # Extraction using iter...
    fls2 <- iter(fls, 2)
    summary(fls2)
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
#>   rec: 367450 - 4303680  (1000) 
#>   ssb: 203391 - 913290  (t) 
#>   catch: 78360 - 315245  (t) 
#>   fbar: 0.20 - 0.72  (f) 
```
