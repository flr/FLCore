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
#>     1  0.26983 -1.84866  1.38604  0.81172 -1.28503  0.54323 -0.87492  0.54465
#>     2 -0.53585  0.20981 -1.34730  0.58537  0.46244  0.98405  0.90995  0.05670
#>     3 -0.49261 -0.75178 -0.15214  0.04071  0.20973 -1.09342 -1.58762 -0.13044
#>     4 -1.19070 -0.55097 -0.78558  1.08571  1.12442  0.64754  0.07231  1.15916
#>      year
#> quant 9        10      
#>     1 -0.71205 -0.34993
#>     2 -0.17977 -0.31565
#>     3 -0.33477  2.27023
#>     4 -0.32931 -0.04837
#> 
#> , , unit = 2, season = all, area = unique
#> 
#>      year
#> quant 1        2        3        4        5        6        7        8       
#>     1 -1.04720  0.44900 -0.26822 -0.11084 -0.53312  0.98774  0.78443 -0.65225
#>     2  0.58229 -0.95558 -1.45039  0.24062  0.06298  1.74256  2.39997 -0.05588
#>     3 -0.21924 -0.00129  1.41165 -0.43766 -2.25481 -0.73121 -1.83341 -0.73407
#>     4 -0.27026  0.00591  0.25183 -0.32832 -0.47056 -0.75426  1.32647  1.07292
#>      year
#> quant 9        10      
#>     1  1.00419  1.15048
#>     2  0.97762 -0.63483
#>     3 -0.18676 -0.35736
#>     4 -0.38289  1.65763
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
