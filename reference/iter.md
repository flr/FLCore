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
#> quant 1       2       3       4       5       6       7       8       9      
#>     1 -1.0979 -0.6450  0.1960  1.3196 -0.7930  2.5634  0.1936  0.3751  0.6370
#>     2 -0.4646 -1.8682 -1.7683 -0.2843  1.2419  1.4957 -1.8257  0.8306 -0.2281
#>     3  0.6567 -0.9573  0.9828  0.9512 -0.9497  0.7339  0.3194 -0.4121 -0.9448
#>     4  0.6569 -0.7697 -0.2988 -0.1671 -0.8639 -0.7691  0.5439 -1.3993  0.5352
#>      year
#> quant 10     
#>     1 -0.4439
#>     2 -1.2650
#>     3 -0.1717
#>     4  0.7078
#> 
#> , , unit = 2, season = all, area = unique
#> 
#>      year
#> quant 1       2       3       4       5       6       7       8       9      
#>     1  0.2955  1.0146 -0.7709  0.0211 -0.5044  1.3892  1.5144 -0.4827 -0.1801
#>     2 -0.8495 -0.2100  0.7376  0.4901 -0.2967  2.2455  1.9355  1.1561  0.5709
#>     3  1.2011  0.0947  1.0545 -1.0931 -0.6010 -0.2377  0.1142 -0.6145 -0.4004
#>     4  0.3470  1.2858 -0.9091 -0.1430 -0.2193  0.6922  2.1305  0.6962 -1.2315
#>      year
#> quant 10     
#>     1 -0.1389
#>     2 -0.2775
#>     3  0.8113
#>     4 -0.4128
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
