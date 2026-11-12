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
#>     1 -1.09315 -0.60100 -0.23768  0.11415 -0.61452 -0.40043  0.81126  0.31763
#>     2 -0.14300 -0.21927  0.69225  2.13045  0.69620 -1.23152 -0.41278 -0.74469
#>     3 -0.50444  1.38921  1.51439 -0.48269 -0.18015 -0.13892 -0.04791  0.06569
#>     4 -0.29665  2.24548  1.93553  1.15605  0.57092 -0.27751 -0.03800 -0.92163
#>      year
#> quant 9        10      
#>     1  0.22679  0.09282
#>     2  0.18971  0.73914
#>     3  0.00345 -0.32204
#>     4  0.66629  0.60120
#> 
#> , , unit = 2, season = all, area = unique
#> 
#>      year
#> quant 1        2        3        4        5        6        7        8       
#>     1  1.75968  0.06657  0.42161 -2.47194  1.35934  0.50081 -0.08220 -0.11555
#>     2 -1.03433  1.42114 -0.09229 -0.62388 -0.93476 -0.20869 -0.99542  0.86933
#>     3  0.51242  0.06793 -0.12541 -0.87147  0.00226 -0.66875 -0.50920 -0.27561
#>     4  1.04183 -0.79991  0.17259  0.43892  0.24400 -0.02304  0.21868  0.00556
#>      year
#> quant 9        10      
#>     1 -0.21165 -0.37601
#>     2  1.71000 -0.39402
#>     3  0.83198  0.40656
#>     4 -0.92566 -0.03091
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
