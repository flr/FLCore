<div id="main" class="col-md-9" role="main">

# Methods iter

<div class="ref-description section level2">

Select or modify iterations of an FLR object

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
iter(obj, ...)

# S4 method for class 'FLArray'
iter(obj, iter)
```

</div>

</div>

<div class="section level2">

## Details

To extract or modify a subset of the iterations contained in an FLR
object, the `iter` and `iter<-` methods can be used.

In complex objects with various `FLQuant` slots, the `iter` method
checks whether individual slots contain more than one iteration, i.e.
`dims(object)[6] > 1`. If a particular slot contains a single iteration,
that is returned, otherwise the chosen iteration is selected. This is in
contrast with the subset operator `[`, which does not carry out this
check.

For objects of class `FLModel`, iters are extracted for slots of classes
`FLQuant`, `FLCohort` and `FLPar`.

</div>

<div class="section level2">

## Generic function

iter(object) iter\<-(object,value)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLComp](FLComp.md), [FLQuant](FLQuant.md)

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
# For an FLQuant
  flq <- FLQuant(rnorm(800), dim=c(4,10,2), iter=10)
  iter(flq, 2)
#> An object of class "FLQuant"
#> , , unit = 1, season = all, area = unique
#> 
#>      year
#> quant 1       2       3       4       5       6       7       8       9      
#>     1  1.1577  0.2932 -0.4180  1.7900  0.8850  0.0557 -0.6689 -0.3818 -0.0854
#>     2 -0.9449 -0.0179  1.8327 -0.0347 -2.9269  0.4509  0.3654 -1.8091  1.4901
#>     3 -1.0338 -0.0789  0.0827 -0.1800 -0.8361  0.8047  1.1490 -2.1767  1.1539
#>     4 -1.4752  0.5616 -0.3082 -0.0740  0.6470  0.5829 -2.3126  0.3180 -0.1113
#>      year
#> quant 10     
#>     1  0.9386
#>     2 -0.1560
#>     3  0.4039
#>     4  0.8045
#> 
#> , , unit = 2, season = all, area = unique
#> 
#>      year
#> quant 1       2       3       4       5       6       7       8       9      
#>     1 -0.4026  0.1104  0.4791  1.7358  0.6179 -0.8476 -0.6043  0.0462 -2.2003
#>     2 -0.9333 -0.2131 -0.8292 -1.0788  1.4886  2.2228 -0.2412 -0.0329 -2.1598
#>     3  1.1906  0.7884  0.7997 -0.4223 -0.4197 -0.0253 -0.3864 -0.3719 -0.0356
#>     4 -0.6574  2.1234 -0.4055 -0.5083  0.1295 -0.3215  0.8977  1.2365  0.1094
#>      year
#> quant 10     
#>     1 -1.3257
#>     2  0.2868
#>     3 -0.1113
#>     4 -0.3222
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

</div>

</div>

</div>
