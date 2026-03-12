# Method propagate

Methods to extend objects of various FLR classes along the `iter` (6th
FLQuant) dimension. Objects must generally have a single `iter` to be
extended. The new iterations can be filled with copies of the existing,
or remain as `NA`.

## Usage

``` r
propagate(object, ...)

# S4 method for class 'FLQuant'
propagate(object, iter, fill.iter = TRUE)
```

## Arguments

- object:

  Object to be propagated.

- fill.iter:

  Should first array be copied to others? Defaults to FALSE.

- iters:

  No. of iterations in output.

## Generic function

propagate(object, ...)

## See also

[FLQuant](FLQuant.md)

## Author

The FLR Team

## Examples

``` r
# An FLQuant with one iter (dim(flq)[6] == 1)
flq <- FLQuant(rnorm(80), dim=c(4,20), quant='age')

# can now be extended along the `iter` dimension, with
#' copies of the first
propagate(flq, 100)
#> An object of class "FLQuant"
#> iters:  100 
#> 
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 1          2          3          4          5          6         
#>   1 -1.1723(0) -0.8079(0)  0.5216(0) -0.0791(0) -1.5098(0) -1.0519(0)
#>   2  1.0291(0) -1.3675(0) -0.7344(0) -1.8013(0)  0.5020(0)  0.2533(0)
#>   3  0.0949(0)  0.6649(0)  0.4077(0)  2.1965(0) -1.6414(0) -0.6192(0)
#>   4 -0.3545(0) -1.3995(0) -0.9431(0) -0.9784(0)  0.8157(0) -1.3308(0)
#>    year
#> age 7          8          9          10         11         12        
#>   1  0.7180(0)  0.2924(0)  1.4531(0)  1.0968(0) -1.4651(0) -0.7151(0)
#>   2  0.7478(0) -0.0365(0)  0.5738(0) -0.3904(0) -1.1495(0)  0.3774(0)
#>   3 -0.2645(0)  0.4676(0)  0.9645(0)  0.9750(0) -0.8308(0) -0.0264(0)
#>   4 -0.2966(0) -0.6988(0)  0.3940(0)  0.2175(0) -1.6057(0)  0.7159(0)
#>    year
#> age 13         14         15         16         17         18        
#>   1  1.0071(0)  1.1463(0)  0.9124(0)  0.6101(0) -0.8975(0) -1.4351(0)
#>   2 -1.0464(0)  1.5353(0)  0.3823(0) -0.4748(0) -0.0515(0)  0.1319(0)
#>   3  1.0828(0) -0.7614(0)  0.7047(0)  0.5331(0) -1.0332(0)  1.1889(0)
#>   4 -0.5425(0) -0.3571(0)  0.0918(0)  1.0982(0)  0.9620(0)  1.1438(0)
#>    year
#> age 19         20        
#>   1 -1.2398(0)  2.2077(0)
#>   2  0.9882(0)  1.1298(0)
#>   3 -1.1349(0)  0.5866(0)
#>   4  0.6470(0)  1.4544(0)
#> 
#> units:  NA 

# or without
iter(propagate(flq, 100, fill.iter=FALSE), 2)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20
#>   1 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#>   2 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#>   3 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#>   4 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#> 
#> units:  NA 
```
