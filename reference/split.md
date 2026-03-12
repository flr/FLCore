# splits *x* along the *iter* dimension into the groups defined by *f*.

Similar to base::split, but working along the 6th, *iter*, dimension of
any singular FLR object. The object is divided into as many objects as
unique values in *f*, and returned as an FLlst-derived object, e.g. an
FLQuants object when applied to an FLQuant.

## Usage

``` r
# S4 method for class 'FLQuant,vector'
split(x, f)

# S4 method for class 'FLComp,vector'
split(x, f)
```

## Arguments

- x:

  The object to be split.

- f:

  The vector of group names.

## Value

An object of the corresponding plural class (FLQuants from FLQuant).

## Author

Iago Mosqueira (WMR).

## Examples

``` r
# FROM FLQuant to FLQuants
flq <- rlnorm(20, FLQuant(seq(0.1, 0.8, length=10)), 0.2)
split(flq, c(rep(1, 5), rep(2,15)))
#> $ 1 
#> An object of class "FLQuant"
#> iters:  5 
#> 
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1            2            3            4            5           
#>   all 1.19(0.3900) 1.32(0.1408) 1.09(0.0727) 1.80(0.2458) 1.68(0.3960)
#>      year
#> quant 6            7            8            9            10          
#>   all 1.53(0.1465) 1.97(0.2700) 1.92(0.1113) 1.96(0.3615) 2.19(0.0618)
#> 
#> units:  NA 
#> 
#> $ 2 
#> An object of class "FLQuant"
#> iters:  15 
#> 
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1            2            3            4            5           
#>   all 1.02(0.0986) 1.28(0.2645) 1.42(0.1802) 1.67(0.4276) 1.59(0.3569)
#>      year
#> quant 6            7            8            9            10          
#>   all 1.65(0.1698) 1.99(0.3855) 1.85(0.4263) 1.90(0.2608) 2.07(0.1706)
#> 
#> units:  NA 
#> 
```
