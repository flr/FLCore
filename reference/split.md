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
#>   all 1.24(0.0936) 1.02(0.1798) 1.67(0.2275) 1.55(0.3074) 1.50(0.3015)
#>      year
#> quant 6            7            8            9            10          
#>   all 1.82(0.1467) 1.70(0.0937) 1.79(0.1548) 2.02(0.0104) 1.87(0.3403)
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
#> quant 1           2           3           4           5           6          
#>   all 1.21(0.260) 1.28(0.267) 1.55(0.396) 1.36(0.237) 1.58(0.160) 1.72(0.217)
#>      year
#> quant 7           8           9           10         
#>   all 1.68(0.210) 1.91(0.409) 1.92(0.177) 2.06(0.201)
#> 
#> units:  NA 
#> 
```
