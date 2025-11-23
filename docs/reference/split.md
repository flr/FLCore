<div id="main" class="col-md-9" role="main">

# splits *x* along the *iter* dimension into the groups defined by *f*.

<div class="ref-description section level2">

Similar to base::split, but working along the 6th, *iter*, dimension of
any singular FLR object. The object is divided into as many objects as
unique values in *f*, and returned as an FLlst-derived object, e.g. an
FLQuants object when applied to an FLQuant.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLQuant,vector'
split(x, f)

# S4 method for class 'FLComp,vector'
split(x, f)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    The object to be split.

-   f:

    The vector of group names.

</div>

<div class="section level2">

## Value

An object of the corresponding plural class (FLQuants from FLQuant).

</div>

<div class="section level2">

## Author

Iago Mosqueira (WMR).

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

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
#>   all 1.46(0.1714) 1.10(0.3587) 1.58(0.2161) 1.46(0.1791) 1.59(0.2670)
#>      year
#> quant 6            7            8            9            10          
#>   all 1.76(0.4430) 1.97(0.5616) 1.65(0.0675) 2.29(0.8808) 2.62(0.2332)
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
#>   all 0.941(0.175) 1.208(0.342) 1.251(0.238) 1.392(0.213) 1.812(0.370)
#>      year
#> quant 6            7            8            9            10          
#>   all 1.679(0.202) 1.769(0.376) 1.840(0.435) 2.205(0.325) 2.378(0.589)
#> 
#> units:  NA 
#> 
```

</div>

</div>

</div>
