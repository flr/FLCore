<div id="main" class="col-md-9" role="main">

# Arithmetic operators for FLCore classes

<div class="ref-description section level2">

Overloaded arithmetic operators for FLCore classes

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'numeric,FLArray'
Arith(e1, e2)

# S4 method for class 'FLArray,numeric'
Arith(e1, e2)

# S4 method for class 'FLArray,FLArray'
Arith(e1, e2)

# S4 method for class 'FLPar,FLPar'
Arith(e1, e2)

# S4 method for class 'FLArray,FLPar'
Arith(e1, e2)

# S4 method for class 'FLPar,FLArray'
Arith(e1, e2)
```

</div>

</div>

<div class="section level2">

## Details

These methods apply the standard arithmetic operators included in the
`Arith` group ("+", "-", "\*", "^", "%%", "%/%", and "/"), so that they
return an object of the appropriate class.

When the operation involves objects of two classes (e.g. `FLPar` and
`FLQuant`), the class is the returned object is that of the more
complexs object, in this case `FLQuant`.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[methods::Arith](https://rdrr.io/r/methods/S4groupGeneric.html)
[base::Arithmetic](https://rdrr.io/r/base/Arithmetic.html)

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
flq <- FLQuant(rlnorm(90), dim=c(3,10), units='kg')
flp <- FLPar(a=99)

# FLQuant and numeric
flq * 25
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1      2      3      4      5      6      7      8      9      10    
#>     1   2.18  78.83  19.58  46.89  41.73  23.72  39.93  52.28   9.81   5.51
#>     2  24.86   4.04  18.84 197.14   3.88  43.03  35.94 165.24  24.60  63.70
#>     3  46.55  19.52  14.37   4.89  14.83  10.02   6.78  22.68  10.94  29.83
#> 
#> units:  kg 
# Two FLQuant objects
flq + flq
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1      2      3      4      5      6      7      8      9      10    
#>     1  0.175  6.306  1.567  3.751  3.339  1.898  3.194  4.183  0.785  0.441
#>     2  1.989  0.323  1.507 15.771  0.310  3.442  2.875 13.219  1.968  5.096
#>     3  3.724  1.562  1.150  0.391  1.187  0.802  0.543  1.814  0.875  2.386
#> 
#> units:  kg 

# FLQuant and FLPar
flq / flp
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1        2        3        4        5        6        7        8       
#>     1 0.000883 0.031850 0.007912 0.018946 0.016862 0.009583 0.016132 0.021124
#>     2 0.010045 0.001634 0.007614 0.079651 0.001568 0.017385 0.014521 0.066762
#>     3 0.018806 0.007888 0.005806 0.001977 0.005993 0.004049 0.002740 0.009163
#>      year
#> quant 9        10      
#>     1 0.003962 0.002226
#>     2 0.009941 0.025739
#>     3 0.004419 0.012051
#> 
#> units:  NA 
```

</div>

</div>

</div>
