<div id="main" class="col-md-9" role="main">

# apply method for FLCore classes

<div class="ref-description section level2">

Applies a function over the margins of an array-based FLCore class

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLArray,numeric,function'
apply(X, MARGIN, FUN, ..., simplify = TRUE)

# S4 method for class 'FLPar,ANY,ANY'
apply(X, MARGIN, FUN, ..., simplify = TRUE)

# S4 method for class 'FLQuantJK,numeric,function'
apply(X, MARGIN, FUN, ..., simplify = TRUE)

# S4 method for class 'FLParJK,numeric,function'
apply(X, MARGIN, FUN, ..., simplify = TRUE)
```

</div>

</div>

<div class="section level2">

## Details

These methods call R's [base::apply](https://rdrr.io/r/base/apply.html)
on an [FLArray](FLArray.md) the standard arithmetic operators included
in the `Arith` group ("+", "-", "\*", \`"^", "%%", "%/%", and "/"), so
that they return an object of the appropriate class.

When the operation involves objects of two classes (e.g. `FLPar` and
`FLQuant`), the class is the returned object is that of the more
complexs object, in this case `FLQuant`.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[base::apply](https://rdrr.io/r/base/apply.html)

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
#>     1  34.86  54.97   9.91 123.01  12.82  37.13  27.54  22.37   7.92  36.35
#>     2  21.03  26.41  17.94  28.63  12.88 103.21  16.99  74.91  37.07   3.26
#>     3  21.19  37.41  24.15  54.59  61.25  17.32  37.90  25.66  53.75  28.04
#> 
#> units:  kg 
# Two FLQuant objects
flq + flq
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1     2     3     4     5     6     7     8     9     10   
#>     1 2.789 4.398 0.793 9.840 1.026 2.970 2.203 1.790 0.633 2.908
#>     2 1.682 2.113 1.435 2.290 1.030 8.256 1.359 5.993 2.966 0.261
#>     3 1.695 2.993 1.932 4.367 4.900 1.385 3.032 2.052 4.300 2.243
#> 
#> units:  kg 
```

</div>

</div>

</div>
