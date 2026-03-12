# apply method for FLCore classes

Applies a function over the margins of an array-based FLCore class

## Usage

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

## Details

These methods call R's [base::apply](https://rdrr.io/r/base/apply.html)
on an [FLArray](FLArray.md) the standard arithmetic operators included
in the [`Arith`](https://rdrr.io/r/methods/S4groupGeneric.html) group
("+", "-", "\*", \`"^", "%%", "%/%", and "/"), so that they return an
object of the appropriate class.

When the operation involves objects of two classes (e.g.
[`FLPar`](FLPar.md) and [`FLQuant`](FLQuant.md)), the class is the
returned object is that of the more complexs object, in this case
[`FLQuant`](FLQuant.md).

## See also

[base::apply](https://rdrr.io/r/base/apply.html)

## Author

The FLR Team

## Examples

``` r
flq <- FLQuant(rlnorm(90), dim=c(3,10), units='kg')
flp <- FLPar(a=99)

# FLQuant and numeric
flq * 25
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1     2     3     4     5     6     7     8     9     10   
#>     1 14.09 14.97  6.40 17.52 52.59 10.57  9.75 61.26 93.96 95.79
#>     2 75.64 19.93 85.23 15.02  4.78 31.27 18.37 11.32  6.35 14.99
#>     3  5.28 35.70 12.16 11.69 11.50 21.69 44.45  5.88 10.89  5.83
#> 
#> units:  kg 
# Two FLQuant objects
flq + flq
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1     2     3     4     5     6     7     8     9     10   
#>     1 1.127 1.197 0.512 1.402 4.207 0.846 0.780 4.901 7.516 7.663
#>     2 6.051 1.594 6.818 1.201 0.382 2.502 1.470 0.905 0.508 1.199
#>     3 0.422 2.856 0.973 0.935 0.920 1.735 3.556 0.470 0.871 0.467
#> 
#> units:  kg 
```
