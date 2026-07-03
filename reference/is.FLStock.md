# Is this an FLStock object?

Checks whether an object inherits from the FLStock class.

## Usage

``` r
is.FLStock(x)
```

## Arguments

- x:

  An object to test

## Value

A logical value, TRUE if x inherits from FLStock

## See also

[FLStock](FLStock.md)

## Author

FLR Team

## Examples

``` r
data(ple4)
is.FLStock(ple4)
#> [1] TRUE
is.FLStock(stock.n(ple4))
#> [1] FALSE
```
