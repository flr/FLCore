# which_iter

Returns the positions along the 'iter' dimension that match a logical
estatement

## Usage

``` r
which_iter(x, rule)
```

## Arguments

- x:

  An input object, class 'FLQuant'

- rule:

  A logical 'rule', either as a 'character' or a logical 'FLQuant'

## Value

As vector of positions along the 'iter' (6th) dimension

## Details

DETAILS

## Examples

``` r
# Creates an example object
x <- rnorm(50, FLQuant(runif(5, 10, 20)), 20)
# Looks for iters with a large value
which_iter(x, x > 60)
#> [1] 25 45 48
# rule as a character vector
which_iter(x, '> 60')
#> [1] 25 45 48
# rule has use & and |
which_iter(x, '> 50 & < 60')
#> [1]  8 26 37 44 49
which_iter(x, '< -40 | > 40')
#>  [1]  1  4  6  8 14 18 25 26 30 33 36 37 43 44 45 48 49
```
