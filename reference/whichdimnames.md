# whichdimnames

Given a names array (an FLQuAnt or FLPar) `x`, find the positions for
which `which(x, arr.ind = TRUE)` is `TRUE` (or non-zero) and return a
data.frame with the corresponding dimnames values for each dimension.

For each matching position the returned data.frame contains one column
per array dimension. Column names are taken from the array's dimnames.

## Usage

``` r
whichdimnames(x)
```

## Arguments

- x:

  An array-based object (FLQuant, FLPar). Values selected are those for
  which `which(x, arr.ind = TRUE)` returns rows.

## Value

A data.frame with one row per selected element of `x` and one column per
array dimension. Each cell contains the dimname (character) for that
dimension at the selected index.

## Details

Extract dimnames values for positions returned by a logical estatement

## See also

[`which`](https://rdrr.io/r/base/which.html)

## Examples

``` r
# lnorm FLQuant
tes <- rlnorm(20, FLQuant(0, dim=c(1, 10)))
# dimnames of values > 5
whichdimnames(tes > 5)
#>    quant year   unit season   area iter
#> 1    all    4 unique    all unique    1
#> 2    all    8 unique    all unique    1
#> 3    all    4 unique    all unique    2
#> 4    all   10 unique    all unique    4
#> 5    all    1 unique    all unique    6
#> 6    all    5 unique    all unique    8
#> 7    all    9 unique    all unique    8
#> 8    all    2 unique    all unique   10
#> 9    all    3 unique    all unique   13
#> 10   all    7 unique    all unique   14
#> 11   all    8 unique    all unique   15
#> 12   all   10 unique    all unique   15
#> 13   all    8 unique    all unique   16
#> 14   all    4 unique    all unique   19
```
