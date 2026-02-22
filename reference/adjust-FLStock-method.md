# Recalculate to adjust abundances to F and M

An FLStock object is projected forward using the initial abundances and
the total mortality-at-age per timestep. New values for the stock.n and
catch.n slots are calculated, assuming that harvest and m are correct.
This calculation provides a test of the internal consistency of the
object.

## Usage

``` r
# S4 method for class 'FLStock'
adjust(object)
```

## Arguments

- object:

  an `FLStock` object

## Value

`FLStock` object

## See also

[`harvest`](accessors.md)

## Examples

``` r
data(ple4)
test <- adjust(ple4)
# Difference in catch due to estimation error
plot(FLStocks(PLE=ple4, TEST=test))
```
