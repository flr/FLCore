<div id="main" class="col-md-9" role="main">

# Recalculate to adjust abundances to F and M

<div class="ref-description section level2">

An FLStock object is projected forward using the initial abundances and
the total mortality-at-age per timestep. New values for the stock.n and
catch.n slots are calculated, assuming that harvest and m are correct.
This calculation provides a test of the internal consistency of the
object.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLStock'
adjust(object)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    an `FLStock` object

</div>

<div class="section level2">

## Value

`FLStock` object

</div>

<div class="section level2">

## See also

<div class="dont-index">

`harvest`

</div>

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
test <- adjust(ple4)
# Difference in catch due to estimation error
plot(FLStocks(PLE=ple4, TEST=test))
```

</div>

</div>

</div>
