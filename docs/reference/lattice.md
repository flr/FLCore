<div id="main" class="col-md-9" role="main">

# Lattice methods

<div class="ref-description section level2">

Implementation of Trellis graphics in FLR

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'formula,FLQuant'
xyplot(x, data, ...)

# S4 method for class 'formula,FLCohort'
xyplot(x, data, ...)

# S4 method for class 'formula,FLQuants'
xyplot(x, data, ...)

# S4 method for class 'formula,FLComp'
xyplot(x, data, ...)

# S4 method for class 'formula,FLQuant'
bwplot(x, data, ...)

# S4 method for class 'formula,FLComp'
bwplot(x, data, ...)

# S4 method for class 'formula,FLQuant'
dotplot(x, data, ...)

# S4 method for class 'formula,FLComp'
dotplot(x, data, ...)

# S4 method for class 'formula,FLQuant'
barchart(x, data, ...)

# S4 method for class 'formula,FLComp'
barchart(x, data, ...)

# S4 method for class 'formula,FLQuant'
stripplot(x, data, ...)

# S4 method for class 'formula,FLComp'
stripplot(x, data, ...)

# S4 method for class 'formula,FLQuant'
histogram(x, data, ...)

# S4 method for class 'formula,FLComp'
histogram(x, data, ...)

# S4 method for class 'formula,FLQuants'
histogram(x, data, ...)

# S4 method for class 'formula,FLPar'
densityplot(x, data, ...)
```

</div>

</div>

<div class="section level2">

## Details

Plot methods in the `lattice` package are available for an object of
classes `FLQuant`, `FLQuants` or those derived from `FLComp`.

See the help page in `lattice` for a full description of each plot
method and all possible arguments.

Plot methods from lattice are called by passing a
[data.frame](https://rdrr.io/r/base/data.frame.html) obtained by
converting the FLR objects using as.data.frame. For details on this
transformation, see as.data.frame-FLCore.

</div>

<div class="section level2">

## Generic function

barchart(x, data, ...)

bwplot(x, data, ...)

densityplot(x, data, ...)

dotplot(x, data, ...)

histogram(x, data, ...)

stripplot(x, data, ...)

xyplot(x, data, ...)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[xyplot](https://rdrr.io/pkg/lattice/man/xyplot.html),
[barchart](https://rdrr.io/pkg/lattice/man/xyplot.html),
[bwplot](https://rdrr.io/pkg/lattice/man/xyplot.html),
[densityplot](https://rdrr.io/pkg/lattice/man/histogram.html),
[dotplot](https://rdrr.io/pkg/lattice/man/xyplot.html),
[histogram](https://rdrr.io/pkg/lattice/man/histogram.html),
[stripplot](https://rdrr.io/pkg/lattice/man/xyplot.html)

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
data(ple4)
# xyplot on FLQuant
  xyplot(data~year|age, catch.n(ple4)[, 1:20])

  xyplot(data~year|as.factor(age), catch.n(ple4)[, 1:20], type='b', pch=19,
    cex=0.5)


# bwplot on FLQuant with iter...
  flq <- rnorm(100, catch.n(ple4)[, 1:20], catch.n(ple4)[,1:20])
  bwplot(data~year|as.factor(age), flq)

# ...now with same style modifications
  bwplot(data~year|as.factor(age), flq, scales=list(relation='free',
    x=list(at=seq(1, 20, by=5),
    labels=dimnames(catch.n(ple4)[,1:20])$year[seq(1, 20, by=5)])),
    cex=0.5, strip=strip.custom(strip.names=TRUE, strip.levels=TRUE,
    var.name='age'))

```

</div>

</div>

</div>
