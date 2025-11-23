<div id="main" class="col-md-9" role="main">

# Class FLCohort

<div class="ref-description section level2">

A class for modelling cohorts.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLCohort(object, ...)

# S4 method for class 'FLQuant'
FLCohort(object, ...)

# S4 method for class 'FLCohort'
FLCohort(object, units = units(object))

# S4 method for class 'array'
FLCohort(
  object,
  dim = rep(1, 6),
  dimnames = "missing",
  units = "NA",
  iter = 1,
  fill.iter = TRUE
)

# S4 method for class 'vector'
FLCohort(
  object,
  dim = c(length(object), rep(1, 5)),
  dimnames = "missing",
  units = "NA",
  iter = 1
)

# S4 method for class 'missing'
FLCohort(object, dim = rep(1, 6), dimnames = "missing", units = "NA", iter = 1)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    Input numeric object

-   ...:

    Additonal arguments

</div>

<div class="section level2">

## Details

This class represents cohorts in columns. It simply shifts the typical
matrix representation where cohorts are found on the diagonals, into a
matrix where cohorts are found in columns. It is very usefull for all
analysis that want to make use of cohorts instead of years.

</div>

<div class="section level2">

## Slots

-   .Data:

    Internal S4 data representation. `array`.

-   units:

    The data units in some understandable metric. `character`

</div>

<div class="section level2">

## Constructor

Objects of this class are generally constructed from an
[FLQuant](FLQuant.md) object.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[\[](https://rdrr.io/r/base/Extract.html),
[as.data.frame](https://rdrr.io/r/base/as.data.frame.html),
[bubbles](bubbles.md), ccplot, FLCohort,FLQuant-method, flc2flq,
[plot](https://rdrr.io/r/graphics/plot.default.html), [quant](quant.md),
[trim](trim.md), [units](https://rdrr.io/r/base/units.html),
units\<-,FLCohort,character-method,
[xyplot](https://rdrr.io/pkg/lattice/man/xyplot.html),
[array](https://rdrr.io/r/base/array.html)

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
flq <- catch.n(ple4)
flc <- FLCohort(flq)
plot(trim(flc, cohort=1960:2000))

```

</div>

</div>

</div>
