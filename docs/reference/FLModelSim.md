<div id="main" class="col-md-9" role="main">

# Class FLModelSim

<div class="ref-description section level2">

A virtual class for statistical simulation models

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLModelSim(object, ...)

# S4 method for class 'missing'
FLModelSim(object, ...)
```

</div>

</div>

<div class="section level2">

## Details

The `FLModelSim` class provides a virtual class that developers of
various statistical models can use to implement classes that allow those
models to be tested, fitted and presented.

Slots in this class attempt to map all the usual outputs for a modelling
exercise, together with the standard inputs. Input data are stored in
slots created by a specified class that is based on `FLModelSim`. See
for example `FLSR` for a class used for stock-recruitment models.

Various fitting algorithms, similar to those present in the basic R
packages, are currently available for `FLModelSim`, including `fmle`,
`nls-FLCore` and `glm`.

</div>

<div class="section level2">

## Slots

-   params:

    Estimated parameter values. `FLPar`.

-   distr:

    `character`

-   vcov:

    `array`

-   model:

    `formula`

</div>

<div class="section level2">

## See also

<div class="dont-index">

[AIC](https://rdrr.io/r/stats/AIC.html),
[BIC](https://rdrr.io/r/stats/AIC.html), fmle,
[nls](https://rdrr.io/r/stats/nls.html)

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

</div>
