# Class FLModelSim

A virtual class for statistical simulation models

## Usage

``` r
FLModelSim(object, ...)

# S4 method for class 'missing'
FLModelSim(object, ...)
```

## Details

The `FLModelSim` class provides a virtual class that developers of
various statistical models can use to implement classes that allow those
models to be tested, fitted and presented.

Slots in this class attempt to map all the usual outputs for a modelling
exercise, together with the standard inputs. Input data are stored in
slots created by a specified class that is based on `FLModelSim`. See
for example [`FLSR`](FLSR.md) for a class used for stock-recruitment
models.

Various fitting algorithms, similar to those present in the basic R
packages, are currently available for `FLModelSim`, including
[`fmle`](fmle-methods.md), `nls-FLCore` and
[`glm`](https://rdrr.io/r/stats/glm.html).

## Slots

- params:

  Estimated parameter values. `FLPar`.

- distr:

  `character`

- vcov:

  `array`

- model:

  `formula`

## See also

[AIC](https://rdrr.io/r/stats/AIC.html),
[BIC](https://rdrr.io/r/stats/AIC.html), [fmle](fmle-methods.md),
[nls](https://rdrr.io/r/stats/nls.html)

## Author

The FLR Team
