# Compute mean absolute scaled error (MASE)

Franses, PH. "A note on the Mean Absolute Scaled Error". International
Journal of Forecasting. 32 (1): 20–22.
doi:10.1016/j.ijforecast.2015.03.008.

## Usage

``` r
mase(ref, preds, ...)

# S4 method for class 'FLQuant,FLQuants'
mase(ref, preds, order = c("inverse", "ahead"))

# S4 method for class 'FLIndices,list'
mase(ref, preds, order = "inverse", wt = "missing")
```

## Arguments

- ref:

  Reference or naive prediction.

- preds:

  Predicitions to compare to reference.

- ...:

  Extra arguments.

- order:

  Are predictions in 'inverse' (default) or 'ahead' order.

- wt:

  Mean weights-at-age to use with indices.

## Value

A numeric vector of the same length as 'preds'.
