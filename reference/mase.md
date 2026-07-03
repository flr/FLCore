# Compute mean absolute scaled error (MASE)

Computes the Mean Absolute Scaled Error (MASE) between a reference
(naive) prediction and one or more alternative predictions. MASE is
scale-independent and robust to outliers, making it useful for comparing
forecast accuracy across different indices or time series.

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

  Reference or naive prediction, an `FLQuant` or `FLIndices` time
  series.

- preds:

  Predictions to compare to the reference; an `FLQuants` or a list of
  `FLIndices`.

- ...:

  Additional arguments passed to methods.

- order:

  Character; whether predictions are in `"inverse"` (default, most
  recent first) or `"ahead"` order.

- wt:

  Mean weights-at-age to use with indices.

## Value

A numeric scalar (or named numeric vector for the `FLIndices, list`
method) giving the MASE value(s).

## Generic function

mase(ref, preds, ...)

## References

Franses, P.H. (2016). A note on the Mean Absolute Scaled Error.
*International Journal of Forecasting*, 32(1):20–22.
[doi:10.1016/j.ijforecast.2015.03.008](https://doi.org/10.1016/j.ijforecast.2015.03.008)
.

## See also

[runstest](runstest.md)

## Author

The FLR Team
