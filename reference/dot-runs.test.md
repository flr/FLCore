# Runs test for randomness

Internal implementation of the runs test for randomness. Performs the
runs test against a threshold value, returning a list of class `"htest"`
with the test statistic, p-value, and related diagnostics. This function
is called internally by [sigma3](sigma3.md).

## Usage

``` r
.runs.test(
  x,
  alternative = "two.sided",
  threshold = median(x),
  pvalue = "normal",
  plot = FALSE
)
```

## Arguments

- x:

  A numeric vector containing the data.

- alternative:

  Character; the alternative hypothesis. One of `"two.sided"` (default),
  `"left.sided"`, or `"right.sided"` (abbreviations `"t"`, `"l"`, `"r"`
  are accepted).

- threshold:

  Numeric; the threshold used to dichotomise `x` into above/below;
  defaults to `median(x)`.

- pvalue:

  Character; method to compute p-values; `"normal"` (default, normal
  approximation) or `"exact"` (exact distribution).

- plot:

  Logical; reserved for future use; currently ignored.

## Value

A list of class `"htest"` with elements:

- statistic:

  Normalised test statistic.

- p.value:

  Asymptotic (or exact) p-value.

- runs:

  Total number of runs.

- mu:

  Expected number of runs under the null.

- var:

  Variance of the number of runs under the null.

- method:

  Character description of the test.

- data.name:

  Name of the data object.

- parameter:

  Named vector with runs, n1, n2, n.

- alternative:

  Description of the alternative hypothesis.

## See also

[sigma3](sigma3.md), [runstest](runstest.md)

## Author

The FLR Team
