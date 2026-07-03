# Generate an autocorrelated noise series

Internal helper that generates a numeric vector of autocorrelated noise
following the AR(1) process described by Ranta and Kaitala (2001): \\v_t
= b \cdot v\_{t-1} + s_t \sqrt{1 - b^2}\\, where \\s_t \sim N(0,
\sigma^2)\\.

## Usage

``` r
noiseFn(len, sd = 1, b = 0, burn = 0, trunc = 0, seed = NA)
```

## Arguments

- len:

  Integer; length of the output vector (after burn-in removal).

- sd:

  Numeric; standard deviation of the innovations; defaults to 1.

- b:

  Numeric; autocorrelation parameter in \\\[-1, 1\]\\; 0 gives white
  noise; defaults to 0.

- burn:

  Integer; number of initial values to discard as burn-in; defaults to
  0.

- trunc:

  Numeric; if \> 0, values outside \\(-(1 - \mathrm{trunc}), 1 -
  \mathrm{trunc})\\ are truncated; defaults to 0 (no truncation).

- seed:

  Integer or `NA`; random seed passed to `set.seed`; if `NA` (default)
  the seed is not set.

## Value

A numeric vector of length `len` containing the simulated autocorrelated
deviates.

## References

Ranta, E. and Kaitala, V. (2001). Travelling waves in vole population
dynamics. *Proceedings of the Royal Society of London. Series B:
Biological Sciences*, 268(1474), 1595–1600.

## See also

[rnoise](rnoise.md), [rlnoise](rnoise.md)

## Author

The FLR Team
