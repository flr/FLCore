# Method splom

Draws a conditional scatter plot matrix.

## Usage

``` r
# S4 method for class 'FLPar,missing'
splom(x, data, ...)
```

## Details

See the help page in
[`lattice`](https://rdrr.io/pkg/lattice/man/Lattice.html) for a full
description of each plot and all possible arguments.

## Generic function

splom(x,data)

## See also

[splom](https://rdrr.io/pkg/lattice/man/splom.html)

## Author

The FLR Team

## Examples

``` r
flp <- FLPar(c(t(mvrnorm(500, mu=c(0, 120, 0.01, 20),
  Sigma=matrix(.7, nrow=4, ncol=4) + diag(4) * 0.3))),
  dimnames=list(params=c('a','b','c','d'), iter=1:500), units="NA")

splom(flp)

```
