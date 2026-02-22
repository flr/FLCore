# Method plot

Standard plot methods for every FLCore class. FLR plot methods are based
on [`lattice`](https://rdrr.io/pkg/lattice/man/Lattice.html), and
attempt to show a general view of the object contents.

## Usage

``` r
# S4 method for class 'FLQuant,missing'
plot(
  x,
  xlab = "year",
  ylab = paste("data (", units(x), ")", sep = ""),
  type = "p",
  ...
)

# S4 method for class 'FLStock,missing'
plot(x, auto.key = TRUE, ...)

# S4 method for class 'FLBiol,missing'
plot(x, y, ...)

# S4 method for class 'FLIndex,missing'
plot(x, type = c("splom"), ...)

# S4 method for class 'FLSR,missing'
plot(x, main = "Functional form", log.resid = FALSE, cex = 0.8)

# S4 method for class 'FLPar,missing'
plot(x, y = "missing", ...)
```

## Details

Users are encouraged to write their own plotting code and make use of
the overloaded [`lattice`](https://rdrr.io/pkg/lattice/man/Lattice.html)
methods, for example
[`xyplot`](https://rdrr.io/pkg/lattice/man/xyplot.html) or
[`bwplot`](https://rdrr.io/pkg/lattice/man/xyplot.html). See also
[`lattice-FLCore`](lattice.md).

## Generic function

plot(x,y)

## See also

[plot](https://rdrr.io/r/graphics/plot.default.html)

## Author

The FLR Team

## Examples

``` r
data(ple4)

# FLQuant
plot(catch.n(ple4)[, 1:20])

plot(catch.n(ple4)[, 1:20], type='b', pch=19, cex=0.5)


# FLStock
data(ple4sex)
plot(ple4)

plot(ple4sex)


# FLBiol
data(ple4.biol)
#> Warning: namespace ‘colorspace’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘Rcpp’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘dplyr’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘data.table’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘remotes’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘xtable’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘shiny’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘htmlwidgets’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
plot(ple4.biol)


# FLIndex
data(ple4.index)
plot(ple4.index)
#> Warning: coercing argument of type 'double' to logical


# FLSR
data(nsher)
plot(nsher)


# FLPar
fpa <- FLPar(a=rnorm(100, 1, 20), b=rlnorm(100, 0.5, 0.2))
plot(fpa)

```
