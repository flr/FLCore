# Estimate AR(1) Parameters from an FLQuant Object

Fits an AR(1) model to each iteration of an `FLQuant` object and returns
the estimated standard deviation and autocorrelation coefficient from
the AR process.

## Usage

``` r
ar1pars(x)
```

## Arguments

- x:

  An `FLQuant` object with a time series.

## Value

An `FLPar` object with two parameters:

- `sd`:

  Residual standard deviation, computed as the square root of the
  estimated innovation variance (`sigma2`) from the AR(1) fit.

- `rho`:

  AR(1) autocorrelation coefficient.

## Details

For each iteration in `x`, an `ARIMA(1, 0, 0)` model is fitted via
[`arima`](https://rdrr.io/r/stats/arima.html). The innovation standard
deviation (`sd`) and the AR(1) coefficient (`rho`) are extracted and
assembled into a new `FLPar`.

## See also

[`arima`](https://rdrr.io/r/stats/arima.html), [`FLQuant`](FLQuant.md)

## Examples

``` r
# Example dataset
data(ple4)
# Compute AR1 parameters on catch
ar1pars(catch(ple4))
#> Warning: possible convergence problem: optim gave code = 1
#> An object of class "FLPar"
#> params
#>       sd      rho 
#> 1.49e+04 9.94e-01 
#> units:    
```
