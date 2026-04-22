# Fit an FLModel by maximum likelihood

Fit an `FLModel` object by maximising its log-likelihood using `optim`
(the `mle` S4 method). The method builds the data list from the model
object's slots, handles fixed parameters, supports iterative fitting
over the `iter` dimension, computes the variance-covariance matrix and
(negative) Hessian when available, and stores results back into the
model slots (`params`, `logLik`, `vcov`, `hessian`, `details`, `fitted`,
and `residuals`).

## Usage

``` r
# S4 method for class 'FLModel,FLPar'
fmle(
  object,
  start,
  method = "Nelder-Mead",
  fixed = list(),
  control = list(trace = 1),
  lower = rep(-Inf, dim(params(object))[2]),
  upper = rep(Inf, dim(params(object))[2]),
  ...
)
```

## Arguments

- object:

  An object of class `FLModel`.

- start:

  Initial parameter values. Can be an [FLPar](FLPar.md) (handled by the
  `FLPar` method) or a named list / numeric vector of starting values.
  If missing and `object@initial` is a function, that function will be
  used to produce starting values.

- method:

  Optimization method passed to `optim`. Default is `"Nelder-Mead"`.

- fixed:

  A named list of parameters to hold fixed during optimization. Elements
  may be length 1 (scalar) or length equal to the number of iterations;
  names must match parameter names expected by the model log-likelihood.

- control:

  A list of control parameters passed to `optim` (default
  `list(trace=1)`).

- lower, upper:

  Numeric vectors of lower and upper parameter bounds. These are used
  when `method` is `"L-BFGS-B"` or `"Brent"`. If missing and the model
  provides bounds, `lower(object)` or `upper(object)` will be used.

- ...:

  Additional arguments passed to internal calls (for example to `fmle`
  when invoked programmatically).

- seq.iter:

  Logical; if `TRUE` (default) the method will fit across iterations
  (the object's `iter` dimension) and return per-iteration results. If
  `FALSE`, fitting is performed once.

- preconvert:

  Logical; if `TRUE` the data objects are converted (via `c`) prior to
  calling the log-likelihood, which can speed up calls for some models.
  Default is `FALSE`.

- model:

  Optional character or function to reset the model definition (i.e.
  replace `object@model`) before fitting.

## Value

The input `FLModel` object with updated slots: `params`, `logLik`,
`vcov`, `hessian`, `details`, `fitted` and `residuals`.

## Details

Two method signatures are provided: one accepting an [FLPar](FLPar.md)
for `start` and a more general one accepting lists or named vectors.

The fmle method:

- Matches formals of the model `logl` function to object slots and
  supplied data to build the call environment.

- Accepts fixed parameters via the `fixed` argument and removes them
  from the optimisation vector.

- If the model contains an `iter` dimension and `seq.iter=TRUE`, the
  method performs optimisation for each iteration and stores results in
  the corresponding iteration slices of `params`, `vcov`, `hessian` and
  `logLik`.

- Computes the variance-covariance matrix as the inverse of the Hessian
  when available (silent on singular/invertible checks) and stores the
  negative Hessian in `@hessian`.

- On successful optimisation the fitted values and residuals are
  computed via `predict(object)` and stored in `@fitted` and
  `@residuals`. The optimisation `details` (call, value, counts,
  convergence, message) are stored in `@details`.

The method relies on the presence of a valid `logl` function in the
model definition (accessible as `object@logl`). The names of parameters
in `start` and `fixed` must match the argument names of `logl`.

## See also

[optim](https://rdrr.io/r/stats/optim.html),
[logLik](https://rdrr.io/r/stats/logLik.html),
[predict](https://rdrr.io/r/stats/predict.html), [FLPar](FLPar.md)

## Author

The FLR Team

## Examples

``` r
# Load the NS herring SRR dataset
data(nsher)
# Using an FLPar start
fmle(nsher)
#>   Nelder-Mead direct search function minimizer
#> function value for initial parameters = -15.862252
#>   Scaled convergence tolerance is 2.36366e-07
#> Stepsize computed as 11.939303
#> BUILD              3 10000000000000000159028911097599180468360808563945281389781327557747838772170381060813469985856815104.000000 -15.862252
#> SHRINK             7 10000000000000000159028911097599180468360808563945281389781327557747838772170381060813469985856815104.000000 -15.862252
#> HI-REDUCTION       9 267.632807 -15.862252
#> HI-REDUCTION      11 236.438352 -15.862252
#> HI-REDUCTION      13 205.241529 -15.862252
#> HI-REDUCTION      15 174.041331 -15.862252
#> HI-REDUCTION      17 142.839861 -15.862252
#> HI-REDUCTION      19 111.657775 -15.862252
#> HI-REDUCTION      21 80.601711 -15.862252
#> HI-REDUCTION      23 50.133420 -15.862252
#> HI-REDUCTION      25 21.954719 -15.862252
#> HI-REDUCTION      27 0.340450 -15.862252
#> HI-REDUCTION      29 -10.920787 -15.862252
#> HI-REDUCTION      31 -14.696800 -15.862252
#> HI-REDUCTION      33 -15.639120 -15.862252
#> HI-REDUCTION      35 -15.676851 -15.862252
#> HI-REDUCTION      37 -15.816477 -15.862252
#> HI-REDUCTION      39 -15.825739 -15.862252
#> LO-REDUCTION      41 -15.847989 -15.862252
#> LO-REDUCTION      43 -15.852490 -15.862252
#> LO-REDUCTION      45 -15.857566 -15.862252
#> LO-REDUCTION      47 -15.860412 -15.862252
#> LO-REDUCTION      49 -15.861062 -15.862252
#> LO-REDUCTION      51 -15.861647 -15.862252
#> LO-REDUCTION      53 -15.862025 -15.862252
#> LO-REDUCTION      55 -15.862108 -15.862252
#> LO-REDUCTION      57 -15.862175 -15.862252
#> LO-REDUCTION      59 -15.862224 -15.862252
#> LO-REDUCTION      61 -15.862235 -15.862252
#> LO-REDUCTION      63 -15.862243 -15.862252
#> LO-REDUCTION      65 -15.862249 -15.862252
#> LO-REDUCTION      67 -15.862250 -15.862252
#> LO-REDUCTION      69 -15.862251 -15.862252
#> LO-REDUCTION      71 -15.862252 -15.862252
#> LO-REDUCTION      73 -15.862252 -15.862252
#> Exiting from Nelder Mead minimizer
#>     75 function evaluations used
#> An object of class "FLSR"
#> 
#> Name:  
#> Description:  
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  1   45  1   1   1   1   
#> 
#> Range:  min  minyear max maxyear 
#>  0   1960    0   2004    
#> 
#> 
#> Model:   rec ~ a * ssb * exp(-b * ssb)
#> An object of class "FLPar"
#> params
#>        a        b 
#> 1.19e+02 9.45e-03 
#> units:  NA 
#> Log-likelihood:  15.862(0) 
#> Variance-covariance:    
#>                a            b
#>   a 255.33855758 1.808868e-02
#>   b   0.01808868 1.992658e-06

# Fixing a parameter
fmle(nsher, fixed=list(b=8e-3))
#> Warning: one-dimensional optimization by Nelder-Mead is unreliable:
#> use "Brent" or optimize() directly
#>   Nelder-Mead direct search function minimizer
#> function value for initial parameters = -14.338303
#>   Scaled convergence tolerance is 2.13657e-07
#> Stepsize computed as 11.939303
#> BUILD              2 -12.191949 -14.338303
#> REFLECTION         4 -14.338303 -15.303027
#> LO-REDUCTION       6 -15.129796 -15.303027
#> HI-REDUCTION       8 -15.279627 -15.303027
#> HI-REDUCTION      10 -15.303027 -15.306538
#> HI-REDUCTION      12 -15.306538 -15.308507
#> HI-REDUCTION      14 -15.308464 -15.308507
#> HI-REDUCTION      16 -15.308507 -15.308720
#> HI-REDUCTION      18 -15.308672 -15.308720
#> HI-REDUCTION      20 -15.308710 -15.308720
#> HI-REDUCTION      22 -15.308719 -15.308720
#> HI-REDUCTION      24 -15.308720 -15.308720
#> Exiting from Nelder Mead minimizer
#>     26 function evaluations used
#> An object of class "FLSR"
#> 
#> Name:  
#> Description:  
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  1   45  1   1   1   1   
#> 
#> Range:  min  minyear max maxyear 
#>  0   1960    0   2004    
#> 
#> 
#> Model:   rec ~ a * ssb * exp(-b * ssb)
#> An object of class "FLPar"
#> params
#>       a       b 
#> 106.527   0.008 
#> units:  NA 
#> Log-likelihood:  15.309(0) 
#> Variance-covariance:    
#>            a
#>   a 74.35688
```
