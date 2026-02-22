# Class FLModel

A virtual class for statistical models

## Usage

``` r
FLModel(model, ...)
```

## Details

The `FLModel` class provides a virtual class that developers of various
statistical models can use to implement classes that allow those models
to be tested, fitted and presented.

Slots in this class attempt to map all the usual outputs for a modelling
exercise, together with the standard inputs. Input data are stored in
slots created by a specified class based on `FLModel`. See for example
[`FLSR`](FLSR.md) for a class used for stock-recruitment models.

The `initial` slot contains a function used to obtain initial values for
the numerical solver. It can also contain two attributes, `upper` and
`lower` that limit the sarch area for each parameter.

Various fitting algorithms, similar to those present in the basic R
packages, are currently available for `FLModel`, including
[`fmle`](fmle-methods.md), `nls-FLCore` and
[`glm`](https://rdrr.io/r/stats/glm.html).

## Slots

- name:

  Name of the object, `character`.

- desc:

  Description of the object, `character`.

- range:

  Range, `numeric`.

- distribution:

  Associated error probability dfistribution, `factor`.

- fitted:

  Estimated values, `FLQuant`.

- residuals:

  Residuals obtained from the model fit, `FLQuant`.

- model:

  Model formula, `formula`.

- gr:

  Function returning the gradient of the likelihood, `function`.

- logl:

  Log-likelihood function. `function`.

- initial:

  Function returning initial parameter values for the optimizer, as an
  object of class `FLPar`, `function`.

- params:

  Estimated parameter values, `FLPar`.

- logLik:

  Value of the log-likelihood, `logLik`.

- vcov:

  Variance-covariance matrix, `array`.

- hessian:

  Hessian matrix obtained from the parameter fitting, `array`.

- details:

  extra information on the model fit procedure, `list`.

## See also

[AIC](https://rdrr.io/r/stats/AIC.html),
[BIC](https://rdrr.io/r/stats/AIC.html), [fmle](fmle-methods.md),
[nls](https://rdrr.io/r/stats/nls.html), [FLComp](FLComp.md)

## Author

The FLR Team

## Examples

``` r
# Normally, FLModel objects won't be created if "class" is not set
  summary(FLModel(length~width*alpha))
#> An object of class "FLModel"
#> 
#> Name:  
#> Description:  
#> Quant: quant 
#> Dims:  quant     year    unit    season  area    iter
#>  1   1   1   1   1   1   
#> 
#> Range:  min  max minyear maxyear 
#>  NA  NA  1   1   
#> 
#> 
#> Model:   length ~ width * alpha
#> An object of class "FLPar"
#> params
#> length  width  alpha 
#>     NA     NA     NA 
#> units:  NA 
#> Log-likelihood:  NA(NA) 
#> Variance-covariance:         params
#> params   length width alpha
#>   length     NA    NA    NA
#>   width      NA    NA    NA
#>   alpha      NA    NA    NA

# Objects of FLModel-based classes use their own constructor,
# which internally calls FLModel
  fsr <- FLModel(rec~ssb*a, class='FLSR')
  is(fsr)
#> [1] "FLSR"    "FLModel" "FLComp" 
  summary(fsr)
#> An object of class "FLSR"
#> 
#> Name:  
#> Description:  
#> Quant: quant 
#> Dims:  quant     year    unit    season  area    iter
#>  1   1   1   1   1   1   
#> 
#> Range:  min  max minyear maxyear 
#>  NA  NA  1   1   
#> 
#> 
#> Model:   rec ~ ssb * a
#> An object of class "FLPar"
#> params
#>  a 
#> NA 
#> units:  NA 
#> Log-likelihood:  NA(NA) 
#> Variance-covariance:       params
#> params  a
#>      a NA

# An example constructor method for an FLModel-based class
  # Create class FLGrowth with a single new slot, 'mass'
    setClass('FLGrowth', representation('FLModel', mass='FLArray'))

  # Define a creator method based on FLModel
     setGeneric("FLGrowth", function(object, ...) standardGeneric("FLGrowth"))
#> [1] "FLGrowth"
    setMethod('FLGrowth', signature(object='ANY'),
      function(object, ...) return(FLModel(object, ..., class='FLGrowth')))
    setMethod('FLGrowth', signature(object='missing'),
      function(...) return(FLModel(formula(NULL), ..., class='FLGrowth')))

  # Define an accessor method
    setMethod('mass', signature(object='FLGrowth'),
      function(object) return(slot(object, 'mass')))
```
