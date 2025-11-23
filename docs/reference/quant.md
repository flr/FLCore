<div id="main" class="col-md-9" role="main">

# Method quant

<div class="ref-description section level2">

Function to get or set the name of the first dimension (quant) in an
object of any FLArray-based class, like `FLQuant` or `FLCohort`.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
quant(object, ...)

# S4 method for class 'FLArray'
quant(object)

# S4 method for class 'FLArray,character'
quant(object) <- value
```

</div>

</div>

<div class="section level2">

## Generic function

quant(object) quant\<-(object,value)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLQuant](FLQuant.md), [FLCohort](FLCohort.md)

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
# quant is 'quant' by default
  quant(FLQuant())
#> [1] "quant"

flq <- FLQuant(rnorm(80), dim=c(4,20), quant='age')
quant(flq)
#> [1] "age"
quant(flq) <- 'length'
summary(flq)
#> An object of class "FLQuant" with:
#> 
#> dim:    length year unit season area iter 
#>         4      20   1    1      1    1 
#> units:  NA 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    %NAs 
#> -1.9681 -0.7670 -0.0917 -0.0032  0.8263  2.2558  0.0000 


# quant is 'quant' by default
  quant(FLQuant())
#> [1] "quant"

flq <- FLQuant(rnorm(80), dim=c(4,20), quant='age')
quant(flq)
#> [1] "age"
quant(flq) <- 'length'
summary(flq)
#> An object of class "FLQuant" with:
#> 
#> dim:    length year unit season area iter 
#>         4      20   1    1      1    1 
#> units:  NA 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    %NAs 
#>  -2.460  -0.758  -0.011  -0.080   0.473   1.998   0.000 
```

</div>

</div>

</div>
