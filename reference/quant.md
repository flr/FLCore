# Method quant

Function to get or set the name of the first dimension (quant) in an
object of any FLArray-based class, like [`FLQuant`](FLQuant.md) or
[`FLCohort`](FLCohort.md).

## Usage

``` r
quant(object, ...)

# S4 method for class 'FLArray'
quant(object)

# S4 method for class 'FLArray,character'
quant(object) <- value
```

## Generic function

quant(object) quant\<-(object,value)

## See also

[FLQuant](FLQuant.md), [FLCohort](FLCohort.md)

## Author

The FLR Team

## Examples

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
#>  -1.865  -0.519  -0.047   0.018   0.578   3.257   0.000 


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
#>  -2.543  -0.654  -0.164  -0.064   0.622   2.284   0.000 
```
