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
#>  -1.865  -0.469   0.021   0.135   0.860   3.257   0.000 


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
#>   -2.54   -0.65   -0.19   -0.12    0.53    2.28    0.00 
```
