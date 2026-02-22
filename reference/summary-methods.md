# Method summary

Outputs a general summary of the structure and content of an fwdControl
object. The method invisibly returns the data.frame shown on screen.

## Usage

``` r
# S4 method for class 'FLArray'
summary(object, .class = TRUE, ...)

# S4 method for class 'FLQuantPoint'
summary(object, ...)

# S4 method for class 'FLPar'
summary(object, title = TRUE, ...)

# S4 method for class 'FLComp'
summary(object, ...)

# S4 method for class 'FLQuants'
summary(object)

# S4 method for class 'predictModel'
summary(object)

# S4 method for class 'FLBiol'
summary(object)

# S4 method for class 'FLModel'
summary(object, ...)

# S4 method for class 'FLlst'
summary(object)
```

## Generic function

summary(object)

## See also

[summary](https://rdrr.io/r/base/summary.html)

## Author

The FLR Team

## Examples

``` r
flq <- FLQuant(rlnorm(90), dim=c(3,10), units='kg')
summary(flq)
#> An object of class "FLQuant" with:
#> 
#> dim:    quant year unit season area iter 
#>         3     10   1    1      1    1 
#> units:  kg 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    %NAs 
#>   0.094   0.780   1.077   1.786   2.396   5.687   0.000 

data(ple4)
summary(ple4)
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   10  10  1957    2017    2   6   
#> 
#> Metrics: 
#>   rec: 367450 - 4303680  (1000) 
#>   ssb: 203391 - 913290  (t) 
#>   catch: 78360 - 315245  (t) 
#>   fbar: 0.20 - 0.72  (f) 

data(nsher)
summary(nsher)
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
#>               a            b
#>   a 255.3388181 1.808870e-02
#>   b   0.0180887 1.992659e-06
```
