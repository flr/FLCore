<div id="main" class="col-md-9" role="main">

# Method summary

<div class="ref-description section level2">

Outputs a general summary of the structure and content of an fwdControl
object. The method invisibly returns the data.frame shown on screen.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

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

</div>

</div>

<div class="section level2">

## Generic function

summary(object)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[summary](https://rdrr.io/r/base/summary.html)

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
flq <- FLQuant(rlnorm(90), dim=c(3,10), units='kg')
summary(flq)
#> An object of class "FLQuant" with:
#> 
#> dim:    quant year unit season area iter 
#>         3     10   1    1      1    1 
#> units:  kg 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    %NAs 
#>    0.10    0.63    1.14    1.45    1.90    7.24    0.00 

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

</div>

</div>

</div>
