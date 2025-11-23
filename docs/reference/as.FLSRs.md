<div id="main" class="col-md-9" role="main">

# Convert an FLStock into a list of one or FLSR objects.

<div class="ref-description section level2">

A single `FLStock` can be coerced into a list with one or more objects
of class `FLSR`, each of them typically set to a diefferemt
stock-recruit model.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
as.FLSRs(x, models = NULL, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    An estimated FLStock object to coerce.

-   models:

    Name(s) of model(s) to fit.

-   ...:

    Any extra arguments to be passed to *as.FLSR*.

</div>

<div class="section level2">

## Value

An objecdt of class `FLSRs`

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLSRs](FLSRs.md) [FLSRs](FLSRs.md) `as.FLSR()`

</div>

</div>

<div class="section level2">

## Author

FLR Team, 2023.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
as.FLSRs(ple4, model=c("bevholt", "segreg"))
#> An object of class "FLSRs"
#> [[1]]
#> An object of class "FLSR"
#> 
#> Name: PLE 
#> Description: 'rec' and 'ssb' slots obtained from a 'FLStock' object 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  1   60  1   1   1   1   
#> 
#> Range:  min  minyear max maxyear 
#>  1   1958    1   2017    
#> 
#> 
#> Model:   rec ~ a * ssb/(b + ssb)
#> An object of class "FLPar"
#> params
#>  a  b 
#> NA NA 
#> units:  NA 
#> Log-likelihood:  NA(NA) 
#> Variance-covariance:       params
#> params  a  b
#>      a NA NA
#>      b NA NA
#> 
#> [[2]]
#> An object of class "FLSR"
#> 
#> Name: PLE 
#> Description: 'rec' and 'ssb' slots obtained from a 'FLStock' object 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  1   60  1   1   1   1   
#> 
#> Range:  min  minyear max maxyear 
#>  1   1958    1   2017    
#> 
#> 
#> Model:   rec ~ ifelse(ssb <= b, a * ssb, a * b)
#> An object of class "FLPar"
#> params
#>  a  b 
#> NA NA 
#> units:  NA 
#> Log-likelihood:  NA(NA) 
#> Variance-covariance:       params
#> params  a  b
#>      a NA NA
#>      b NA NA
#> 
#> Slot "names":
#> [1] "bevholt" "segreg" 
#> 
#> Slot "desc":
#> character(0)
#> 
#> Slot "lock":
#> [1] FALSE
#> 
```

</div>

</div>

</div>
