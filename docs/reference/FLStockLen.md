<div id="main" class="col-md-9" role="main">

# Class FLStockLen

<div class="ref-description section level2">

A class for modelling a length-structured fish stock.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLStockLen(object, ...)

# S4 method for class 'FLQuant'
FLStockLen(object, ...)

# S4 method for class 'missing'
FLStockLen(object, ...)
```

</div>

</div>

<div class="section level2">

## Details

The `FLStockLen` object contains a length based representation of a fish
stock. This includes information on removals (i.e. catches, landings and
discards), maturity, natural mortality and the results of an analytical
assessment (i.e. estimates of abundance and removal rates).

</div>

<div class="section level2">

## Slots

-   halfwidth:

    The middle of the length bins (`numeric`).

-   catch:

    Total catch weight (`FLQuant`).

-   catch.n:

    Catch numbers (`FLQuant`).

-   catch.wt:

    Mean catch weights (`FLQuant`).

-   discards:

    Total discards weight (`FLQuant`).

-   discards.n:

    Discard numbers (`FLQuant`).

-   discards.wt:

    Mean discard weights (`FLQuant`).

-   landings:

    Total landings weight (`FLQuant`).

-   landings.n:

    Landing numbers (`FLQuant`).

-   landings.wt:

    Landing weights (`FLQuant`).

-   stock:

    Total stock weight (`FLQuant`).

-   stock.n:

    Stock numbers (`FLQuant`).

-   stock.wt:

    Mean stock weights (`FLQuant`).

-   m:

    Natural mortality (`FLQuant`).

-   mat:

    Proportion mature (`FLQuant`).

-   harvest:

    Harvest rate or fishing mortality. The units of this slot should be
    set to 'harvest' or 'f' accordingly (`FLQuant`).

-   harvest.spwn:

    Proportion of harvest/fishing mortality before spawning (`FLQuant`).

-   m.spwn:

    Proportion of natural mortality before spawning (`FLQuant`).

-   name:

    Name of the stock (`character`).

-   desc:

    Description of the stock (`character`).

-   range:

    Named numeric vector containing the quant and year ranges, the
    plusgroup and the quant range that the average fishing mortality
    should be calculated over (`numeric`).

</div>

<div class="section level2">

## See also

<div class="dont-index">

[\[](https://rdrr.io/r/base/Extract.html), \[\<-, as.FLBiol, as.FLSR,
[computeCatch](compute.md), [computeDiscards](compute.md),
[computeLandings](compute.md),
[plot](https://rdrr.io/r/graphics/plot.default.html), [ssb](ssb.md),
ssbpurec, [trim](trim.md), [FLComp](FLComp.md)

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
stkl <- FLStockLen(m=FLQuant(0.2, dimnames=list(len=seq(5, 50, by=2), year=2015:2020)))
summary(stkl)
#> An object of class "FLStockLen"
#> 
#> Name:  
#> Description:  
#> Quant: len 
#> Dims:  len   year    unit    season  area    iter
#>  23  6   1   1   1   1   
#> 
#> Range:  min  max minyear maxyear minfbar maxfbar 
#>  5   49  2015    2020    5   49  
#> 
# Unnamed FLQuant used for sizing
stkl <- FLStockLen(FLQuant(0.2, dimnames=list(len=seq(5, 50, by=2), year=2015:2020)))
summary(stkl)
#> An object of class "FLStockLen"
#> 
#> Name:  
#> Description:  
#> Quant: len 
#> Dims:  len   year    unit    season  area    iter
#>  23  6   1   1   1   1   
#> 
#> Range:  min  max minyear maxyear minfbar maxfbar 
#>  5   49  2015    2020    5   49  
#> 
m(stkl)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> len  2015 2016 2017 2018 2019 2020
#>   5  NA   NA   NA   NA   NA   NA  
#>   7  NA   NA   NA   NA   NA   NA  
#>   9  NA   NA   NA   NA   NA   NA  
#>   11 NA   NA   NA   NA   NA   NA  
#>   13 NA   NA   NA   NA   NA   NA  
#>   15 NA   NA   NA   NA   NA   NA  
#>   17 NA   NA   NA   NA   NA   NA  
#>   19 NA   NA   NA   NA   NA   NA  
#>   21 NA   NA   NA   NA   NA   NA  
#>   23 NA   NA   NA   NA   NA   NA  
#>   25 NA   NA   NA   NA   NA   NA  
#>   27 NA   NA   NA   NA   NA   NA  
#>   29 NA   NA   NA   NA   NA   NA  
#>   31 NA   NA   NA   NA   NA   NA  
#>   33 NA   NA   NA   NA   NA   NA  
#>   35 NA   NA   NA   NA   NA   NA  
#>   37 NA   NA   NA   NA   NA   NA  
#>   39 NA   NA   NA   NA   NA   NA  
#>   41 NA   NA   NA   NA   NA   NA  
#>   43 NA   NA   NA   NA   NA   NA  
#>   45 NA   NA   NA   NA   NA   NA  
#>   47 NA   NA   NA   NA   NA   NA  
#>   49 NA   NA   NA   NA   NA   NA  
#> 
#> units:  len 
```

</div>

</div>

</div>
