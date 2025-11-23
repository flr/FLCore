<div id="main" class="col-md-9" role="main">

# Calculate next yera's SSB from survivors and Fbar

<div class="ref-description section level2">

The spawning stock biomass (SSB) of the stock gets calculated from the
survivors of the previous year. This provides a value for the first year
after the end of the object. Weights-at-age, maturity in this extra year
are calculated as averages over the last *wts.nyears*.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
ssb_next(x, fbar = 0, wts.nyears = 3, fbar.nyears = 3)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    An FLStock object containing estimates of abundance and harvesting.

-   fbar:

    The Fbar rate assumed on the extra year. Defaults to 0.

-   wts.nyears:

    Number of years in calculation of mean weight-at-age and maturity
    for the extra year.

-   fbar.nyears:

    Number of years in calculation of mean selectivity, natural
    mortality and fraction of F abnd M before spawning for the extra
    year.

</div>

<div class="section level2">

## Value

An FLQuant.

</div>

<div class="section level2">

## Details

For stocks spawning later in the year, a value for the average fishing
mortality, *fbar*, expected in that year can be provided. Mortality
until spawning is then calculated, with M and selectivity assumed in the
extra year to be an average of the last *fbar.nyears*.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
ssb_next(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1958   1959   1960   1961   1962   1963   1964   1965   1966   1967  
#>   all 355374 362120 380052 391385 482245 440658 430475 383583 404516 473937
#>      year
#> age   1968   1969   1970   1971   1972   1973   1974   1975   1976   1977  
#>   all 458977 402865 370473 361610 366129 302365 298094 301757 328726 329116
#>      year
#> age   1978   1979   1980   1981   1982   1983   1984   1985   1986   1987  
#>   all 327543 302271 319089 290780 284211 339238 367251 394666 407972 470953
#>      year
#> age   1988   1989   1990   1991   1992   1993   1994   1995   1996   1997  
#>   all 424696 448230 396458 356947 311431 279962 233481 222203 203391 204948
#>      year
#> age   1998   1999   2000   2001   2002   2003   2004   2005   2006   2007  
#>   all 237863 219149 230902 234211 221902 248312 233477 253737 284447 293330
#>      year
#> age   2008   2009   2010   2011   2012   2013   2014   2015   2016   2017  
#>   all 371837 453027 554244 575459 617538 709948 823276 774157 836453 913289
#>      year
#> age   2018  
#>   all 980986
#> 
#> units:  t 
# Compare with ssb()
ssb(ple4)[, ac(2014:2017)] / ssb_next(ple4)[, ac(2014:2017)]
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   2014 2015 2016 2017
#>   all 1    1    1    1   
#> 
#> units:   
```

</div>

</div>

</div>
