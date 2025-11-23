<div id="main" class="col-md-9" role="main">

# Generate a random walk time series from a starting point

<div class="ref-description section level2">

The last year of an `FLQuant` object is used as atrating point to
generate a time series following a random walk with drift: $$z_t =
z\_{t-1} + \\epsilon_t + \\delta_t, t=1,2,...$$ where \\(\\epsilon\\) is
\\(\\mathcal{N}(0, \\sigma)\\)

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
rwalk(x0, end = 1, sd = 0.05, delta = 0)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x0:

    The initial state of the random walk, 'FLQuant'.

-   end:

    The number of years or the final year of the series. numeric.

-   sd:

    The standard deviation of the random walk, numeric.

-   delta:

    The drift of the random walk.

</div>

<div class="section level2">

## Value

An 'FLQuant' object.

</div>

<div class="section level2">

## Details

The length of the series is set by argument *end*. This is taken as a
number of years, if its value is smaller than the final 'year' of 'x0',
or as a final year if larger or of class 'character'.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLQuant](FLQuant.md) [rnorm](https://rdrr.io/r/stats/Normal.html)

</div>

</div>

<div class="section level2">

## Author

Iago Mosqueira, WMR (2023)

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
# Generate random walk recruitmrnt with positive drift
rwalk(rec(ple4), end=5, sd=0.08, delta=0.05)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   2018    2019    2020    2021    2022   
#>   all 1894396 1770741 1923579 1981299 2068556
#> 
#> units:  1000 
# Use append() to add the new values at the end
append(rec(ple4), rwalk(rec(ple4), end=10, sd=0.04, delta=0))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 1957    1958    1959    1960    1961    1962    1963    1964    1965   
#>   1  477074  710748  874712  797702  870799  615691  610017 2449900  664500
#>    year
#> age 1966    1967    1968    1969    1970    1971    1972    1973    1974   
#>   1  579075  428110  418228  666902  671454  433599  367450 1391430 1074920
#>    year
#> age 1975    1976    1977    1978    1979    1980    1981    1982    1983   
#>   1  787372  674010 1033740  879043  915553 1078660  999968 1935350 1375880
#>    year
#> age 1984    1985    1986    1987    1988    1989    1990    1991    1992   
#>   1 1302060 1792220 4303680 1910200 1774940 1250510 1083810  981356  854841
#>    year
#> age 1993    1994    1995    1996    1997    1998    1999    2000    2001   
#>   1  550376  566448  932162  893056 2431310  778427  683151  857525  634808
#>    year
#> age 2002    2003    2004    2005    2006    2007    2008    2009    2010   
#>   1 1792880  557844 1235790  863893  875191 1379750 1135050 1088820 1444570
#>    year
#> age 2011    2012    2013    2014    2015    2016    2017    2018    2019   
#>   1 1608190 1278010 1455050 1640700  895620 1211320 1823000 1930564 2031272
#>    year
#> age 2020    2021    2022    2023    2024    2025    2026    2027   
#>   1 2051192 2122013 2128065 2130869 2317102 2424302 2600481 2639857
#> 
#> units:  1000 
# Use end as number of years
rwalk(rec(ple4), end=5)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   2018    2019    2020    2021    2022   
#>   all 1972106 1863191 1818043 1778570 1692195
#> 
#> units:  1000 
# or as final year
rwalk(rec(ple4), end=2020)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   2018    2019    2020   
#>   all 1818237 1842790 1767146
#> 
#> units:  1000 
```

</div>

</div>

</div>
