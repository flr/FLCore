<div id="main" class="col-md-9" role="main">

# Extract and modify the recruitment time series

<div class="ref-description section level2">

Recruitment in number of fish is the first row of the 'stock.n' slot of
an age-structured 'FLStock'. These convenience functions allow a clearer
syntax when retrieving of altering the content of 'stock.nrec.age,',
where 'rec.age' is usually the first age in the object.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLStock'
rec(object, rec.age = as.character(object@range["min"]))
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    An object of class 'FLStock'

-   rec.age:

    What age to extract, defaults to first one. As 'character' to select
    by name or as 'numeric' by position.

</div>

<div class="section level2">

## Value

RETURN Lorem ipsum dolor sit amet

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLComp](FLComp.md)

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
data(ple4)
rec(ple4)
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
#> age 2011    2012    2013    2014    2015    2016    2017   
#>   1 1608190 1278010 1455050 1640700  895620 1211320 1823000
#> 
#> units:  1000 
# Multiple recruitment by a factor of 2
rec(ple4) <- rec(ple4) * 2
```

</div>

</div>

</div>
