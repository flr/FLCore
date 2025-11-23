<div id="main" class="col-md-9" role="main">

# Proportion of mature and inmature fish in the catch

<div class="ref-description section level2">

The proportion in weight of mature and inmature fish in the catch can be
computed using catchMature and catchInmature.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
catchInmature(object)

catchMature(object)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    An age-structured FLStock object

</div>

<div class="section level2">

## Value

An FLQuant object

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
catchInmature(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all  11792  12110  20054  25054  23842  21857  22721  25601  31779  41565
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all  18240  19683  23468  23484  28266  28470  31956  42319  54747  52910
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all  51540  54576  57286  58245  59681  70196  75922  85011  80664 112244
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 111446 114200  72999  62712  53467  49598  46376  37444  35802  39250
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all  55499  60331  64730  32075  35524  48878  54410  56284  36520  40576
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all  35517  39694  40525  36357  31164  33045  34036  29370  26922  26347
#>      year
#> age   2017  
#>   all  20826
#> 
#> units:  t 
catchMature(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all  66569  76676  85133  92921  95698 104434 118094 121939 119629 120701
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 136234 130137 122709 113135 112960 120920 119559 115675 110645 122972
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 114303 123590 115366 126446 124812 122243 136710 143254 166407 166985
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 197033 201045 219036 187892 164717 143093 133197 113805  96827  92470
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all  96696 110909 105932 113923  92582  94929  99619  83772  78031  71288
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all  69253  73702  75177  82467  88555  98827 107019 110380 110417 104870
#>      year
#> age   2017  
#>   all 104095
#> 
#> units:  t 
```

</div>

</div>

</div>
