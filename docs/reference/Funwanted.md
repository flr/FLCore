<div id="main" class="col-md-9" role="main">

# Calculate the discards and landings-associated fishing mortalities

<div class="ref-description section level2">

Computes the fishing mortality at age (harvest) associated with either
landings (*Fwanted*) or discards (*Funwanted*) through the respective
proportions at age. The function names reflect the convention used in
ICES.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
Funwanted(x, ages = dimnames(x)$age)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    An FLStock object, with harvest

-   ages:

    Ages over which the respective Fbar calculation applies

</div>

<div class="section level2">

## Value

An FLQuant

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
Fwanted(ple4, ages=2:6)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 0.2018 0.2033 0.1972 0.2381 0.2197 0.2126 0.2287 0.2510 0.2750 0.2271
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 0.2016 0.2241 0.2646 0.2656 0.2764 0.3212 0.4001 0.4014 0.3039 0.3141
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 0.2932 0.3705 0.3821 0.4768 0.4758 0.4487 0.4176 0.3959 0.4595 0.4512
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 0.4439 0.3920 0.4061 0.4275 0.4135 0.4256 0.4834 0.5065 0.5213 0.5158
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 0.5010 0.4329 0.4158 0.4196 0.3011 0.3957 0.3536 0.2996 0.2056 0.1980
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 0.1573 0.1599 0.1286 0.1164 0.1064 0.1113 0.1217 0.0953 0.0984 0.0932
#>      year
#> age   2017  
#>   all 0.0928
#> 
#> units:  f 
Funwanted(ple4, ages=1:3)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 0.0951 0.1537 0.1857 0.1801 0.2137 0.2115 0.2413 0.2094 0.1985 0.2157
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 0.2014 0.1969 0.1856 0.2137 0.2044 0.1739 0.1468 0.2054 0.3331 0.2905
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 0.3134 0.2845 0.2919 0.1880 0.1761 0.2002 0.2428 0.2503 0.2542 0.2820
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 0.3723 0.3837 0.3528 0.3228 0.3171 0.3032 0.2499 0.2071 0.1698 0.2341
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 0.3873 0.4038 0.3033 0.2489 0.2998 0.3333 0.3532 0.3198 0.2902 0.2577
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 0.2410 0.1947 0.1810 0.1565 0.1400 0.1461 0.1511 0.1755 0.1816 0.1636
#>      year
#> age   2017  
#>   all 0.1414
#> 
#> units:  f 
```

</div>

</div>

</div>
