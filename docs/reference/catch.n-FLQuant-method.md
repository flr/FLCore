<div id="main" class="col-md-9" role="main">

# catch.n calculation method

<div class="ref-description section level2">

Calculate catch.n (catch-at-age/length) from abundances, F and M using
the catch equation

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLQuant'
catch.n(object, harvest, m)
```

</div>

</div>

<div class="section level2">

## Details

The catch-at-age/length, commonly found in the catch.n slot of an
`FLStock` object, can be simply calculated from
abundances-at-age/length, and natural and fishing
mortalities-at-age/length by applying the catch equation $$C = N \\cdot
F \\frac{F}{M+F} \\cdot (1 - {\\rm e}^(-M-F))$$

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLStock](FLStock.md)

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
res <- catch.n(stock.n(ple4), harvest(ple4), m(ple4))
catch.n(ple4) / res
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971
#>   1  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   2  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   3  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   4  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   5  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   6  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   7  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   8  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   9  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   10 1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>     year
#> age  1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986
#>   1  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   2  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   3  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   4  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   5  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   6  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   7  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   8  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   9  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   10 1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>     year
#> age  1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001
#>   1  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   2  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   3  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   4  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   5  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   6  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   7  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   8  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   9  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   10 1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>     year
#> age  2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016
#>   1  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   2  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   3  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   4  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   5  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   6  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   7  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   8  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   9  1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>   10 1    1    1    1    1    1    1    1    1    1    1    1    1    1    1   
#>     year
#> age  2017
#>   1  1   
#>   2  1   
#>   3  1   
#>   4  1   
#>   5  1   
#>   6  1   
#>   7  1   
#>   8  1   
#>   9  1   
#>   10 1   
#> 
#> units:  NA 
```

</div>

</div>

</div>
