<div id="main" class="col-md-9" role="main">

# Computes the mean natural mortality acros the fully selected ages

<div class="ref-description section level2">

Equivalent to the mean fishing mortality metric returned by 'fbar',
'mbar' calculates the mean natural mortality across the ages inside the
range defined by 'minfbar' and 'maxfbar'.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
mbar(object, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    An object of class 'FLStock'.

</div>

<div class="section level2">

## Value

An object of class 'FLQuant'.

</div>

<div class="section level2">

## See also

<div class="dont-index">

fbar

</div>

</div>

<div class="section level2">

## Author

The FLR Team, proposal by H. Winker.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
mbar(ple4)
#> An x of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970
#>   all 0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1 
#>      year
#> age   1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984
#>   all 0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1 
#>      year
#> age   1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998
#>   all 0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1 
#>      year
#> age   1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012
#>   all 0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1  0.1 
#>      year
#> age   2013 2014 2015 2016 2017
#>   all 0.1  0.1  0.1  0.1  0.1 
#> 
#> units:  m 
```

</div>

</div>

</div>
