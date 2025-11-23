<div id="main" class="col-md-9" role="main">

# Calculate the mean age in the stock and catch

<div class="ref-description section level2">

Average age in the stock numbers or catch-at-age.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
meanage(object)

meanageCatch(object)
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
meanage(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970
#>   all 3.37 3.09 2.85 2.84 2.83 3.05 3.17 2.20 2.63 3.05 3.44 3.67 3.43 3.23
#>      year
#> age   1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984
#>   all 3.38 3.51 2.43 2.35 2.52 2.70 2.49 2.52 2.47 2.31 2.29 1.94 2.11 2.26
#>      year
#> age   1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998
#>   all 2.16 1.71 2.09 2.31 2.50 2.59 2.61 2.61 2.75 2.71 2.29 2.18 1.70 2.14
#>      year
#> age   1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012
#>   all 2.45 2.43 2.53 1.97 2.37 2.24 2.40 2.55 2.40 2.56 2.76 2.78 2.83 3.07
#>      year
#> age   2013 2014 2015 2016 2017
#>   all 3.20 3.26 3.71 3.88 3.67
#> 
#> units:   
meanageCatch(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970
#>   all 3.80 3.59 3.27 3.14 3.13 3.33 3.58 3.20 3.00 3.35 3.73 3.92 3.74 3.49
#>      year
#> age   1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984
#>   all 3.60 3.77 2.93 2.62 2.67 2.77 2.55 2.59 2.63 2.60 2.60 2.28 2.31 2.44
#>      year
#> age   1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998
#>   all 2.38 1.96 2.31 2.62 2.85 3.00 3.04 3.04 3.17 3.24 2.99 2.79 2.41 2.46
#>      year
#> age   1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012
#>   all 2.85 2.99 3.02 2.55 2.55 2.51 2.48 2.60 2.49 2.48 2.55 2.58 2.76 3.00
#>      year
#> age   2013 2014 2015 2016 2017
#>   all 3.03 2.88 3.09 3.29 3.35
#> 
#> units:   
```

</div>

</div>

</div>
