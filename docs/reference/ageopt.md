<div id="main" class="col-md-9" role="main">

# Age at which a cohort reaches its maximum biomass, calculated by year

<div class="ref-description section level2">

The optimal (or critical) age is the transition point when a cohort
achieves its maximum biomass in the absemce of fishing, i.e. losses due
to natural mortality are now greater than gains due to increase in
individual biomass.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLStock'
ageopt(object)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    An object of class 'FLStock'

</div>

<div class="section level2">

## Value

The age at which maximum biomass is reached, an 'FLQuant'.

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
ageopt(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971
#>   all 4    5    6    7    8    9    9    8    8    8    7    7    9    9   
#>      year
#> age   1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985
#>   all 6    6    6    7    6    7    9    9    9    6    6    8    7    7   
#>      year
#> age   1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999
#>   all 9    8    8    9    8    8    9    8    9    9    9    6    6    7   
#>      year
#> age   2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013
#>   all 7    9    9    9    9    9    9    7    8    8    8    6    7    9   
#>      year
#> age   2014 2015 2016 2017
#>   all 7    8    8    8   
#> 
#> units:   
```

</div>

</div>

</div>
