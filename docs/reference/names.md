<div id="main" class="col-md-9" role="main">

# Method names

<div class="ref-description section level2">

The `names` method returns the names of the dimnames of an object. For
some classes, the names attribute can be modified directly using
names\<-.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLArray'
names(x)

# S4 method for class 'FLPar'
names(x)

# S4 method for class 'FLPar,character'
names(x) <- value
```

</div>

</div>

<div class="section level2">

## Generic function

names(x) names\<-(x, value)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[names](https://rdrr.io/r/base/names.html)

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
# FLQuant
data(ple4)
names(catch.n(ple4))
#> [1] "age"    "year"   "unit"   "season" "area"   "iter"  

# Contrast this with
dimnames(catch.n(ple4))
#> $age
#>  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"
#> 
#> $year
#>  [1] "1957" "1958" "1959" "1960" "1961" "1962" "1963" "1964" "1965" "1966"
#> [11] "1967" "1968" "1969" "1970" "1971" "1972" "1973" "1974" "1975" "1976"
#> [21] "1977" "1978" "1979" "1980" "1981" "1982" "1983" "1984" "1985" "1986"
#> [31] "1987" "1988" "1989" "1990" "1991" "1992" "1993" "1994" "1995" "1996"
#> [41] "1997" "1998" "1999" "2000" "2001" "2002" "2003" "2004" "2005" "2006"
#> [51] "2007" "2008" "2009" "2010" "2011" "2012" "2013" "2014" "2015" "2016"
#> [61] "2017"
#> 
#> $unit
#> [1] "unique"
#> 
#> $season
#> [1] "all"
#> 
#> $area
#> [1] "unique"
#> 
#> $iter
#> [1] "1"
#> 
```

</div>

</div>

</div>
