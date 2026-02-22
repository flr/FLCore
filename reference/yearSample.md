# Samples along the year dimension

A resample from an FLQuant object along the 'year' dimension is
returned. The 'year' dimnames of the output object can be specified,
although that is not needed if the resample is to be assigned in a slot.

## Usage

``` r
yearSample(x, size = length(years), years, replace = TRUE, prob = NULL)
```

## Arguments

- x:

  An FLQuant object.

- size:

  Number of samples (years), non-negative integer.

- years:

  Optional vector to set as 'year' dimnames in output.

- replace:

  should sampling be with replacement? Defaults to TRUE.

- prob:

  a vector of probability weights.

## Value

RETURN Description, class

## See also

[FLQuant](FLQuant.md) [`sample()`](https://rdrr.io/r/base/sample.html)

## Author

Iago Mosqueira (WMR)

## Examples

``` r
data(ple4)
# Take 20 samples of recent recruitment 
yearSample(rec(ple4)[, ac(2013:2017)], 20)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2013    2017    2015    2015    2016    2014    2013    2017    2013   
#>   1 1455050 1823000  895620  895620 1211320 1640700 1455050 1823000 1455050
#>    year
#> age 2014    2013    2013    2013    2014    2016    2013    2015    2017   
#>   1 1640700 1455050 1455050 1455050 1640700 1211320 1455050  895620 1823000
#>    year
#> age 2015    2015   
#>   1  895620  895620
#> 
#> units:  1000 
# Providing 'years' sets the output object dimnames
yearSample(rec(ple4)[, ac(2013:2017)], 20, year=2000:2019)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2000    2001    2002    2003    2004    2005    2006    2007    2008   
#>   1  895620 1455050 1455050  895620 1211320  895620 1640700  895620 1211320
#>    year
#> age 2009    2010    2011    2012    2013    2014    2015    2016    2017   
#>   1  895620  895620  895620 1823000 1211320 1823000  895620  895620 1455050
#>    year
#> age 2018    2019   
#>   1 1455050 1211320
#> 
#> units:  1000 
```
