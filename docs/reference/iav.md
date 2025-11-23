<div id="main" class="col-md-9" role="main">

# Compute the inter-annual variability of a time series

<div class="ref-description section level2">

The inter-annual variability of a time series stored in an FLQuant
object, is computed as \\(\|x_y - x\_{y-1}) / x\_{y-1}\|\\). The
resulting object will be one year shorter than the input. The first year
will be missing as values are assigned to the final year of each pair.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
iav(object)
```

</div>

</div>

<div class="section level2">

## Value

An object of the same class as object.

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
# Compute inter-annual variability in catch
iav(catch(ple4))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1958    1959    1960    1961    1962    1963    1964    1965    1966   
#>   all 0.13304 0.18473 0.12158 0.01328 0.05646 0.11501 0.04776 0.02621 0.07172
#>      year
#> age   1967    1968    1969    1970    1971    1972    1973    1974    1975   
#>   all 0.04802 0.03013 0.02431 0.06539 0.03372 0.05781 0.01422 0.04276 0.04682
#>      year
#> age   1976    1977    1978    1979    1980    1981    1982    1983    1984   
#>   all 0.06342 0.05707 0.07430 0.03095 0.06973 0.00106 0.04306 0.10493 0.07352
#>      year
#> age   1985    1986    1987    1988    1989    1990    1991    1992    1993   
#>   all 0.08239 0.13015 0.10476 0.02193 0.07362 0.14187 0.12937 0.11684 0.06808
#>      year
#> age   1994    1995    1996    1997    1998    1999    2000    2001    2002   
#>   all 0.15773 0.12311 0.00686 0.15545 0.12514 0.00338 0.14452 0.12254 0.12256
#>      year
#> age   2003    2004    2005    2006    2007    2008    2009    2010    2011   
#>   all 0.07108 0.09072 0.18211 0.02345 0.06342 0.08234 0.02034 0.02698 0.00752
#>      year
#> age   2012    2013    2014    2015    2016    2017   
#>   all 0.10152 0.06964 0.00925 0.01726 0.04458 0.04797
#> 
#> units:   
```

</div>

</div>

</div>
