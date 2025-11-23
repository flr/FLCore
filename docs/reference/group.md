<div id="main" class="col-md-9" role="main">

# Group objects over some index by applying a function over a single dimension

<div class="ref-description section level2">

Array objects (e.g. FLQuant or FLQuants) are divided along a single
dimnension following a given index or expression, an aggregating
function is applied to each subset, and the results are joined again.
Data can be added, for example, by decade or for two age groups.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
group(x, FUN, ...)

# S4 method for class 'FLQuant,function'
group(x, FUN = sum, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    An object to group.

-   FUN:

    A function to apply along the chosen dimension, defaults to 'sum'.

-   ...:

    An expression or indexing vector, named as the chosen dimension.
    Extra arguments to FUN can also be provided, but cannmot match names
    in x.

</div>

<div class="section level2">

## Value

A single object with reduced dimensionality.

</div>

<div class="section level2">

## Author

Iago Mosqueira (WMR)

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
# Add catch-at-age along two age groups, 'juv'eniles and 'adu'lts
group(catch.n(ple4), sum, age=c('juv', 'juv', rep('adu', 8)))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957    1958    1959    1960    1961    1962    1963    1964    1965   
#>   juv   82844  144028  220590  250246  240235  212994  163538  284592  488725
#>   adu  217709  216081  229525  263171  300448  323824  375336  356155  320567
#>      year
#> age   1966    1967    1968    1969    1970    1971    1972    1973    1974   
#>   juv  150830  136276  135024  197136  251958  200146  144800  322925  524851
#>   adu  634678  479953  396629  324414  263583  266412  285149  264431  221214
#>      year
#> age   1975    1976    1977    1978    1979    1980    1981    1982    1983   
#>   juv  424841  354998  453126  483173  435736  446896  455098  628425  775178
#>   adu  349358  352906  316769  286774  315207  303644  303591  333781  335564
#>      year
#> age   1984    1985    1986    1987    1988    1989    1990    1991    1992   
#>   juv  645330  763038 1500360 1489939  853142  683898  503947  446701  410487
#>   adu  501451  501309  484611  554359 1016274  795393  685345  560852  485903
#>      year
#> age   1993    1994    1995    1996    1997    1998    1999    2000    2001   
#>   juv  316124  221653  255472  337605  528114  793612  278299  267005  285233
#>   adu  426742  361297  282531  268988  366551  360498  637135  443016  365948
#>      year
#> age   2002    2003    2004    2005    2006    2007    2008    2009    2010   
#>   juv  445588  623612  382435  443476  323632  368491  415136  344764  349579
#>   adu  375610  288089  418011  247371  293727  259470  236503  280668  288770
#>      year
#> age   2011    2012    2013    2014    2015    2016    2017   
#>   juv  339133  303843  306544  418660  354316  242102  258717
#>   adu  310395  382591  449713  437356  442764  469107  397382
#> 
#> units:  1000 
# An expression can use based on dimnames
group(catch.n(ple4), sum, age=age < 3)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>        year
#> age     1957    1958    1959    1960    1961    1962    1963    1964    1965   
#>   TRUE    82844  144028  220590  250246  240235  212994  163538  284592  488725
#>   FALSE  217709  216081  229525  263171  300448  323824  375336  356155  320567
#>        year
#> age     1966    1967    1968    1969    1970    1971    1972    1973    1974   
#>   TRUE   150830  136276  135024  197136  251958  200146  144800  322925  524851
#>   FALSE  634678  479953  396629  324414  263583  266412  285149  264431  221214
#>        year
#> age     1975    1976    1977    1978    1979    1980    1981    1982    1983   
#>   TRUE   424841  354998  453126  483173  435736  446896  455098  628425  775178
#>   FALSE  349358  352906  316769  286774  315207  303644  303591  333781  335564
#>        year
#> age     1984    1985    1986    1987    1988    1989    1990    1991    1992   
#>   TRUE   645330  763038 1500360 1489939  853142  683898  503947  446701  410487
#>   FALSE  501451  501309  484611  554359 1016274  795393  685345  560852  485903
#>        year
#> age     1993    1994    1995    1996    1997    1998    1999    2000    2001   
#>   TRUE   316124  221653  255472  337605  528114  793612  278299  267005  285233
#>   FALSE  426742  361297  282531  268988  366551  360498  637135  443016  365948
#>        year
#> age     2002    2003    2004    2005    2006    2007    2008    2009    2010   
#>   TRUE   445588  623612  382435  443476  323632  368491  415136  344764  349579
#>   FALSE  375610  288089  418011  247371  293727  259470  236503  280668  288770
#>        year
#> age     2011    2012    2013    2014    2015    2016    2017   
#>   TRUE   339133  303843  306544  418660  354316  242102  258717
#>   FALSE  310395  382591  449713  437356  442764  469107  397382
#> 
#> units:  1000 
# Mean by lustrum, by using 'year - year %% 5'
group(catch.n(ple4), mean, year = year - year %% 5)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  1955   1960   1965   1970   1975   1980   1985   1990   1995   2000  
#>   1   71809  97611  56285 142897 236660 287022 522676 151862 137448 165349
#>   2   77345 132710 165314 146039 193714 303164 535399 227921 301173 235426
#>   3   70668 134318 177300  83951 151557 200469 364165 214340 240943 187282
#>   4   53897  77485 102812  54228  77645  79913 183142 122345  71373 106119
#>   5   37029  42709  65547  35383  42043  32622  67189  88977  33588  50225
#>   6   18876  26412  41723  22400  22713  15435  29589  44095  17259  21676
#>   7   11647  13543  14706  21814  10821   9641  12152  17572   8772   7918
#>   8    8069   8236   8886  13606   4790   6739   5394   7693   4638   2478
#>   9    6493   5486   5791   9201   3280   3589   2705   4034   2400    747
#>   10  14426  15599  14483  19574  11352   7199   6053   4972   4168   1691
#>     year
#> age  2005   2010   2015  
#>   1  166916 161602 123926
#>   2  212184 181950 161119
#>   3  136160 163860 161994
#>   4   68691 108734 123839
#>   5   30263  53586  73302
#>   6   15753  24526  39483
#>   7    7192  11669  18796
#>   8    2995   4802   7518
#>   9     962   2073   3159
#>   10   1532   4516   8326
#> 
#> units:  1000 
```

</div>

</div>

</div>
