<div id="main" class="col-md-9" role="main">

# Returns the computed yearly production

<div class="ref-description section level2">

Returns the computed yearly production

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
production(object, ...)

# S4 method for class 'FLStock'
production(object, what = "ssb", ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    An object with biomass and catch data.

-   what:

    One of the production options: "ssb", "biomass", or "exploitation".

</div>

<div class="section level2">

## Value

The production by year, of class FLQuant.

</div>

<div class="section level2">

## Details

Production can be calculated for an FLStock based on the spawning stock
biomass ("ssb"), total biomass ("biomass"), or exploitation
("exploitation").

</div>

<div class="section level2">

## Author

Laurie Kell (Sea++), Iago Mosqueira (WMR)

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
# For SSB
production(ple4, "ssb")
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all  91512  95530 123119 129308 210400  84703 130632 100648 172340 231689
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 139513  93708 113785 127757 145745  85625 147244 161657 192360 176272
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 164270 152893 189471 156381 177925 247466 240644 255680 260378 342209
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 262223 338779 240262 211093 172667 161222 133093 139970 113817 133277
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 185109 152527 182415 149306 115799 170217 139195 160315 145261 120747
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 183276 194586 216921 140039 161798 224281 254384  90631 199634 208053
#>      year
#> age   2017  
#>   all     NA
#> 
#> units:  t 
# For total biomass
production(ple4, "biomass")
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all  97058 124336 134194 131624 195831  84314 156283 106621 184772 168397
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 135624 103681 116318 135302 137681  98870 177708 199230 182734 171714
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 160913 160718 182691 144394 236823 256091 250806 257466 367166 307643
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 241039 255693 216386 200280 151335 142126 122293 150908 118295 198090
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 158178 143119 145741 159858 160431 150073 156416 123679 164461 123874
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 198507 214558 221034 134975 172571 232425 263438   3241 198078 219445
#>      year
#> age   2017  
#>   all     NA
#> 
#> units:  t 
```

</div>

</div>

</div>
