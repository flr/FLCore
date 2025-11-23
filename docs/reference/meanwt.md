<div id="main" class="col-md-9" role="main">

# Calculate the mean weight in stock and catch

<div class="ref-description section level2">

Average weight in the stock numbers or catch-at-age.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
meanwt(object)

meanwtCatch(object)
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
meanwt(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 0.2298 0.2095 0.1942 0.1944 0.1888 0.2306 0.2262 0.1348 0.1358 0.1693
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 0.2125 0.2442 0.2274 0.2159 0.2451 0.2814 0.1639 0.1562 0.1835 0.2159
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 0.2006 0.2052 0.2005 0.1921 0.1714 0.1380 0.1505 0.1654 0.1570 0.1102
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 0.1259 0.1306 0.1480 0.1568 0.1651 0.1641 0.1831 0.1857 0.1658 0.1444
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 0.0996 0.1140 0.1348 0.1391 0.1630 0.1224 0.1491 0.1431 0.1453 0.1663
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 0.1405 0.1582 0.1803 0.1832 0.1622 0.1657 0.1741 0.1826 0.1722 0.1853
#>      year
#> age   2017  
#>   all 0.1794
#> 
#> units:  kg 
meanwtCatch(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957  1958  1959  1960  1961  1962  1963  1964  1965  1966  1967  1968 
#>   all 0.260 0.247 0.225 0.217 0.212 0.250 0.256 0.224 0.166 0.186 0.231 0.263
#>      year
#> age   1969  1970  1971  1972  1973  1974  1975  1976  1977  1978  1979  1980 
#>   all 0.250 0.236 0.266 0.308 0.210 0.181 0.195 0.222 0.206 0.214 0.217 0.225
#>      year
#> age   1981  1982  1983  1984  1985  1986  1987  1988  1989  1990  1991  1992 
#>   all 0.204 0.171 0.171 0.183 0.178 0.133 0.145 0.154 0.174 0.186 0.196 0.196
#>      year
#> age   1993  1994  1995  1996  1997  1998  1999  2000  2001  2002  2003  2004 
#>   all 0.215 0.226 0.229 0.199 0.167 0.142 0.162 0.174 0.195 0.167 0.161 0.164
#>      year
#> age   2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016 
#>   all 0.150 0.170 0.151 0.155 0.168 0.172 0.160 0.158 0.164 0.158 0.139 0.150
#>      year
#> age   2017 
#>   all 0.157
#> 
#> units:  kg 
```

</div>

</div>

</div>
