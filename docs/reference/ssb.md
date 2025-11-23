<div id="main" class="col-md-9" role="main">

# Calculate or return the Spawning Stock Biomass

<div class="ref-description section level2">

The calculated Spawning Stock Biomass (SSB) of a fish population is
returned by this method. SSB is the combined weight of all individuals
in a fish stock that are capable of reproducing. In some classes this is
calculated from information stored in different slots, while in others
`ssb()` is simply an slot accessor. When the later is the case, the
corresponding replacement method also exists.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
ssb(object, ...)

# S4 method for class 'FLBiol'
ssb(object, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    Object on which `ssb` is calculated or extracted.

</div>

<div class="section level2">

## Value

An object, generally of class `FLQuant`.

</div>

<div class="section level2">

## Details

Objects of the *FLBiol* class do not contain any information on catch or
fishing mortality, so a call to `ssb()` will only correct abundances for
natural mortality to the moment of spawning. The method can also take
information on catches or fishing mortality and use them when
calculating abundances at spawning time. An *FLQuant* named either
'catch.n', 'f', 'hr' or 'harvest' can be used. The first three are
self-explanatory, while for the last units must be either 'f' or 'hr'.
The quantities should refer to total yearly values, as the value in the
'spwn' slot will be used to calculate what fraction of fishing mortality
to apply.

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
biol <- as(ple4, "FLBiol")
# SSB from FLBiol, abundances corrected only for M
ssb(biol)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 342223 355375 362119 380052 391386 482245 440658 430475 383583 404516
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 473938 458977 402865 370472 361610 366129 302365 298094 301757 328726
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 329116 327544 302271 319090 290780 284211 339238 367251 394666 407972
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 470952 424696 448230 396458 356947 311431 279962 233481 222203 203391
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 204948 237863 219149 230902 234211 221902 248312 233478 253737 284447
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 293330 371837 453026 554245 575459 617539 709948 823276 774157 836453
#>      year
#> age   2017  
#>   all 913290
#> 
#> units:  t 
# Provide catch-at-age, F or HR to correct N
ssb(biol, catch.n=catch.n(ple4))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 342223 355375 362119 380052 391386 482245 440658 430475 383583 404516
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 473938 458977 402865 370472 361610 366129 302365 298094 301757 328726
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 329116 327544 302271 319090 290780 284211 339238 367251 394666 407972
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 470952 424696 448230 396458 356947 311431 279962 233481 222203 203391
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 204948 237863 219149 230902 234211 221902 248312 233478 253737 284447
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 293330 371837 453026 554245 575459 617539 709948 823276 774157 836453
#>      year
#> age   2017  
#>   all 913290
#> 
#> units:  t 
ssb(biol, f=harvest(ple4))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 342223 355375 362119 380052 391386 482245 440658 430475 383583 404516
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 473938 458977 402865 370472 361610 366129 302365 298094 301757 328726
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 329116 327544 302271 319090 290780 284211 339238 367251 394666 407972
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 470952 424696 448230 396458 356947 311431 279962 233481 222203 203391
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 204948 237863 219149 230902 234211 221902 248312 233478 253737 284447
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 293330 371837 453026 554245 575459 617539 709948 823276 774157 836453
#>      year
#> age   2017  
#>   all 913290
#> 
#> units:  t 
ssb(biol, harvest=harvest(ple4))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 342223 355375 362119 380052 391386 482245 440658 430475 383583 404516
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 473938 458977 402865 370472 361610 366129 302365 298094 301757 328726
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 329116 327544 302271 319090 290780 284211 339238 367251 394666 407972
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 470952 424696 448230 396458 356947 311431 279962 233481 222203 203391
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 204948 237863 219149 230902 234211 221902 248312 233478 253737 284447
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 293330 371837 453026 554245 575459 617539 709948 823276 774157 836453
#>      year
#> age   2017  
#>   all 913290
#> 
#> units:  t 
ssb(biol, hr=catch.n(ple4) / stock.n(ple4))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 342223 355375 362119 380052 391386 482245 440658 430475 383583 404516
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 473938 458977 402865 370472 361610 366129 302365 298094 301757 328726
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 329116 327544 302271 319090 290780 284211 339238 367251 394666 407972
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 470952 424696 448230 396458 356947 311431 279962 233481 222203 203391
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 204948 237863 219149 230902 234211 221902 248312 233478 253737 284447
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 293330 371837 453026 554245 575459 617539 709948 823276 774157 836453
#>      year
#> age   2017  
#>   all 913290
#> 
#> units:  t 
```

</div>

</div>

</div>
