# Biomass metrics for FLStock

A family of functions and methods to compute different biomass
quantities from an [FLStock](FLStock.md) object, varying in which
component of the stock is selected and at what point in the time step
mortality is evaluated.

## Usage

``` r
# S4 method for class 'FLStock'
ssb(object, byage = FALSE, byunit = FALSE, ...)

ssb_end(object, byage = FALSE, byunit = FALSE, ...)

ssb_start(object, byage = FALSE, byunit = FALSE, ...)

# S4 method for class 'FLStock,ANY'
vb(x, sel, byage = FALSE, ...)

# S4 method for class 'FLStock,missing'
vb(x, byage = FALSE, ...)

# S4 method for class 'FLStock'
exb(x, sel = catch.sel(x), wt = catch.wt(x), byage = FALSE, ...)

biomass_end(x)

# S4 method for class 'FLStock'
tsb(object, time = m.spwn(object), byage = FALSE, byunit = FALSE, ...)

ssb_next(x, fbar = 0, wts.nyears = 3, fbar.nyears = 3)

biomass_end(x)

biomass_spawn(x)
```

## Arguments

- object, x:

  An object of class [FLStock](FLStock.md).

- byage:

  Logical; if `TRUE` return values by age rather than summing over ages.
  Default `FALSE` (except `biomass_end` where it is `TRUE`).

- byunit:

  Logical; if `TRUE` return values by unit rather than summing over
  units. Default `FALSE`.

- ...:

  Named slot–value pairs to temporarily override slots of `object`
  before calculation (e.g. `m = 0.2`).

- sel:

  Selectivity-at-age as an [FLQuant](FLQuant.md). Defaults to
  `catch.sel(x)` in `vb` and `exb`.

- wt:

  Weight-at-age as an [FLQuant](FLQuant.md). Defaults to `catch.wt(x)`
  in `exb`.

- time:

  Fraction of year at which biomass is evaluated in `tsb`; defaults to
  `m.spwn(object)`.

- fbar:

  Assumed mean fishing mortality for the projected extra year in
  `ssb_next`. Default `0`.

- wts.nyears:

  Number of years over which to average weights-at-age and maturity for
  the projected year in `ssb_next`. Default `3`.

- fbar.nyears:

  Number of years over which to average selectivity, M and spawning
  fractions for the projected year in `ssb_next`. Default `3`.

## Value

An [FLQuant](FLQuant.md) with the requested biomass quantity. Dimensions
depend on `byage` and `byunit`.

## Details

All calculations use the internal `.biomass` workhorse, which applies
fishing mortality (F or harvest rate) and natural mortality
proportionally according to the `harvest.spwn` and `m.spwn` timing
fractions.

- `ssb`:

  Spawning stock biomass at the spawning fraction of the time step,
  weighted by maturity-at-age. Mortality applied up to `harvest.spwn`
  and `m.spwn`.

- `ssb_end`:

  Spawning stock biomass at the *end* of the time step (`ph = pm = 1`):
  full annual F and M applied before weighting by maturity.

- `ssb_start`:

  Spawning stock biomass at the *start* of the time step
  (`ph = pm = 0`): no mortality applied, numbers weighted by maturity
  and stock weight only.

- `ssb_next`:

  Projected SSB for the year immediately following the last year in the
  object, calculated from survivors and an assumed `fbar`.
  Weights-at-age and maturity are averaged over the last `wts.nyears`
  years; selectivity, M and spawning fractions over the last
  `fbar.nyears` years.

- `tsb`:

  Total stock biomass (all ages, `sel = 1`) at the spawning fraction of
  the time step.

- `biomass_end`:

  Total stock biomass at the end of the time step (`ph = pm = 1`,
  `sel = 1`).

- `biomass_spawn`:

  Total stock biomass surviving to the spawning fraction, using the
  stored `harvest.spwn` and `m.spwn` slots directly.

- `vb`:

  Vulnerable biomass at the *start* of the time step (`ph = pm = 0`),
  using a user-supplied or default (`catch.sel`) selectivity.

- `exb`:

  Exploitable biomass at the start of the time step, using `catch.sel`
  and `catch.wt` by default, or user-supplied `sel` and `wt`.

## See also

[`fbar`](fbar.md), [`catch`](accessors.md), [FLStock](FLStock.md)

## Author

FLR Team

## Examples

``` r
data(ple4)

# SSB at spawning fraction of the year
ssb(ple4)
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

# SSB at start and end of the time step
ssb_start(ple4)
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
ssb_end(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 280554 291336 296865 311566 320857 395344 361251 352903 314461 331621
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 388534 376269 330268 303713 296448 300152 247878 244377 247380 269489
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 269809 268520 247801 261589 238381 232996 278107 301072 323546 334455
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 386086 348165 367459 325016 292625 255311 229512 191408 182162 166740
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 168016 194999 179658 189293 192006 181915 203566 191405 208013 233189
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 240472 304831 371390 454369 471760 506257 582014 674921 634653 685723
#>      year
#> age   2017  
#>   all 748714
#> 
#> units:  t 

# Projected SSB for year after last, assuming fbar = 0.3
ssb_next(ple4, fbar = 0.3)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1958   1959   1960   1961   1962   1963   1964   1965   1966   1967  
#>   all 355374 362120 380052 391385 482245 440658 430475 383583 404516 473937
#>      year
#> age   1968   1969   1970   1971   1972   1973   1974   1975   1976   1977  
#>   all 458977 402865 370473 361610 366129 302365 298094 301757 328726 329116
#>      year
#> age   1978   1979   1980   1981   1982   1983   1984   1985   1986   1987  
#>   all 327543 302271 319089 290780 284211 339238 367251 394666 407972 470953
#>      year
#> age   1988   1989   1990   1991   1992   1993   1994   1995   1996   1997  
#>   all 424696 448230 396458 356947 311431 279962 233481 222203 203391 204948
#>      year
#> age   1998   1999   2000   2001   2002   2003   2004   2005   2006   2007  
#>   all 237863 219149 230902 234211 221902 248312 233477 253737 284447 293330
#>      year
#> age   2008   2009   2010   2011   2012   2013   2014   2015   2016   2017  
#>   all 371837 453027 554244 575459 617538 709948 823276 774157 836453 913289
#>      year
#> age   2018  
#>   all 980986
#> 
#> units:  t 

# Total stock biomass
tsb(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957    1958    1959    1960    1961    1962    1963    1964    1965   
#>   all  402414  421111  456662  485670  499319  575609  533633  549102  508183
#>      year
#> age   1966    1967    1968    1969    1970    1971    1972    1973    1974   
#>   all  541547  547678  528828  482689  452829  451512  447967  397447  423640
#>      year
#> age   1975    1976    1977    1978    1979    1980    1981    1982    1983   
#>   all  464876  482218  478051  473121  455674  465713  425416  477746  541397
#>      year
#> age   1984    1985    1986    1987    1988    1989    1990    1991    1992   
#>   all  579572  608773  728868  757282  689841  630289  554640  504316  437467
#>      year
#> age   1993    1994    1995    1996    1997    1998    1999    2000    2001   
#>   all  386902  329622  329282  314948  381319  387303  359182  334261  348122
#>      year
#> age   2002    2003    2004    2005    2006    2007    2008    2009    2010   
#>   all  380446  386712  389098  372721  422631  434641  528377  629539  734871
#>      year
#> age   2011    2012    2013    2014    2015    2016    2017   
#>   all  751021  803874  904428 1026811  890302  951042 1039271
#> 
#> units:  t 
biomass_end(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 289897 296590 317036 333857 343225 393476 352113 360623 332412 351525
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 360379 345876 313248 294442 290848 280019 242699 255167 277548 287591
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 282530 272245 258046 261209 238149 276371 309904 325312 337817 408787
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 403915 351947 326461 292081 269549 229503 198954 173663 181174 170792
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 203877 195719 184708 185640 194967 214682 211128 227678 239254 282638
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 303470 382205 469881 560723 581047 624532 700730 801104 700240 758976
#>      year
#> age   2017  
#>   all 842808
#> 
#> units:  t 
biomass_spawn(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957    1958    1959    1960    1961    1962    1963    1964    1965   
#>   all  402414  421111  456662  485670  499319  575609  533633  549102  508183
#>      year
#> age   1966    1967    1968    1969    1970    1971    1972    1973    1974   
#>   all  541547  547678  528828  482689  452829  451512  447967  397447  423640
#>      year
#> age   1975    1976    1977    1978    1979    1980    1981    1982    1983   
#>   all  464876  482218  478051  473121  455674  465713  425416  477746  541397
#>      year
#> age   1984    1985    1986    1987    1988    1989    1990    1991    1992   
#>   all  579572  608773  728868  757282  689841  630289  554640  504316  437467
#>      year
#> age   1993    1994    1995    1996    1997    1998    1999    2000    2001   
#>   all  386902  329622  329282  314948  381319  387303  359182  334261  348122
#>      year
#> age   2002    2003    2004    2005    2006    2007    2008    2009    2010   
#>   all  380446  386712  389098  372721  422631  434641  528377  629539  734871
#>      year
#> age   2011    2012    2013    2014    2015    2016    2017   
#>   all  751021  803874  904428 1026811  890302  951042 1039271
#> 
#> units:  t 

# Vulnerable and exploitable biomass
vb(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 298568 322796 348338 374391 393937 442448 396140 367390 354946 442995
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 461854 453073 408956 382786 391648 377091 309240 319719 376574 423624
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 434357 424366 379396 353337 308942 342912 405810 459944 494812 551998
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 613625 598865 523168 438959 378675 332984 307925 263198 253165 231755
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 247161 270252 271272 224733 252989 267745 271234 294289 280962 304803
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 277946 339063 445309 521004 493669 476897 543880 623671 530886 503358
#>      year
#> age   2017  
#>   all 460610
#> 
#> units:  NA 
exb(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 299469 321906 361483 396857 410927 416469 404582 376864 399309 492220
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 500361 484571 458718 429971 445216 425793 379705 373232 412069 474675
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 454316 458942 402082 385826 368258 401543 453128 498126 542582 580248
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 635067 655903 592328 495035 418033 363958 344964 301632 270150 252101
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 249779 280817 311273 265536 253850 280453 284588 313860 311232 324276
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 306880 381386 490461 563606 569810 582287 619392 647453 657521 617996
#>      year
#> age   2017  
#>   all 559422
#> 
#> units:  NA 
```
