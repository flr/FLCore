<div id="main" class="col-md-9" role="main">

# Class FLStock

<div class="ref-description section level2">

A class for modelling a fish stock.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLStock(object, ...)

# S4 method for class 'FLQuant'
FLStock(object, plusgroup = dims(object)$max, ...)

# S4 method for class 'missing'
FLStock(object, ...)

# S4 method for class 'FLQuants'
FLStock(object, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    FLQuant object used for sizing

-   ...:

    Other objects to be assigned by name to the class slots

-   plusgroup:

    Plusgroup age, to be stored in range

</div>

<div class="section level2">

## Details

The `FLStock` object contains a representation of a fish stock as
constructed for the purposes of scientific analysis and advice. This
includes information on removals (i.e. catches, landings and discards),
maturity, natural mortality and the results of an analytical assessment
(i.e. estimates of abundance and removal rates) .

</div>

<div class="section level2">

## Slots

-   catch:

    Total catch weight (`FLQuant`).

-   catch.n:

    Catch numbers (`FLQuant`).

-   catch.wt:

    Mean catch weights (`FLQuant`).

-   discards:

    Total discards weight (`FLQuant`).

-   discards.n:

    Discard numbers (`FLQuant`).

-   discards.wt:

    Mean discard weights (`FLQuant`).

-   landings:

    Total landings weight (`FLQuant`).

-   landings.n:

    Landing numbers (`FLQuant`).

-   landings.wt:

    Landing weights (`FLQuant`).

-   stock:

    Total stock weight (`FLQuant`).

-   stock.n:

    Stock numbers (`FLQuant`).

-   stock.wt:

    Mean stock weights (`FLQuant`).

-   m:

    Natural mortality (`FLQuant`).

-   mat:

    Proportion mature (`FLQuant`).

-   harvest:

    Harvest rate or fishing mortality. The units of this slot should be
    set to 'hr' or 'f' accordingly (`FLQuant`).

-   harvest.spwn:

    Proportion of harvest/fishing mortality before spawning (`FLQuant`).

-   m.spwn:

    Proportion of natural mortality before spawning (`FLQuant`).

-   name:

    Name of the stock (`character`).

-   desc:

    Description of the stock (`character`).

-   range:

    Named numeric vector containing the quant and year ranges, the
    plusgroup and the quant range that the average fishing mortality
    should be calculated over (`numeric`).

</div>

<div class="section level2">

## Accessors

All slots in the class have accessor and replacement methods defined
that allow retrieving and substituting individual slots.

The values passed for replacement need to be of the class of that slot.
A numeric vector can also be used when replacing FLQuant slots, and the
vector will be used to substitute the values in the slot, but not its
other attributes.

</div>

<div class="section level2">

## Constructor

A construction method exists for this class that can take named
arguments for any of its slots. All slots are then created to match the
requirements of the class validity. If an unnamed `FLQuant` object is
provided, this is used for sizing but not stored in any slot.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[\[](https://rdrr.io/r/base/Extract.html), \[\<-, as.FLBiol, as.FLSR,
[catch](accessors.md), catch\<-, [catch.n](accessors.md), catch.n\<-,
[catch.wt](accessors.md), catch.wt\<-,
[coerce](https://rdrr.io/r/methods/setAs.html),
[computeCatch](compute.md), [computeDiscards](compute.md),
[computeLandings](compute.md), [discards](accessors.md), discards\<-,
[discards.n](accessors.md), discards.n\<-, [discards.wt](accessors.md),
discards.wt\<-, [harvest](accessors.md), harvest\<-,
[harvest.spwn](accessors.md), [landings](accessors.md), landings\<-,
[landings.n](accessors.md), landings.n\<-, [landings.wt](accessors.md),
landings.wt\<-, [m](accessors.md), m\<-, [mat](accessors.md),
[m.spwn](accessors.md),
[plot](https://rdrr.io/r/graphics/plot.default.html), [ssb](ssb.md),
ssbpurec, [stock](accessors.md), [stock.n](accessors.md),
[stock.wt](accessors.md), [trim](trim.md), [FLComp](FLComp.md)

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
summary(ple4)
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   10  10  1957    2017    2   6   
#> 
#> Metrics: 
#>   rec: 367450 - 4303680  (1000) 
#>   ssb: 203391 - 913290  (t) 
#>   catch: 78360 - 315245  (t) 
#>   fbar: 0.20 - 0.72  (f) 

# get the landings slot and assign values to it
  landings(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all  70926  74157  78178  88764  85267  90305 103162 111121 105424  98334
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 103947 121020 122661 111783 117301 130443 133768 115181  94458 122166
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 108524 128366 119827 150640 151304 145669 143690 162681 182374 166633
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 155005 168118 187666 174414 147844 134793 141800 126195 109681  93642
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all  82875  74217  97340 100318  66596  87138  74770  82529  61501  62161
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all  59713  64574  68842  74246  74437  82307  97123  86424  91033  86381
#>      year
#> age   2017  
#>   all  84750
#> 
#> units:  t 
  landings(ple4) <- apply(landings.n(ple4)*landings.wt(ple4),2,sum)

# perform similar calculation as the preceding apply function
  discards(ple4) <- computeDiscards(ple4)
  catch(ple4) <- computeCatch(ple4)
  catch(ple4) <- computeCatch(ple4, slot="all")

# set the units of the harvest slot of an FLStock object
  harvest(ple4) <- 'f'

# subset and trim the FLStock
  ple4[,1]
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  1   1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   10  10  1957    1957    2   6   
#> 
#> Metrics: 
#>   rec: 477074 - 477074  (1000) 
#>   ssb: 342223 - 342223  (t) 
#>   catch: 78360 - 78360  (t) 
#>   fbar: 0.24 - 0.24  (f) 
  trim(ple4, age=2:6, year=1980:1990)
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  5   11  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  2   6   NA  1980    1990    2   6   
#> 
#> Metrics: 
#>   rec: 603278 - 2866780  (1000) 
#>   ssb: 221023 - 422517  (t) 
#>   catch: 184494 - 315245  (t) 
#>   fbar: 0.52 - 0.63  (f) 

# Calculate SSB, and SSB per recruit at zero fishing mortality
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
  ssbpurec(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957
#>   all 5.84
#> 
#> units:  NA 

# Coerce an FLStock to an FLBiol
  biol <- as(ple4, "FLBiol")

# Initialise an FLSR object from an FLStock
  flsr <- as.FLSR(ple4)
```

</div>

</div>

</div>
