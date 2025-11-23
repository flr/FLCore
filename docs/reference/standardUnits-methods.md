<div id="main" class="col-md-9" role="main">

# Standard units of measurement for a complex class object

<div class="ref-description section level2">

Returns values for the *units* of each *FLQuant* slot according to the
standard adopted by the FLR Team for the supplied class.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
standardUnits(object, ...)

# S4 method for class 'character'
standardUnits(object, ...)

# S4 method for class 'FLS'
standardUnits(object, ...)

# S4 method for class 'FLBiol'
standardUnits(object, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    for which the standard *units* are to be returned

</div>

<div class="section level2">

## Value

A list with the corresponding *units* value for each slot

</div>

<div class="section level2">

## Details

For objects derived from class *FLS*, which currently includes *FLStock*
and *FLStockLen*, the adopted standard includes: 'kg' for individual
weights, '1000' for number of individuals, 't' for biomass, 'f' for
harvest, 'm' for natural mortality, and an empty string for proportions
(spwn, mat).

For objects derived of class *FLBiol* the adopted standard units are:
'kg' for individual weights, '1000' for number of individuals, 'm' for
natural mortality, and an empty string for proportions (spwn, mat).

</div>

<div class="section level2">

## See also

<div class="dont-index">

`units-FLCore`

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
stk <- FLStock(catch=FLQuant(runif(20, 2, 120)))
# FLStock object has no units
summary(stk)
#> An object of class "FLStock"
#> 
#> Name:  
#> Description:  
#> Quant: quant 
#> Dims:  quant     year    unit    season  area    iter
#>  1   20  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  NA  NA  NA  1   20  NA  NA  
#> 
#> Metrics: 
#>   rec: NA - NA (NA)
#>   ssb: NA - NA  (NA) 
#>   catch: 17 - 90  (NA) 
#>   fbar: NA - NA (NA)
# Obtain standard units for the class as a list
standardUnits(stk)
#> $catch.wt
#> [1] "kg"
#> 
#> $landings.wt
#> [1] "kg"
#> 
#> $discards.wt
#> [1] "kg"
#> 
#> $stock.wt
#> [1] "kg"
#> 
#> $catch
#> [1] "t"
#> 
#> $landings
#> [1] "t"
#> 
#> $discards
#> [1] "t"
#> 
#> $stock
#> [1] "t"
#> 
#> $catch.n
#> [1] "1000"
#> 
#> $landings.n
#> [1] "1000"
#> 
#> $discards.n
#> [1] "1000"
#> 
#> $stock.n
#> [1] "1000"
#> 
#> $mat
#> [1] ""
#> 
#> $harvest.spwn
#> [1] ""
#> 
#> $m.spwn
#> [1] ""
#> 
#> $m
#> [1] "m"
#> 
#> $harvest
#> [1] "f"
#> 
# which can then be assigned to the object
units(stk) <- standardUnits(stk)
summary(stk)
#> An object of class "FLStock"
#> 
#> Name:  
#> Description:  
#> Quant: quant 
#> Dims:  quant     year    unit    season  area    iter
#>  1   20  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  NA  NA  NA  1   20  NA  NA  
#> 
#> Metrics: 
#>   rec: NA - NA (NA)
#>   ssb: NA - NA  (t) 
#>   catch: 17 - 90  (t) 
#>   fbar: NA - NA (NA)
# units<- methjod also accepts a function to be called to provide units
units(stk) <- standardUnits
bio <- FLBiol(n=FLQuant(runif(50, 2, 120), dim=c(5, 10)))
# Object has no units
summary(bio)
#> An object of class "FLBiol"
#> 
#> Name:  
#> Description:  
#> Quant: quant 
#> Dims:  quant     year    unit    season  area    iter
#>  5   10  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear 
#>  1   5   5   1   10  
#> 
#> mat           ~ mat 
#>   mat         : [ 5 10 1 1 1 1 ], units =  NA 
#>   NA          : [ 1 1 ], units =  NA 
#> fec           ~ fec 
#>   fec         : [ 5 10 1 1 1 1 ], units =  NA 
#>   NA          : [ 1 1 ], units =  NA 
#> rec           ~ rec 
#>   rec         : [ 1 10 1 1 1 1 ], units =  NA 
#>   NA          : [ 1 1 ], units =  NA 
# Obtain standard units for the class as a list
standardUnits(bio)
#> $n
#> [1] "1000"
#> 
#> $m
#> [1] "m"
#> 
#> $wt
#> [1] "kg"
#> 
#> $spwn
#> [1] ""
#> 
#> $mat
#> [1] "mat"
#> 
#> $fec
#> [1] "NA"
#> 
#> $rec
#> [1] "1000"
#> 
# which can then be assigned to the object
units(bio) <- standardUnits(bio)
summary(stk)
#> An object of class "FLStock"
#> 
#> Name:  
#> Description:  
#> Quant: quant 
#> Dims:  quant     year    unit    season  area    iter
#>  1   20  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  NA  NA  NA  1   20  NA  NA  
#> 
#> Metrics: 
#>   rec: NA - NA (NA)
#>   ssb: NA - NA  (t) 
#>   catch: 17 - 90  (t) 
#>   fbar: NA - NA (NA)
```

</div>

</div>

</div>
