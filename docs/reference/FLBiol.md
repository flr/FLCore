<div id="main" class="col-md-9" role="main">

# Class FLBiol

<div class="ref-description section level2">

A class for modelling age / length or biomass structured populations.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLBiol(object, ...)

# S4 method for class 'FLQuant'
FLBiol(object, plusgroup = dims(object)$max, ...)
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

The `FLBiol` class is a representation of a biological fish population.
This includes information on abundances, natural mortality and
fecundity.

</div>

<div class="section level2">

## Slots

-   n:

    Numbers in the population. `FLQuant`.

-   m:

    Mortality rate of the population. `FLQuant`.

-   wt:

    Mean weight of an individual. `FLQuant`.

-   mat:

    `predictModel`.

-   fec:

    `predictModel`.

-   rec:

    `predictModel`.

-   spwn:

    Proportion of time step at which spawning ocurrs. `FLQuant`.

-   name:

    Name of the object. `character`.

-   desc:

    Brief description of the object. `character`.

-   range:

    Named numeric vector describing the range of the object. `numeric`.

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

## Validity

-   Dimensions:

    All FLQuant slots must have iters equal to 1 or 'n'.

-   Iters:

    The dimname for iter1 should be '1'.

-   Dimnames:

    The name of the quant dimension must be the same for all FLQuant
    slots.

</div>

<div class="section level2">

## See also

<div class="dont-index">

as.FLBiol, as.FLSR, [coerce](https://rdrr.io/r/methods/setAs.html),
[plot](https://rdrr.io/r/graphics/plot.default.html), [ssb](ssb.md)
catch.n,FLBiol-method

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
# An FLBiol example dataset
data(ple4.biol)

summary(ple4.biol)
#> An object of class "FLBiol"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear 
#>  1   10  10  1957    2017    
#> 
#> mat           ~ mat 
#>   mat         : [ 10 61 1 1 1 1 ], units =   
#>   NA          : [ 1 1 ], units =  NA 
#> fec           ~ fec 
#>   fec         : [ 10 61 1 1 1 1 ], units =   
#>   NA          : [ 1 1 ], units =  NA 
#> rec           ~ rec a * ssb * exp(-b * ssb) 
#>   residuals   : [ 1 60 1 1 1 1 ], units =   
#>   a, b        : [ 2 1 ], units =    
```

</div>

</div>

</div>
