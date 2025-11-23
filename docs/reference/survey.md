<div id="main" class="col-md-9" role="main">

# A method to generate observations of abundance at age.

<div class="ref-description section level2">

A method to generate observations of abundance at age.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
survey(object, index, ...)

# S4 method for class 'FLStock,FLIndex'
survey(
  object,
  index,
  sel = sel.pattern(index),
  ages = dimnames(index)$age,
  timing = mean(range(index, c("startf", "endf"))),
  index.q = index@index.q,
  stability = 1
)

# S4 method for class 'FLStock,FLIndexBiomass'
survey(
  object,
  index,
  sel = sel.pattern(index),
  ages = ac(seq(range(index, c("min")), range(index, c("max")))),
  timing = mean(range(index, c("startf", "endf"))),
  catch.wt = index@catch.wt,
  index.q = index@index.q,
  stability = 1
)

# S4 method for class 'FLStock,missing'
survey(
  object,
  sel = catch.sel(object),
  ages = dimnames(sel)$age,
  timing = 0.5,
  index.q = 1,
  biomass = FALSE,
  stability = 1
)

# S4 method for class 'FLStock,FLIndices'
survey(object, index, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    The object on which to draw the observation

</div>

<div class="section level2">

## Value

An FLQuant for the index of abundance

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
data(ple4.index)
# CONSTRUCT a survey from stock and index
survey(ple4, ple4.index)
#> An object of class "FLIndex"
#> 
#> Name: BTS-Combined (all) 
#> Description: Plaice in IV                                             . Imported from VPA file. 
#> Type :  numbers 
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  22  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   10  1   1996    2017    0.6453376   0.6453376   
#> 
# Create FLIndexBiomass
ple4.biom <- as(ple4.index, "FLIndexBiomass")
survey(ple4, ple4.biom)
#> An object of class "FLIndexBiomass"
#> 
#> Name: BTS-Combined (all) 
#> Description: Plaice in IV                                             . Imported from VPA file. 
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  22  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   10  1   1996    2017    0.6453376   0.6453376   
#> 
data(ple4)
survey(ple4)
#> An object of class "FLIndex"
#> 
#> Name:  
#> Description:  
#> Type :  number 
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   10  10  1957    2017    0.5 0.5 
#> 
survey(ple4, biomass=TRUE)
#> An object of class "FLIndexBiomass"
#> 
#> Name:  
#> Description:  
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   10  10  1957    2017    0.5 0.5 
#> 
```

</div>

</div>

</div>
