<div id="main" class="col-md-9" role="main">

# uom Units of Measurement

<div class="ref-description section level2">

The 'units' attribute of FLQuant objects provides a mechanism for
keeping track of the units of measurement of that particular piece of
data.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
uom(op, u1, u2)

uomUnits(unit = missing)
```

</div>

</div>

<div class="section level2">

## Arguments

-   op:

    The arithmetic operator to be used, one of '+', '-', '\*' or '/'

-   u1:

    The units of measurement string of the first object

-   u2:

    The units of measurement string of the second object

-   unit:

    A character vector for one or more units to be compared with those
    known to `uom`.

</div>

<div class="section level2">

## Value

`uom` returns a string with the corresponding units of measurement, or a
character vector, showing the operation carried out, when units are not
known to `uom` or not compatible, e.g. "100 \* d".

`uomUnits` returns TRUE or FALSE if `unit` is given, otherwise a
character vector with all units known to `uom`.

</div>

<div class="section level2">

## Details

Arithmetic operators for 'FLQuant' objects are aware of a limited set of
units of measurement and will output the right unit when two object are
arithmetically combined. For example, the product of object with units
of 'kg' and '1000' will output an object with 'units' of 't' (for metric
tonnes).

Operations involving combinations of units not defined will issue a
warning, and the resulting 'units' attribute will simply keep a string
indicating the input units of measurement and the operation carried out,
as in '10 \* 1000'.

Note that no scaling or modification of the values in the object takes
place.

Conversion across units is carried out by the `uom()` function

The list of units known to `uom` is stored internally but can be queried
by calling `uomUnits()` with no arguments. If a character vector is
provided, a logical is returned telling whether the string is included
or not in that table.

</div>

<div class="section level2">

## See also

<div class="dont-index">

`FLQuant` `units,FLArray-method`

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
# Conversion between weights
FLQuant(1, units='kg') * FLQuant(1000, units='1')
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1   
#>   all 1000
#> 
#> units:  kg 

# Conversion between mortalities
FLQuant(0.2, units='m') + FLQuant(0.34, units='f')
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1   
#>   all 0.54
#> 
#> units:  z 

# Check if units are known
uomUnits('kg')
#> [1] TRUE
uomUnits('kell')
#> [1] FALSE
```

</div>

</div>

</div>
