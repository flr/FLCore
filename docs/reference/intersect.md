<div id="main" class="col-md-9" role="main">

# Returns FLR objects trimmed to their shared dimensions.

<div class="ref-description section level2">

Objects sharing certain dimensions, as inferred by their *dimnames*, are
subset to the common ones along all dimensions. The returned object is
of one of the *FLlst* classes, as corresponds to the input class. The
objects in the list can then be, for example, combined or directly
compared, as shown in the examples.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLArray,FLArray'
intersect(x, y)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    First object to be compared and subset

-   y:

    Second object to be compared and subset

</div>

<div class="section level2">

## Value

And object of the corresponding *FLsdt*-based plural class.

</div>

<div class="section level2">

## See also

<div class="dont-index">

base::intercept

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
big <- FLQuant(64.39, dimnames=list(age=1:4, year=2001:2012))
small <- FLQuant(3.52, dimnames=list(age=2:3, year=2001:2005))
intersect(big, small)
#> $ NA 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2001 2002 2003 2004 2005
#>   2 64.4 64.4 64.4 64.4 64.4
#>   3 64.4 64.4 64.4 64.4 64.4
#> 
#> units:  NA 
#> 
#> $ NA 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2001 2002 2003 2004 2005
#>   2 3.52 3.52 3.52 3.52 3.52
#>   3 3.52 3.52 3.52 3.52 3.52
#> 
#> units:  NA 
#> 

# Two FLQuant objects can be added along their common dimension using Reduce() 
Reduce('+', intersect(big, small))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2001 2002 2003 2004 2005
#>   2 67.9 67.9 67.9 67.9 67.9
#>   3 67.9 67.9 67.9 67.9 67.9
#> 
#> units:  NA 
```

</div>

</div>

</div>
