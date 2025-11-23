<div id="main" class="col-md-9" role="main">

# Method dims

<div class="ref-description section level2">

List with information on object dimensions

List with information on object dimensions

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
dims(obj, ...)

dims(obj, ...)

# S4 method for class 'FLQuant'
dims(obj, element, ...)
```

</div>

</div>

<div class="section level2">

## Details

Method `dims` returns a named list with information on the dimensions
and dimension names of a given object. The list returned could be
extended in the future and currently contains, depending on the class of
the object, some of the following:

-   quant:

    Length of the first dimension

-   min:

    First quant

-   max:

    Last quant

-   year:

    Number of years

-   minyear:

    First year in series

-   maxyear:

    Last year in series

-   cohort:

    Number of cohorts

-   mincohort:

    First cohort in series

-   maxcohort:

    Last cohort in series

-   unit:

    Length of the third (`unit`) dimension

-   season:

    Length of the fourth (`season`) dimension

-   area:

    Length of the fifth (`area`) dimension

-   iter:

    Length of the sixth (`iter`) dimension

Values in the returned list are of class `numeric`, unless dimnames are
strings with no numeric translation, in which case the result is `NA`.

Please note that the name of the first element in the returned list
changes with the name of the first dimension in the input object. Use
`quant` to obtain the name and extract the relevant element from the
result list.

Method `dims` returns a named list with information on the dimensions
and dimension names of a given object. The list returned could be
extended in the future and currently contains, depending on the class of
the object, some of the following:

-   quant:

    Length of the first dimension

-   min:

    First quant

-   max:

    Last quant

-   year:

    Number of years

-   minyear:

    First year in series

-   maxyear:

    Last year in series

-   cohort:

    Number of cohorts

-   mincohort:

    First cohort in series

-   maxcohort:

    Last cohort in series

-   unit:

    Length of the third (`unit`) dimension

-   season:

    Length of the fourth (`season`) dimension

-   area:

    Length of the fifth (`area`) dimension

-   iter:

    Length of the sixth (`iter`) dimension

Values in the returned list are of class `numeric`, unless dimnames are
strings with no numeric translation, in which case the result is `NA`.

Please note that the name of the first element in the returned list
changes with the name of the first dimension in the input object. Use
`quant` to obtain the name and extract the relevant element from the
result list.

</div>

<div class="section level2">

## Generic function

dims(obj)

dims(obj)

</div>

<div class="section level2">

## See also

<div class="dont-index">

`dimnames`, `FLQuant`

`dimnames`, `FLQuant`

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
flq <- FLQuant(rnorm(96), dim=c(3,8,1,4), quant='age')
dims(flq)
#> $age
#> [1] 3
#> 
#> $min
#> [1] 1
#> 
#> $max
#> [1] 3
#> 
#> $year
#> [1] 8
#> 
#> $minyear
#> [1] 1
#> 
#> $maxyear
#> [1] 8
#> 
#> $unit
#> [1] 1
#> 
#> $season
#> [1] 4
#> 
#> $area
#> [1] 1
#> 
#> $iter
#> [1] 1
#> 

# Number of seasons
  dims(flq)$season
#> [1] 4

# Length of first dimension
  dims(flq)[[quant(flq)]]
#> [1] 3
```

</div>

</div>

</div>
