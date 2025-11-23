<div id="main" class="col-md-9" role="main">

# A function to make available list elements inside a function or method

<div class="ref-description section level2">

Inside a function, a call to spread() will attach to the function
environment, sys.frame(), the elements in the list, or of the conversion
to list of the object (e.g. named vector or FLPar), so that they be
called by name. The function environment will be deleted once the
function returns, so those variables won't make it to the environment
from which the function was called, or further up in the call stack.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
spread(object, FORCE = FALSE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    A named list or vector whose elements are to be loaded into the
    calling environment.

-   FORCE:

    Should existing variable with matching names be redefined?

</div>

<div class="section level2">

## Value

Invisibly the names of the variables loaded into the calling
environment.

</div>

<div class="section level2">

## Details

By default, spread() will not overwrite variables in the function
environment with the same name as any list element, unless FORCE=TRUE

</div>

<div class="section level2">

## See also

<div class="dont-index">

[sys.nframe](https://rdrr.io/r/base/sys.parent.html)

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
# EXAMPLE function
foo <- function (params) {
  a <- spread(params)
 print(a)
 x*y
}
# x and y are accesible to the internal calculation
foo(params=list(x=3.5, y=9))
#> [1] "x" "y"
#> [1] 31.5

# Works with FLPar
foo(params=FLPar(x=3L, y=0.99238))
#> [1] "x" "y"
#> [1] 2.97714

# Elements in object must be named
if (FALSE) foo(list(3, y=0.99238)) # \dontrun{}

# If a variable is missing from the spread object, function will fail
if (FALSE) foo(list(x=4)) # \dontrun{}
# Unless the variable is already defined in the calling environment,
# in this case <environment: R_GlobalEnv>
y <- 45
foo(params=list(x=4))
#> [1] "x"
#> [1] 180
```

</div>

</div>

</div>
