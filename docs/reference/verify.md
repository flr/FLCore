<div id="main" class="col-md-9" role="main">

# Verify FLR objects

<div class="ref-description section level2">

Verifies the content of FLR objects according to a set of rules

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
verify(object, ...)

# S4 method for class 'FLComp'
verify(object, ..., report = TRUE)

# S4 method for class 'FLStock'
verify(object, rules = ruleset(object), ..., report = TRUE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    An object of any FLR class for which the method has been defined

-   ...:

    Additional rules to be tested, as a formula or list. See details

-   report:

    Should the standard data.frame report be output (if TRUE) or a
    single logical value for all tests?

-   rules:

    Basic set of rules for a given class, as returned by ruleset().

</div>

<div class="section level2">

## Value

A data.frame with the results of applying those rules, or a single
logical value, if report=FALSE

</div>

<div class="section level2">

## Details

Classes' validity functions generally check the structure and dimensions
of objects and their component slots. But some checks on the data
content of objects is often required. The various verify methods
implement both a system to create *rules* that an object is expected to
pass, and a minimum standard set of rules for each defined class

The data.frame output by the method when `report=TRUE` contains one row
per rule and the following columns:

-   *name*, the rule name

-   *items*, number of comparisons carried out

-   *passes*, number of *TRUE* values

-   *fails*, number of *FALSE* values

-   *NAs*, number of logical NAs

-   *valid*, are all values *TRUE*?

-   *rule*, the expression being evaluated

Additional rules can be specify in a call to *verify*, in one of two
forms. Simple rules can be defined as a formula involving methods
defined for the class. A rule such as `highm = ~ m < 2` will check if
values in the *m* slot are higher than 2 and return a logical *FLQuant*.

Some rules cannot simply use existing methods or functions, for example
those operating on all slots of the object, or requiring additional
computations. In this case, the argument to *verify* can be a list, with
an element named *rule* of class *formula* and where test is defined.
The test then calls for a new function, defined as another element of
the list, and which will be used by verify when evaluating the set of
rules. See below for examples.

A set of rules has been defined for the *FLStock* class, available by
calling the ruleset method. The verify method for *FLStock* will by
default evaluate those rules, as well as any other defined in the call.

</div>

<div class="section level2">

## See also

<div class="dont-index">

`ruleset`

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
# Verifying a new rule for an FLSR object
data(nsher)
# rule: are all recruitment values greater than 0?
verify(nsher, rec=~rec > 0)
#>   name items passes fails NAs valid    rule
#> 1  rec    45     45     0   0  TRUE rec > 0

# Define rule calling its own function
data(ple4)
# rule: ssb is less
verify(ple4, ssbstock = ~ssb < stock)
#>            name items passes fails NAs valid
#> 1         anyna    17     17     0   0  TRUE
#> 2      catch.wt   610    610     0   0  TRUE
#> 3   landings.wt   610    594    16   0 FALSE
#> 4   discards.wt   610    431   179   0 FALSE
#> 5      stock.wt   610    610     0   0  TRUE
#> 6           mat   610    610     0   0  TRUE
#> 7  harvest.spwn   610    610     0   0  TRUE
#> 8        m.spwn   610    610     0   0  TRUE
#> 9       harvest   610    610     0   0  TRUE
#> 10      cohorts   336    336     0   0  TRUE
#> 11         uoms    17     17     0   0  TRUE
#> 12     ssbstock    61     61     0   0  TRUE
#>                                     rule
#> 1                                 !anyna
#> 2                           catch.wt > 0
#> 3                        landings.wt > 0
#> 4                        discards.wt > 0
#> 5                           stock.wt > 0
#> 6                    mat <= 1 & mat >= 0
#> 7  harvest.spwn <= 1 & harvest.spwn >= 0
#> 8              m.spwn <= 1 & m.spwn >= 0
#> 9                           harvest >= 0
#> 10                     ccohorts(stock.n)
#> 11                                  uoms
#> 12                           ssb < stock
data(ple4)
# verify for the standard set of rules for FLStock
verify(ple4)
#>            name items passes fails NAs valid
#> 1         anyna    17     17     0   0  TRUE
#> 2      catch.wt   610    610     0   0  TRUE
#> 3   landings.wt   610    594    16   0 FALSE
#> 4   discards.wt   610    431   179   0 FALSE
#> 5      stock.wt   610    610     0   0  TRUE
#> 6           mat   610    610     0   0  TRUE
#> 7  harvest.spwn   610    610     0   0  TRUE
#> 8        m.spwn   610    610     0   0  TRUE
#> 9       harvest   610    610     0   0  TRUE
#> 10      cohorts   336    336     0   0  TRUE
#> 11         uoms    17     17     0   0  TRUE
#>                                     rule
#> 1                                 !anyna
#> 2                           catch.wt > 0
#> 3                        landings.wt > 0
#> 4                        discards.wt > 0
#> 5                           stock.wt > 0
#> 6                    mat <= 1 & mat >= 0
#> 7  harvest.spwn <= 1 & harvest.spwn >= 0
#> 8              m.spwn <= 1 & m.spwn >= 0
#> 9                           harvest >= 0
#> 10                     ccohorts(stock.n)
#> 11                                  uoms
# verify a single rule from set
verify(ple4, rules=ruleset(ple4, 'anyna'), report=FALSE)
#> [1] TRUE

# add own rule to set
verify(ple4, m = ~m >=0)
#>            name items passes fails NAs valid
#> 1         anyna    17     17     0   0  TRUE
#> 2      catch.wt   610    610     0   0  TRUE
#> 3   landings.wt   610    594    16   0 FALSE
#> 4   discards.wt   610    431   179   0 FALSE
#> 5      stock.wt   610    610     0   0  TRUE
#> 6           mat   610    610     0   0  TRUE
#> 7  harvest.spwn   610    610     0   0  TRUE
#> 8        m.spwn   610    610     0   0  TRUE
#> 9       harvest   610    610     0   0  TRUE
#> 10      cohorts   336    336     0   0  TRUE
#> 11         uoms    17     17     0   0  TRUE
#> 12            m   610    610     0   0  TRUE
#>                                     rule
#> 1                                 !anyna
#> 2                           catch.wt > 0
#> 3                        landings.wt > 0
#> 4                        discards.wt > 0
#> 5                           stock.wt > 0
#> 6                    mat <= 1 & mat >= 0
#> 7  harvest.spwn <= 1 & harvest.spwn >= 0
#> 8              m.spwn <= 1 & m.spwn >= 0
#> 9                           harvest >= 0
#> 10                     ccohorts(stock.n)
#> 11                                  uoms
#> 12                                m >= 0
```

</div>

</div>

</div>
