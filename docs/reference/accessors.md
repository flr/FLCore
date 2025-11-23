<div id="main" class="col-md-9" role="main">

# accessor and replacement methods for FLCore classes

<div class="ref-description section level2">

All S4 classes defined in FLCore have methods for accessing and
replacing any of their slots. These methods are named as the slot, and
will return the content of the slot, for the accessor method, or modify
it with the provided value.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
name(object, ...)

desc(object, ...)

range(x, i) <- value

catch(object, ...)

catch.n(object, ...) <- value

catch.wt(object, ...)

discards(object, ...)

discards.n(object, ...)

discards.wt(object, ...)

landings(object, ...)

landings.n(object, ...)

landings.wt(object, ...)

m(object, ...)

stock(object, ...)

stock.n(object, ...)

stock.wt(object, ...)

m.spwn(object, ...)

harvest(object, catch, ...)

harvest.spwn(object, ...)

mat(object, ...)

n(object, ...)

m(object, ...)

wt(object, ...)

fec(object, ...)

spwn(object, ...)

effort(object, metier, ...)

type(object, ...)

distr(object, ...)

distribution(object, ...)

index(object, ...)

index.var(object, ...)

catch.n(object, ...)

catch.wt(object, ...)

sel.pattern(object, ...)

index.q(object, ...)

model(object, ...)

logl(object, ...)

gr(object, ...)

initial(object, ...)

logLik(object, ...)

vcov(object, ...) <- value

hessian(object, ...)

logerror(object, ...)

details(object, ...)

residuals(object, ...) <- value

fitted(object, ...)

rec(object, ...)

rec.obs(object, ...)

catch.q(object, ...)

discards.sel(object, ...)

landings.sel(object, ...)

params(object, ...)

# S4 method for class 'FLS,FLQuants'
catch(object) <- value
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    The object from which a slot is to be extracted or replaced

-   value:

    Object to be inserted into the relevant slot

</div>

<div class="section level2">

## Value

The required slot, for an accessor method, or invisible modifies the
object, for the replacement one.

</div>

<div class="section level2">

## Details

Accessors and replacement methods, with some exception, are created at
build time by calls to the `createFLAccessors` function. An accessor
method is created for each slot, with simply calls `slot()` on the
relevant slot name. For slots of class `FLQuant`, or `FLArray`-based,
two methods are created: one if `value` is of class `FLQuant`, and
another for `value` being a numeric vector. The later would insert the
vector into the slot structure, using R's recycling rules.

Users are encouraged to use the accessor methods, rather than the '@'
operator or the `slot()` method, to isolate code from the internal
structure of the class. If a slot was to be altered or deleted in the
future, a method would be provided to return the same value, computed
from other slots.

Some of these methods might already not access directly an slot, and
instead carry out a calculation to return the requested value, depending
on the class being called with. Please refer to the particular method
implementation to see if this is the case.

Accessor methods for slots of class `predictModel` behave differently
depending on the `compute` argument. Please refer to the relevant help
page for further clarification.

An object of class FLQuants, containing three elements named *catch*,
*catch.n* and *catch.wt*, as returned by `computeCatch`, can be assigned
directly to an object using *catch\<-*.

</div>

<div class="section level2">

## See also

<div class="dont-index">

`FLQuant`, `FLStock`, `FLIndex`, `FLBiol`, `predictModel`

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

# To access the catch slot in an FLStock, use
catch(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all  78360  88785 105186 117975 119541 126290 140815 147540 151408 162266
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 154474 149820 146178 136619 141226 149390 151515 157994 165392 175881
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 165843 178166 172652 184690 184494 192439 212632 228265 247071 279228
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 308480 315245 292035 250604 218184 192691 179573 151248 132629 131719
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 152195 171240 170662 145998 128107 143807 154029 140056 114551 111864
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 104770 113397 115703 118824 119718 131872 141055 139750 137338 131216
#>      year
#> age   2017  
#>   all 124922
#> 
#> units:  t 

# while to modify it, do
catch(ple4) <- catch(ple4) * 2

# A number can be used as input, to be recycled
m(ple4) <- 0.3
# same as a longer vector, by age
m(ple4) <- 0.4^(seq(1, 2, length=10))

# To see the methods defined by createFLAccessors, run, for example
getMethod('catch', 'FLS')
#> new("MethodDefinition", .Data = function (object, ...) 
#> {
#>     .local <- function (object) 
#>     return(slot(object, "catch"))
#>     .local(object, ...)
#> }, target = new("signature", .Data = "FLS", names = "object", 
#>     package = "FLCore"), defined = new("signature", .Data = "FLS", 
#>     names = "object", package = "FLCore"), generic = "catch")
#> <bytecode: 0x563fa894b828>
#> <environment: 0x563fa8933320>
#> attr(,"target")
#> An object of class “signature”
#> object 
#>  "FLS" 
#> attr(,"defined")
#> An object of class “signature”
#> object 
#>  "FLS" 
#> attr(,"generic")
#> [1] "catch"
#> attr(,"generic")attr(,"package")
#> [1] "FLCore"
#> attr(,"class")
#> [1] "MethodDefinition"
#> attr(,"class")attr(,"package")
#> [1] "methods"

# Assign the 3 catch slots
catch(ple4) <- computeCatch(ple4, slot="all")
```

</div>

</div>

</div>
