<div id="main" class="col-md-9" role="main">

# Class FLComp

<div class="ref-description section level2">

A virtual class that forms the basis for most FLR classes composed of
slots of class `FLQuant`. No objects of this class can be constructed.

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

## Slots

-   name:

    A character vector for the object name.

-   desc:

    A textual description of the object contents.

-   range:

    A named numeric vector with various values of quant and year ranges,
    plusgroup, fishing mortality ranges, etc. Elements are specific to
    each child class.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[\[](https://rdrr.io/r/base/Extract.html), \[\<-,
[as.data.frame](https://rdrr.io/r/base/as.data.frame.html),
[iter](iter.md), [propagate](propagate.md), qapply,
[summary](https://rdrr.io/r/base/summary.html),
[transform](https://rdrr.io/r/base/transform.html), [trim](trim.md),
[units,FLComp-method](units-FLCore.md), units\<-,FLComp,list-method,
[window](https://rdrr.io/r/stats/window.html)

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

</div>
