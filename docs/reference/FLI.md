<div id="main" class="col-md-9" role="main">

# Class FLI

<div class="ref-description section level2">

A VIRTUAL class that holds data and parameters related to abundance
indices.

</div>

<div class="section level2">

## Arguments

-   object:

    FLQuant object used for sizing

-   ...:

    Other objects to be assigned by name to the class slots

</div>

<div class="section level2">

## Slots

-   distribution:

    Statistical distribution of the index values (`character`).

-   index:

    Index values (`FLQuant`).

-   index.var:

    Variance of the index (`FLQuant`).

-   catch.n:

    Catch numbers used to create the index (`FLQuant`).

-   catch.wt:

    Catch weight of the index (`FLQuant`).

-   effort:

    Effort used to create the index (`FLQuant`).

-   sel.pattern:

    Selection pattern for the index (`FLQuant`).

-   index.q:

    Catchability of the index (`FLQuant`).

-   name:

    Name of the stock (`character`).

-   desc:

    General description of the object (`character`).

-   range:

    Range of the object (`numeric`)

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

[computeCatch](compute.md), [dims](dims.md), [iter](iter.md),
[plot](https://rdrr.io/r/graphics/plot.default.html),
[propagate](propagate.md),
[summary](https://rdrr.io/r/base/summary.html),
[transform](https://rdrr.io/r/base/transform.html), [trim](trim.md),
[window](https://rdrr.io/r/stats/window.html), [FLComp](FLComp.md)

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

</div>
