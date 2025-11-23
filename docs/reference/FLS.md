<div id="main" class="col-md-9" role="main">

# Class FLS

<div class="ref-description section level2">

A virtual class that forms the basis for the `FLStock` and `FLStockLen`
classes. No objects of this class can be constructed.

</div>

<div class="section level2">

## Validity

-   None:

    No particular validity checks

</div>

<div class="section level2">

## Slots

-   catch:

    Total catch weight (`FLQuant`).

-   catch.n:

    Catch numbers (`FLQuant`).

-   catch.wt:

    Mean catch weights (`FLQuant`).

-   desc:

    Description of the stock (`character`).

-   discards:

    Total discards weight (`FLQuant`).

-   discards.n:

    Discard numbers (`FLQuant`).

-   discards.wt:

    Mean discard weights (`FLQuant`).

-   landings:

    Total landings weight (`FLQuant`).

-   landings.n:

    Landing numbers (`FLQuant`).

-   landings.wt:

    Landing weights (`FLQuant`).

-   stock:

    Total stock weight (`FLQuant`).

-   stock.n:

    Stock numbers (`FLQuant`).

-   stock.wt:

    Mean stock weights (`FLQuant`).

-   m:

    Natural mortality (`FLQuant`).

-   m.spwn:

    Proportion of natural mortality before spawning (`FLQuant`).

-   mat:

    Proportion mature (`FLQuant`).

-   harvest:

    Harvest rate or fishing mortality. The units of this slot should be
    set to 'harvest' or 'f' accordingly (`FLQuant`).

-   harvest.spwn:

    Proportion of harvest/fishing mortality before spawning (`FLQuant`).

-   name:

    Name of the stock (`character`).

-   range:

    Named numeric vector containing the quant and year ranges, the
    plusgroup and the quant range that the average fishing mortality
    should be calculated over (`numeric`).

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
