<div id="main" class="col-md-9" role="main">

# Class FLParJK

<div class="ref-description section level2">

A class for storing parameters of a jackknifed model fit.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'ANY'
FLParJK(object, orig)

# S4 method for class 'FLParJK'
orig(object)
```

</div>

</div>

<div class="section level2">

## Slots

-   `.Data`:

    Jackknifed object, `FLPar`.

-   `units`:

    units of measurement, `character`.

-   `orig`:

    original object being jackknifed, `FLPar`.

</div>

<div class="section level2">

## Validity

You can inspect the class validity function by using
`getValidity(getClassDef('FLParJK'))`

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

Objects of this class are commonly created by calling the `jackknife()`
method A construction method exists for this class that can take named
arguments for any of its slots. All slots are then created to match the
requirements of the class validity.

</div>

<div class="section level2">

## See also

<div class="dont-index">

`FLPar`

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

</div>
