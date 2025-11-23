<div id="main" class="col-md-9" role="main">

# A class for jackknifing fisheries data

<div class="ref-description section level2">

This extended FLQuant class holds both a jackknifed FLQuant, one in
which each iter is missing one element, and the original object, as a
separate `FLQuant` in the `orig` slot.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'ANY'
FLQuantJK(object, orig)

# S4 method for class 'FLQuantJK'
orig(object)

# S4 method for class 'FLQuants'
orig(object)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    Input numeric object

-   ...:

    Additonal arguments

</div>

<div class="section level2">

## Slots

-   .Data:

    Unnamed slot containing the jackknifed
    object([FLQuant](FLQuant.md)).

-   orig:

    Original object, ([FLQuant](FLQuant.md)).

</div>

<div class="section level2">

## Validity

-   slot dims:

    .Data and orig slots must have the same dimensions 1-5.

-   slot dimnames:

    .Data and var slots must have the same dimnames 1-5.

You can inspect the class validity function by using
`getValidity(getClassDef('FLQuantJK'))`

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

Objects of this class must be constructed from an [FLQuant](FLQuant.md)
that is to be jackknifed, through the `jackknife` method.

</div>

<div class="section level2">

## Methods

All methods defined for the [FLQuant](FLQuant.md) class are available,
but they will operate only on the jackknifed (`.Data`) slot. Please use
`orig()` to apply them to the original object stored in the class.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLQuant](FLQuant.md)

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
fjk <- jackknife(stock(ple4))
# New object has as many iters as length of jackknifed dimension (defaults to 'year')
dim(fjk)
#> [1]  1 61  1  1  1 61
```

</div>

</div>

</div>
