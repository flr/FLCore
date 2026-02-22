# A class for jackknifing fisheries data

This extended FLQuant class holds both a jackknifed FLQuant, one in
which each iter is missing one element, and the original object, as a
separate `FLQuant` in the `orig` slot.

## Usage

``` r
# S4 method for class 'ANY'
FLQuantJK(object, orig)

# S4 method for class 'FLQuantJK'
orig(object)

# S4 method for class 'FLQuants'
orig(object)
```

## Arguments

- object:

  Input numeric object

- ...:

  Additonal arguments

## Slots

- .Data:

  Unnamed slot containing the jackknifed object([FLQuant](FLQuant.md)).

- orig:

  Original object, ([FLQuant](FLQuant.md)).

## Validity

- slot dims:

  .Data and orig slots must have the same dimensions 1-5.

- slot dimnames:

  .Data and var slots must have the same dimnames 1-5.

You can inspect the class validity function by using
`getValidity(getClassDef('FLQuantJK'))`

## Accessors

All slots in the class have accessor and replacement methods defined
that allow retrieving and substituting individual slots.

The values passed for replacement need to be of the class of that slot.
A numeric vector can also be used when replacing FLQuant slots, and the
vector will be used to substitute the values in the slot, but not its
other attributes.

## Constructor

Objects of this class must be constructed from an [FLQuant](FLQuant.md)
that is to be jackknifed, through the [`jackknife`](jackknife.md)
method.

## Methods

All methods defined for the [FLQuant](FLQuant.md) class are available,
but they will operate only on the jackknifed (`.Data`) slot. Please use
`orig()` to apply them to the original object stored in the class.

## See also

[FLQuant](FLQuant.md)

## Author

The FLR Team

## Examples

``` r
data(ple4)
fjk <- jackknife(stock(ple4))
# New object has as many iters as length of jackknifed dimension (defaults to 'year')
dim(fjk)
#> [1]  1 61  1  1  1 61
```
