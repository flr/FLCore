# A class for samples of a probability distribution

This extended FLQuant class holds both a measure of central tendendy
(mean, median) and of dispersion (tipically variance), to be later used
to generate, for example, random numbers with those mean and variances.

## Usage

``` r
FLQuantDistr(object, var, ...)

# S4 method for class 'ANY,ANY'
FLQuantDistr(object, var, ...)

# S4 method for class 'FLQuant,FLQuant'
FLQuantDistr(object, var, units = object@units, distr = "norm")
```

## Arguments

- object:

  Input numeric object

- ...:

  Additonal arguments

## Slots

- .Data:

  Unnamed slot for storing the mean (or other measure of expectation)
  (`FLQuant`).

- var:

  Variance, or other measure of dispersion, (`FLQuant`).

- distr:

  Name of the probability distribution, see Details (`character`).

## Validity

- slot dims:

  .Data and var slots must have the same dimensions.

- slot dimnames:

  .Data and var slots must have the same dimnames.

You can inspect the class validity function by using
`getValidity(getClassDef('FLQuantDistr'))`

## Accessors

All slots in the class have accessor and replacement methods defined
that allow retrieving and substituting individual slots.

The values passed for replacement need to be of the class of that slot.
A numeric vector can also be used when replacing FLQuant slots, and the
vector will be used to substitute the values in the slot, but not its
other attributes.

The contents of the unnamed slot (.Data) can be accessed through the
`e()` method, see Example below.

## Constructor

A construction method exists for this class that can take named
arguments for any of its slots. All slots are then created to match the
requirements of the class validity. If an unnamed `FLQuant` object is
provided, this is used for the .Data slot.

## Methods

Methods exist for various calculations based on values stored in the
class:

- Arith:

  .

## Arithmetic

The methods under the *Arith* group have been defined for objects of
this class, both for operations between two `FLQuantDistr` objects and
with objects of class `FLQuant` (`FLArray`) as follows:

- `+`, FLQuantDistr,FLArray:

  .

- `-`, FLQuantDistr,FLArray:

  .

- `*`, FLQuantDistr,FLArray:

  .

- `/`, FLQuantDistr,FLArray:

  .

- `+`, FLQuantDistr,FLQuantDistr:

  .

- `-`, FLQuantDistr,FLQuantDistr:

  .

- `*`, FLQuantDistr,FLQuantDistr:

  .

## See also

[FLQuant](FLQuant.md)

## Author

The FLR Team

## Examples

``` r
data(ple4)
fqd <- FLQuantDistr(catch.n(ple4), var=catch.n(ple4) * 10, distr='norm')
```
