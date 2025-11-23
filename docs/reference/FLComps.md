<div id="main" class="col-md-9" role="main">

# Class FLComps

<div class="ref-description section level2">

A virtual class that forms the basis for many FLR list classes. No
objects of this class can be constructed.

</div>

<div class="section level2">

## Arguments

-   object:

    unnamed object to be added to the list

-   ...:

    other named or unnamed objects

</div>

<div class="section level2">

## Validity

-   Elements:

    All elements must be of a class that inherits from FLComp

</div>

<div class="section level2">

## Slots

-   .Data:

    The data. `list`.

-   names:

    Names of the list elements. `character`.

-   desc:

    Description of the object. `character`.

-   lock:

    Lock mechanism, if turned on the length of the list can not be
    modified by adding or removing elements. `logical`.

</div>

<div class="section level2">

## Constructor

A constructor method exists for this class that can take named arguments
for any of the list elements.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLlst](FLlst.md), [FLComp](FLComp.md)

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

</div>
