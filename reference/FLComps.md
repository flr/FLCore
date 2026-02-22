# Class FLComps

A virtual class that forms the basis for many FLR list classes. No
objects of this class can be constructed.

## Arguments

- object:

  unnamed object to be added to the list

- ...:

  other named or unnamed objects

## Validity

- Elements:

  All elements must be of a class that inherits from FLComp

## Slots

- .Data:

  The data. `list`.

- names:

  Names of the list elements. `character`.

- desc:

  Description of the object. `character`.

- lock:

  Lock mechanism, if turned on the length of the list can not be
  modified by adding or removing elements. `logical`.

## Constructor

A constructor method exists for this class that can take named arguments
for any of the list elements.

## See also

[FLlst](FLlst.md), [FLComp](FLComp.md)

## Author

The FLR Team
