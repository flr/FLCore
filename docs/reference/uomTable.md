<div id="main" class="col-md-9" role="main">

# Table for conversions and operations between units of measurement

<div class="ref-description section level2">

-   uom defaults to NA unless defined below.

-   unit +/- itself, returns the same unit (e.g. kg + kg = kg)

-   numeric unit \* 1 returns same unit (e.g. 1e4 \* 1 = 1e4)

-   numeric unit \* numeric unit returns product (e.g. 10 \* 100 = 1000)

-   unit / unit returns "" (e.g. 100 / 100 = "")

-   numeric unit / smaller numeric unit returns division (e.g. 100 / 10
    = 10)

-   100 times kg returns t

-   numeric unit \* 'kg' returns the product in tonnes (e.g. kg \* 1e4 =
    t \* 10)

-   units with divisions are parsed (e.g. days/boat \* boat = days)

-   

-   

</div>

<div class="section level2">

## Format

An object of class array

</div>

</div>
