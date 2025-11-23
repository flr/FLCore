<div id="main" class="col-md-9" role="main">

# Method wireframe

<div class="ref-description section level2">

3D plot for FLQuant objects

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'formula,FLQuant'
wireframe(x, data, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    a `formula` formula for lattice

-   data:

    a `FLQuant` object with the values

-   ...:

    Additional argument list to be passed to `wireframe`

</div>

<div class="section level2">

## Value

a `wireframe` plot

</div>

<div class="section level2">

## Details

Method to plot 3D representations of FLQuant objects

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
wireframe(data~age+year, data=harvest(ple4))

```

</div>

</div>

</div>
