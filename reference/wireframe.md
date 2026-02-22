# Method wireframe

3D plot for FLQuant objects

## Usage

``` r
# S4 method for class 'formula,FLQuant'
wireframe(x, data, ...)
```

## Arguments

- x:

  a `formula` formula for lattice

- data:

  a `FLQuant` object with the values

- ...:

  Additional argument list to be passed to `wireframe`

## Value

a `wireframe` plot

## Details

Method to plot 3D representations of FLQuant objects

## Examples

``` r
data(ple4)
wireframe(data~age+year, data=harvest(ple4))

```
