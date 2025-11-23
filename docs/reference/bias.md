<div id="main" class="col-md-9" role="main">

# Bias of estimates through jackknife

<div class="ref-description section level2">

Description: Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Pellentesque eleifend odio ac rutrum luctus. Aenean placerat porttitor
commodo. Pellentesque eget porta libero. Pellentesque molestie mi sed
orci feugiat, non mollis enim tristique.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLQuantJK'
bias(x)

# S4 method for class 'FLParJK'
bias(x)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    An object holding estimates obtained through jackknife

</div>

<div class="section level2">

## Value

A value for the mean bias

</div>

<div class="section level2">

## Details

Details: Aliquam sagittis feugiat felis eget consequat. Praesent
eleifend dolor massa, vitae faucibus justo lacinia a. Cras sed erat et
magna pharetra bibendum quis in mi. Sed sodales mollis arcu, sit amet
venenatis lorem fringilla vel. Vivamus vitae ipsum sem. Donec malesuada
purus at libero bibendum accumsan. Donec ipsum sapien, feugiat blandit
arcu in, dapibus dictum felis.

$$\\widehat{Bias}\_{(\\theta)} = (n -
1)((\\frac{1}{n}\\sum\\limits\_{i=1}^n\\hat{\\theta}\_{(i)})-\\hat{\\theta})$$

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLComp](FLComp.md)

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
flq <- FLQuant(1:8)
flj <- jackknife(flq)
bias(flj)
#> An object of class "FLQuant"
#> iters:  8 
#> 
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1       2       3       4       5       6       7       8      
#>   all 1.02(0) 1.04(0) 1.06(0) 1.08(0) 1.10(0) 1.13(0) 1.15(0) 1.17(0)
#> 
#> units:  NA 
```

</div>

</div>

</div>
