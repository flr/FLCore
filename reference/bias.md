# Bias of estimates through jackknife

Description: Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Pellentesque eleifend odio ac rutrum luctus. Aenean placerat porttitor
commodo. Pellentesque eget porta libero. Pellentesque molestie mi sed
orci feugiat, non mollis enim tristique.

## Usage

``` r
# S4 method for class 'FLQuantJK'
bias(x)

# S4 method for class 'FLParJK'
bias(x)
```

## Arguments

- x:

  An object holding estimates obtained through jackknife

## Value

A value for the mean bias

## Details

Details: Aliquam sagittis feugiat felis eget consequat. Praesent
eleifend dolor massa, vitae faucibus justo lacinia a. Cras sed erat et
magna pharetra bibendum quis in mi. Sed sodales mollis arcu, sit amet
venenatis lorem fringilla vel. Vivamus vitae ipsum sem. Donec malesuada
purus at libero bibendum accumsan. Donec ipsum sapien, feugiat blandit
arcu in, dapibus dictum felis.

\$\$\widehat{Bias}\_{(\theta)} = (n -
1)((\frac{1}{n}\sum\limits\_{i=1}^n\hat{\theta}\_{(i)})-\hat{\theta})\$\$

## See also

[FLComp](FLComp.md)

## Author

The FLR Team

## Examples

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
