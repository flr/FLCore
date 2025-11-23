<div id="main" class="col-md-9" role="main">

# Methods quantTotals

<div class="ref-description section level2">

Methods to compute totals over selected dimensions of `FLQuant` objects
These methods return an object of same dimensions as the input but with
the sums along the first (`yearTotals`) or second dimension
(`quantTotals`). Although the names might appear contradictory, it must
be noted that what each method really returns are the totals over the
selected dimension.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
quantTotals(x, ...)
```

</div>

</div>

<div class="section level2">

## Generic function

quantTotals(x)

yearTotals(x)

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
flq <- FLQuant(rlnorm(100), dim=c(10,10))
quantTotals(flq)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1     2     3     4     5     6     7     8     9     10   
#>    1  11.98 11.98 11.98 11.98 11.98 11.98 11.98 11.98 11.98 11.98
#>    2  12.97 12.97 12.97 12.97 12.97 12.97 12.97 12.97 12.97 12.97
#>    3  39.92 39.92 39.92 39.92 39.92 39.92 39.92 39.92 39.92 39.92
#>    4  22.06 22.06 22.06 22.06 22.06 22.06 22.06 22.06 22.06 22.06
#>    5  11.58 11.58 11.58 11.58 11.58 11.58 11.58 11.58 11.58 11.58
#>    6   9.41  9.41  9.41  9.41  9.41  9.41  9.41  9.41  9.41  9.41
#>    7  15.42 15.42 15.42 15.42 15.42 15.42 15.42 15.42 15.42 15.42
#>    8   6.63  6.63  6.63  6.63  6.63  6.63  6.63  6.63  6.63  6.63
#>    9  32.75 32.75 32.75 32.75 32.75 32.75 32.75 32.75 32.75 32.75
#>    10 16.56 16.56 16.56 16.56 16.56 16.56 16.56 16.56 16.56 16.56
#> 
#> units:  NA 
# See how the values obtained by yearSums are being replicated
  yearSums(flq)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1    
#>    1  11.98
#>    2  12.97
#>    3  39.92
#>    4  22.06
#>    5  11.58
#>    6   9.41
#>    7  15.42
#>    8   6.63
#>    9  32.75
#>    10 16.56
#> 
#> units:  NA 
# Get the proportions by quant
  flq / quantTotals(flq)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1       2       3       4       5       6       7       8       9      
#>    1  0.09739 0.01029 0.12441 0.16063 0.05655 0.06699 0.07446 0.21534 0.04338
#>    2  0.06810 0.29197 0.26045 0.07365 0.03535 0.06776 0.01430 0.03878 0.07584
#>    3  0.03891 0.05579 0.11961 0.40037 0.05840 0.12698 0.02107 0.13707 0.03716
#>    4  0.00731 0.04679 0.47869 0.06569 0.05185 0.04560 0.02877 0.21669 0.03252
#>    5  0.13746 0.10295 0.25120 0.07666 0.03178 0.04004 0.06309 0.06158 0.12078
#>    6  0.06371 0.04338 0.00586 0.13983 0.12914 0.08002 0.02482 0.35520 0.12698
#>    7  0.03000 0.01617 0.02166 0.02823 0.07716 0.07085 0.20270 0.07825 0.15733
#>    8  0.21006 0.05792 0.06288 0.07716 0.21164 0.11273 0.02976 0.12625 0.06517
#>    9  0.01436 0.02872 0.01919 0.09557 0.00455 0.09192 0.02706 0.64857 0.01799
#>    10 0.00943 0.07178 0.04279 0.45398 0.09618 0.00414 0.08782 0.15962 0.06734
#>      year
#> quant 10     
#>    1  0.15057
#>    2  0.07381
#>    3  0.00464
#>    4  0.02610
#>    5  0.11446
#>    6  0.03106
#>    7  0.31766
#>    8  0.04642
#>    9  0.05207
#>    10 0.00692
#> 
#> units:  NA 
# or year
  flq / yearTotals(flq)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1       2       3       4       5       6       7       8       9      
#>    1  0.13828 0.01069 0.05901 0.05641 0.06434 0.05776 0.09716 0.05955 0.04786
#>    2  0.10466 0.32830 0.13372 0.02800 0.04354 0.06324 0.02019 0.01161 0.09056
#>    3  0.18412 0.19318 0.18910 0.46863 0.22146 0.36495 0.09163 0.12634 0.13662
#>    4  0.01911 0.08950 0.41812 0.04248 0.10863 0.07240 0.06913 0.11035 0.06605
#>    5  0.18870 0.10339 0.11520 0.02603 0.03496 0.03338 0.07960 0.01646 0.12881
#>    6  0.07103 0.03539 0.00218 0.03856 0.11539 0.05419 0.02543 0.07714 0.11000
#>    7  0.05481 0.02161 0.01322 0.01276 0.11299 0.07863 0.34042 0.02785 0.22337
#>    8  0.16504 0.03330 0.01651 0.01500 0.13326 0.05379 0.02149 0.01932 0.03978
#>    9  0.05574 0.08157 0.02488 0.09176 0.01416 0.21671 0.09653 0.49036 0.05427
#>    10 0.01850 0.10307 0.02805 0.22038 0.15127 0.00494 0.15841 0.06102 0.10268
#>      year
#> quant 10     
#>    1  0.14827
#>    2  0.07867
#>    3  0.01522
#>    4  0.04733
#>    5  0.10897
#>    6  0.02402
#>    7  0.40260
#>    8  0.02530
#>    9  0.14019
#>    10 0.00943
#> 
#> units:  NA 
```

</div>

</div>

</div>
