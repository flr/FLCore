<div id="main" class="col-md-9" role="main">

# Method trim

<div class="ref-description section level2">

Trim FLR objects using named dimensions

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
trim(x, ...)

# S4 method for class 'FLArray'
trim(x, ...)

# S4 method for class 'FLComp'
trim(x, ...)

# S4 method for class 'FLS'
trim(x, ...)

# S4 method for class 'FLBiol'
trim(x, ...)
```

</div>

</div>

<div class="section level2">

## Details

Subsetting of FLR objects can be carried out with dimension names by
using `trim`. A number of dimension names and selected dimensions are
passed to the method and those are used to subset the input object.

Exceptions are made for those classes where certain slots might differ
in one or more dimensions. If trim is applied to an FLQuant object of
length 1 in its first dimension and with dimension name equal to 'all',
values to `trim` specified for that dimension will be ignored. For
example, `FLStock` objects contain slots with length=1 in their first
dimension. Specifying values to trim over the first dimension will have
no effect on those slots (`catch`, `landings`, `discards`, and `stock`).
Calculations might need to be carried out to recalculate those slots
(e.g. using `computeCatch`, `computeLandings`, `computeDiscards` and
`computeStock`) if their quant-structured counterparts are modified
along the first dimension.

</div>

<div class="section level2">

## Generic function

trim(x)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLQuant](FLQuant.md), [FLStock](FLStock.md), [FLCohort](FLCohort.md),
[FLIndex](FLIndex.md)

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
flq <- FLQuant(rnorm(90), dimnames=list(age=1:10, year=2000:2016))

trim(flq, year=2000:2005)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2000     2001     2002     2003     2004     2005    
#>   1  -0.90630 -0.75078 -0.02434 -0.81966 -1.83948  0.22713
#>   2  -0.50448  0.93232 -0.37410 -0.11215  0.64431 -0.10455
#>   3  -0.97830  0.15426  0.56761 -0.51602  0.65159 -0.60208
#>   4  -2.46514  0.41024  0.29108  0.18560  1.57666 -0.68689
#>   5   1.23671  0.02537  0.51855 -1.34503 -0.08645  1.00806
#>   6  -0.31821  0.20773 -1.18328 -1.57599  0.15917 -1.23874
#>   7  -0.21825  0.34671 -0.79477  1.53897 -1.88600  0.43119
#>   8   0.25617  0.06281 -0.02997 -0.44535 -0.16137  1.15948
#>   9   0.41019  1.81880 -1.06670 -0.12699 -1.52739  0.00236
#>   10 -0.80914  0.97920 -2.03692  0.11260  0.05107 -1.23579
#> 
#> units:  NA 
# which is equivalent to
window(flq, start=2000, end=2005)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2000     2001     2002     2003     2004     2005    
#>   1  -0.90630 -0.75078 -0.02434 -0.81966 -1.83948  0.22713
#>   2  -0.50448  0.93232 -0.37410 -0.11215  0.64431 -0.10455
#>   3  -0.97830  0.15426  0.56761 -0.51602  0.65159 -0.60208
#>   4  -2.46514  0.41024  0.29108  0.18560  1.57666 -0.68689
#>   5   1.23671  0.02537  0.51855 -1.34503 -0.08645  1.00806
#>   6  -0.31821  0.20773 -1.18328 -1.57599  0.15917 -1.23874
#>   7  -0.21825  0.34671 -0.79477  1.53897 -1.88600  0.43119
#>   8   0.25617  0.06281 -0.02997 -0.44535 -0.16137  1.15948
#>   9   0.41019  1.81880 -1.06670 -0.12699 -1.52739  0.00236
#>   10 -0.80914  0.97920 -2.03692  0.11260  0.05107 -1.23579
#> 
#> units:  NA 

trim(flq, year=2000:2005, age=1:2)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2000    2001    2002    2003    2004    2005   
#>   1 -0.9063 -0.7508 -0.0243 -0.8197 -1.8395  0.2271
#>   2 -0.5045  0.9323 -0.3741 -0.1122  0.6443 -0.1045
#> 
#> units:  NA 


# Now on an FLStock
data(ple4)
summary(trim(ple4, year=1990:1995))
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  6   1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   10  10  1990    1995    2   6   
#> 
#> Metrics: 
#>   rec: 550376 - 1083810  (1000) 
#>   ssb: 222203 - 396458  (t) 
#>   catch: 132629 - 250604  (t) 
#>   fbar: 0.60 - 0.61  (f) 

# If 'age' is trimmed in ple4, catch, landings and discards need to be
# recalculated
  shpl4 <- trim(ple4, age=1:4)
  landings(shpl4) <- computeLandings(shpl4)
  discards(shpl4) <- computeDiscards(shpl4)
  catch(shpl4) <- computeCatch(shpl4)
  summary(shpl4)
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  4   61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   4   NA  1957    2017    2   4   
#> 
#> Metrics: 
#>   rec: 367450 - 4303680  (1000) 
#>   ssb: 91016 - 316967  (t) 
#>   catch: 36219 - 253152  (t) 
#>   fbar: 0.22 - 0.71  (f) 
```

</div>

</div>

</div>
