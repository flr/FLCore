# Method trim

Trim FLR objects using named dimensions

## Usage

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

## Details

Subsetting of FLR objects can be carried out with dimension names by
using `trim`. A number of dimension names and selected dimensions are
passed to the method and those are used to subset the input object.

Exceptions are made for those classes where certain slots might differ
in one or more dimensions. If trim is applied to an FLQuant object of
length 1 in its first dimension and with dimension name equal to 'all',
values to `trim` specified for that dimension will be ignored. For
example, [`FLStock`](FLStock.md) objects contain slots with length=1 in
their first dimension. Specifying values to trim over the first
dimension will have no effect on those slots (`catch`, `landings`,
`discards`, and `stock`). Calculations might need to be carried out to
recalculate those slots (e.g. using `computeCatch`, `computeLandings`,
`computeDiscards` and `computeStock`) if their quant-structured
counterparts are modified along the first dimension.

## Generic function

trim(x)

## See also

[FLQuant](FLQuant.md), [FLStock](FLStock.md), [FLCohort](FLCohort.md),
[FLIndex](FLIndex.md)

## Author

The FLR Team

## Examples

``` r
flq <- FLQuant(rnorm(90), dimnames=list(age=1:10, year=2000:2016))

trim(flq, year=2000:2005)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2000     2001     2002     2003     2004     2005    
#>   1   0.57984  0.73513 -0.55415  2.95564  0.50004 -0.49678
#>   2  -2.61083 -2.01938 -0.56800 -0.83394  2.11979 -0.51532
#>   3  -0.75112  1.07972 -0.05714 -0.31975  0.85063  0.11481
#>   4   0.46673 -0.73354 -0.49607 -1.00482  0.00821  2.46060
#>   5  -1.28764  1.29598 -0.45553 -0.43139  1.09732 -0.23906
#>   6  -0.79524  0.23105  1.84572  1.09850  2.69262 -0.82407
#>   7   1.18700  1.66789 -0.30876 -0.10920 -0.21684  0.45755
#>   8  -0.88802  1.06868  0.98081  0.16996  0.85695 -2.07220
#>   9   1.73272 -0.85710  0.09380 -1.81962 -0.22610  0.57519
#>   10  0.02773  2.57158 -0.47901 -2.06922  0.12027 -0.79215
#> 
#> units:  NA 
# which is equivalent to
window(flq, start=2000, end=2005)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2000     2001     2002     2003     2004     2005    
#>   1   0.57984  0.73513 -0.55415  2.95564  0.50004 -0.49678
#>   2  -2.61083 -2.01938 -0.56800 -0.83394  2.11979 -0.51532
#>   3  -0.75112  1.07972 -0.05714 -0.31975  0.85063  0.11481
#>   4   0.46673 -0.73354 -0.49607 -1.00482  0.00821  2.46060
#>   5  -1.28764  1.29598 -0.45553 -0.43139  1.09732 -0.23906
#>   6  -0.79524  0.23105  1.84572  1.09850  2.69262 -0.82407
#>   7   1.18700  1.66789 -0.30876 -0.10920 -0.21684  0.45755
#>   8  -0.88802  1.06868  0.98081  0.16996  0.85695 -2.07220
#>   9   1.73272 -0.85710  0.09380 -1.81962 -0.22610  0.57519
#>   10  0.02773  2.57158 -0.47901 -2.06922  0.12027 -0.79215
#> 
#> units:  NA 

trim(flq, year=2000:2005, age=1:2)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2000   2001   2002   2003   2004   2005  
#>   1  0.580  0.735 -0.554  2.956  0.500 -0.497
#>   2 -2.611 -2.019 -0.568 -0.834  2.120 -0.515
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
