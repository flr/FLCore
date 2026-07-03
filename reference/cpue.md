# Compute a CPUE index of abundance from an FLStock

The observation of stock abundance by CPUE series from commercial fleets
is an important step in the generation of management advice that needs
to be replicated on an Operating Model during any simulation exercise.
This method generates an observation of biomass or numbers-at-age from
an `FLStock` being used as OM.

## Usage

``` r
cpue(object, index, ...)

# S4 method for class 'FLStock,missing'
cpue(
  object,
  sel.pattern = harvest(object),
  effort = units(harvest(object)),
  biomass = TRUE
)
```

## Arguments

- object:

  An `FLStock` from which to generate the CPUE observation.

- index:

  An optional index object (currently unused for the `FLStock, missing`
  method).

- ...:

  Additional arguments passed to methods.

- sel.pattern:

  Selectivity pattern to apply; defaults to `harvest(object)`.

- effort:

  Units of effort used to mimic the effort series in the fishery: `"f"`
  (default, fishing mortality), `"hr"` (harvest rate), or a numeric
  value.

- biomass:

  Logical; if `TRUE` (default) the index is summed across ages and
  weighted by catch weight-at-age to give a biomass CPUE.

## Value

An `FLQuant` containing the CPUE index: age-disaggregated if
`biomass = FALSE`, or aggregated biomass CPUE if `biomass = TRUE`.

## Generic function

cpue(object, index, ...)

## See also

[survey](survey.md), [FLStock](FLStock.md)

## Author

The FLR Team

## Examples

``` r

data(ple4)

cpue(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all  78051  83671  94547 105473 107196 114773 131736 135215 136997 160535
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 149615 142493 138212 127121 132826 141951 140378 140683 152634 170196
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 158271 167408 160092 175025 179501 184034 198791 215973 229085 242239
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 274837 303250 276689 238235 205251 181311 173349 146356 122400 120744
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 141943 153857 164072 142703 118790 134722 148192 143316 110487 114377
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 106431 118495 120210 121111 115815 128030 135992 133361 129688 121409
#>      year
#> age   2017  
#>   all 108121
#> 
#> units:  t 
# An aggregated biomass CPUE
quantSums(cpue(ple4))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all  78051  83671  94547 105473 107196 114773 131736 135215 136997 160535
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 149615 142493 138212 127121 132826 141951 140378 140683 152634 170196
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 158271 167408 160092 175025 179501 184034 198791 215973 229085 242239
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 274837 303250 276689 238235 205251 181311 173349 146356 122400 120744
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 141943 153857 164072 142703 118790 134722 148192 143316 110487 114377
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 106431 118495 120210 121111 115815 128030 135992 133361 129688 121409
#>      year
#> age   2017  
#>   all 108121
#> 
#> units:  t 

if (FALSE) { # \dontrun{
plot(FLQuants(om=stock(ple4), cpue=quantSums(cpue(ple4)),
  hr=quantSums(cpue(ple4, effort="hr"))))
} # }
```
