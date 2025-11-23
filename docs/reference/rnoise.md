<div id="main" class="col-md-9" role="main">

# Random noise with different frequencies

<div class="ref-description section level2">

A noise generator

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'numeric,FLQuant'
rnoise(
  n = n,
  len = len,
  sd = 1,
  b = 0,
  burn = 0,
  trunc = 0,
  what = c("year", "cohort", "age"),
  seed = NA
)

# S4 method for class 'numeric,missing'
rnoise(n = n, sd = 1, b = 0, burn = 0, trunc = 0, seed = NA)

# S4 method for class 'numeric,FLQuant'
rlnoise(
  n = n,
  len = len,
  sd = 1,
  b = 0,
  burn = 0,
  trunc = 0,
  what = c("year", "cohort", "age"),
  seed = NA
)
```

</div>

</div>

<div class="section level2">

## Arguments

-   n:

    number of iterations

-   len:

    an `FLQuant`

-   sd:

    standard error for simulated series

-   b:

    autocorrelation parameter a real number in 0,1

-   burn:

    gets rid of 1st values i series

-   trunc:

    get rid of values \> abs(trunc)

-   what:

    returns time series for year, cohort or age"

-   ...:

    any

</div>

<div class="section level2">

## Value

A `FLQuant` with autocorrelation equal to B.

</div>

<div class="section level2">

## References

Ranta and Kaitala 2001 Proc. R. Soc. vt = b \* vt-1 + s \* sqrt(1 - b^2)
s is a normally distributed random variable with mean = 0 b is the
autocorrelation parameter

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
if (FALSE) { # \dontrun{
flq <- FLQuant(1:100, quant="age")
white <- rnoise(100,flq,sd=.3,b=0)
plot(white)
acf(white)

red <- rnoise(100,flq,sd=.3,b=0.7)
plot(red)
acf(red)

res <- rnoise(100,flq,sd=.3,b=0)

ggplot() +
  geom_point(aes(year,age,size=data),
    data=subset(as.data.frame(res), data>0)) +
geom_point(aes(year,age,size=-data),
            data=subset(as.data.frame(res),data<=0),colour="red")+
scale_size_area(max_size=4, guide="none")+
facet_wrap(~iter)

data(ple4)
res <- rnoise(4,m(ple4),burn=10,b=0.9,what="cohort")
ggplot()+
geom_point(aes(year,age,size= data),
          data=subset(as.data.frame(res),data>0))+
geom_point(aes(year,age,size=-data),
          data=subset(as.data.frame(res),data<=0),colour="red")+
scale_size_area(max_size=4, guide="none")+
facet_wrap(~iter)

} # }
```

</div>

</div>

</div>
