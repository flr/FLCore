\name{r}
\alias{r}
\alias{r-methods}
\alias{r,FLBiol-method}
\title{Intrinsic rate of increase from an FLBiol object}
\description{
	For an \code{FLBiol} object with the mortality-at-age, fecundity and spwn data 
	present in the object slots, this method calculates the intrinsic rate of increase r 
  for the given population.

	It does this using two methods:

	(1) Solving the Euler-Lotka equation.

	(2) Calculating the logarithm of the real part of the largest/lead eigenvalue 
	of the Leslie transition matrix.

	These two methods are not identical but do give similar answers for the same data 
	and parameters
}
\usage{
r(object, ...)
}
\arguments{
  \item{object}{ An object of type \code{\link{FLBiol-class}}.}
  \item{\dots}{ Extra arguments accepted by each implementation.}
}
\details{
To chose the method used to estimate r (Euler-Lotka or Leslie matrix) we supply either 
'el' or 'leslie' as the 'method' argument (see below). To calculate r along years or cohorts
by supply either 'year' or 'cohort' as the 'by' argument (see below).

The method can handle Monte Carlo samples (i.e. with iterations) in either the fec, m or both slots required to calculate r and the conversion is done internally so that we obtain an \code{FLQuant} of the correct dimensions.
}
\value{
	An object of class \code{\link{FLQuant-class}}.
}
\author{FLR Team}
\seealso{\code{\link{FLBiol-class}}}
\examples{\dontrun{

# call in the NS herring stock and biol objects

data(nsher.biol)
data(nsher)

# calculate the gradient at the origin from the spawning stock numbers to the recruits

tmp <- nsher.biol
n(tmp) <- stock.n(nsher)
m(tmp) <- harvest(nsher)+m(nsher.biol)

# use nls to calculate gradient of (recruits/spawning stock numbers)
# assuming log-normal errors

dfx <- data.frame(rec=as.vector(n(tmp)[1,]),ssn=as.vector(ssn(tmp)))
res <- nls(log(rec)~log(a)+log(ssn),dfx,start=list(a=mean(n(tmp)[1,]/ssn(tmp))))

# use this value of recruits per spawner times maturity as the birth function for 
# the Leslie transition matrix/Euler-Lotka equation

alpha <- coef(res)[[1]]
fec(tmp)[] <- fec(tmp)[] * alpha

# calculate r assuming only natural mortality and by year
# using both Euler-Lotka (el) and Lesie matrix (leslie) method

m(tmp) <- m(nsh.biol)
r.nsh.el <- r(tmp,by='year',method='el')
r.nsh.lm <- r(tmp,by='year',method='leslie')
}
}
\keyword{methods}
