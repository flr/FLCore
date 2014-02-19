%$Id: FLQuantTotals.Rd 11 2009-01-26 15:00:07Z imosqueira $
\name{FLQuantTotals}
% quant
\docType{methods}
\alias{quantTotals}
\alias{quantTotals-methods}
\alias{quantTotals,FLQuant-method}
% year
\alias{yearTotals}
\alias{yearTotals-methods}
\alias{yearTotals,FLQuant-method}
\title{Method quantTotals}
\description{

	These methods return an object of same dimensions as the input but with the sums along
	the first (\code{yearTotals}) or second dimension (\code{quantTotals}). Although the
	names might appear contradictory, it must be noted that what each method really returns
	are the totals over the selected dimension.
	
}
\section{Generic function}{
 quantTotals(x) 

 yearTotals(x) 
}
\section{Methods}{\describe{
\item{ signature(x=FLQuant) :}{Compute totals for an FLQuant dimension and replicate to
	the whole object}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
 flq <- FLQuant(rlnorm(100), dim=c(10,10))
 quantTotals(flq)
 # See how the values obtained by yearSums are being replicated
 yearSums(flq)
 # Get the proportions by quant
 flq / quantTotals(flq)
 # or year
 flq / yearTotals(flq)
 }
\keyword{methods}
