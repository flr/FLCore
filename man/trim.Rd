\name{trim}
\docType{methods}
\alias{trim}
\alias{trim-methods}
\alias{trim,FLArray-method}
\alias{trim,FLQuant-method}
\alias{trim,FLComp-method}
\alias{trim,FLCatch-method}
\alias{trim,FLMetier-method}
\alias{trim,FLFleet-method}
\alias{trim,FLStock-method}
\alias{trim,FLCohort-method}
\alias{trim,FLIndex-method}
\title{Trim FLR objects using named dimensions}
\description{
  Subsetting of FLR objects can be carried out using the dimension names by using 
  \code{trim}. A number of dimension names and selected dimensions are passed to the
  method and those are used to subset the input object.

  Exceptions are made for those classes where certain slots might differ in one or
  more dimensions. If trim is applied on an FLQuant object of length 1 in its first
  dimension and with dimension name equal to 'all', values to trim on specified for 
  that dimension will be ignored. For example, \code{\link{FLStock}} objects
  contain slots with length=1 on their first dimension. Specifying values to trim along
  over the first dimension will have no effect on those slots (\code{catch},  
  \code{landings}, \code{discards}, and \code{stock}). Calculations might need to be 
  carried out to recalculate those slots if their quant-structured counterparts are 
  modified along the first dimension.
}
\section{Generic function}{
 trim(x) 
}
\section{Methods}{\describe{
\item{ signature(x=FLQuant) :}{Trims along the specified dimensions.}
\item{ signature(x=FLComp) :}{Trims along the specified dimensions.}
\item{ signature(x=FLStock) :}{Trims along the specified dimensions, but ignores the \emph{quant} (first) dimension for those slots where it is always of length=1.}
\item{ signature(x=FLCohort) :}{Trims along the specified dimensions.}
\item{ signature(x=FLIndex) :}{Trims along the specified dimensions, but ignores the \emph{quant} (first) dimension for thoe slots where it is of length=1.}
}}
\author{The FLR Team}
\seealso{ \link{FLQuant-class},  \link{FLStock-class}, \link{FLCohort-class}, \link{FLIndex-class}}
\examples{
data(ple4)

# This is equivalent to window(catch(ple4), start=1990, end=1995)
trim(catch(ple4), year=1990:1995)

trim(catch.n(ple4), year=1990:1995, age=1:2)

# Now on an FLStock
summary(trim(ple4), year=1990:1995)

# If 'age' is trimmed in ple4, catch, landings and discards need to be recalculated
shpl4 <- trim(ple4, age=1:4)
landings(shpl4) <- computeLandings(shpl4)
discards(shpl4) <- computeDiscards(shpl4)
catch(shpl4) <- computeCatch(shpl4)

summary(shpl4)
}
\keyword{methods}
