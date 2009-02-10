%$Id$
\name{setPlusGroup}
\docType{methods}
\alias{setPlusGroup}
\alias{setPlusGroup-methods}
\alias{setPlusGroup,FLQuant,numeric-method}
\alias{setPlusGroup,FLStock,numeric-method}
\alias{setPlusGroup,FLBiol,numeric-method}
\alias{setPlusGroup,FLCatch,numeric-method}
\title{Method setPlusGroup}
\description{
 Calculates the appropriate values for the plusgroup of an object
 and returns a new object with the plusgroup set to the given age.

 \emph{quant} of the given object must be 'age', and the selected age must not be
 greater than the oldest age present in the object.
}
\section{Generic function}{
 sePlusGroup(x, plusgroup) 
}
\section{Methods}{\describe{
\item{ signature(x=FLQuant, plusgroup=numeric) :}{Adds values for the indicated age and older.}
\item{ signature(x=FLStock, plusgroup=numeric) :}{
 The values for the plusgroup of the various slots in the \code{FLStock} object
 are calculated in different ways.
 
 For slots catch.n, landings.n, discards.n and stock.n the plusgroup is
 calculated as the sum of values for ages equal to the plusgroup and above.
 
 For slots catch.wt, landings.wt, discards.wt and stock.wt the plusgroup value
 is calculated as the weighted average of the values for ages equal to the
 plusgroup and above, weighted by the corresponding numbers at age.
 
 If stock numbers at age are not available then the revised stock weights are
 calculated as a weighted average using the catch numbers at age and the slots
 for harvest, m, mat, harvest.spwn and m.spwn are truncated at the plusgroup age.
 
 If stock numbers at age are available the plusgroup values for harvest, m, mat,
 harvest.spwn and m.spwn are calculated as a weighted average using the stock
 numbers at age.
}
\item{ signature(x=FLBiol, plusgroup=numeric) :}{Calculations are similar to FLStock above.}
\item{ signature(x=FLCatch, plusgroup=numeric) :}{Abundances (\emph{n} slot) are added, while weights (\emph{wt}), natural mortality (\emph{m}), and fecundity (\emph{fec}) are
averaged.}
}}
\author{The FLR Team}
\seealso{ \link{FLStock-class}, \link{FLQuant-class}, \link{FLBiol-class}, \link{FLCatch-class} }
\examples{
data(ple4)
ple4.pg <- setPlusGroup(ple4, 6)
}
\keyword{methods}
