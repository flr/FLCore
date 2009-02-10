\name{mergeFL}
\docType{methods}
\alias{mergeFLStock}
\alias{+,FLStock,FLStock-method}
\title{Merging FLStock objects}
\description{
Two FLStock object can be \emph{merged} using this method or a plus sign. Catch slots are added, and weight slots are averaged, weighted by the relative catches. No meaningful calculation is currently done for harvest, harvest.spwn, m, and m.spwn.
}
\section{Methods}{\describe{
\item{ signature(e1=FLStock, e2=FLStock) :}{Adds two FLStock objects}
}}
\author{The FLR Team}
\seealso{ \link{FLStock} }
\keyword{methods}
