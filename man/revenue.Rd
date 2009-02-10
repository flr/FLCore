\name{revenue}
\docType{methods}
\alias{revenue}
\alias{revenue-methods}
\alias{revenue,FLCatch-method}
\alias{revenue,FLMetier-method}
\alias{revenue,FLCatches-method}
\alias{revenue,FLMetiers-method}
\alias{revenue,FLFleet-method}
\title{Method revenue}
\description{
  Calculates the revenue from elements of an FLFleet object as the sum of the landings 
  weight-at-quant times its price by quant.
}
\section{Generic function}{
 revenue(object)
}
\section{Methods}{\describe{
\item{ signature(object=FLCatch) :}{Returns an FLQuant object containing the revenue of a single catch}
\item{ signature(object=FLMetier) :}{Returns an FLQuants object containing the revenues of the catches in the metier}
}}
\author{The FLR Team}
\seealso{ \link{FLFleet} }
\examples{
    # load FLFleet dataset
    data(bt4)
    # Calculate revenue of individual catches
    sol.rev <- revenue(catches(metier(bt4,"TBB"),"sol"))
    ple.rev <- revenue(catches(metier(bt4,"TBB"),"ple"))
    # Calculate revenue of the catches in the metier
    TBB.rev <- revenue(metier(bt4,"TBB"))

    \dontrun{
    plot(sol.rev, type='b', ylab='revenues (euros)', xlab='')
    }
}
\keyword{methods}
