\name{metier}
\docType{methods}
\alias{metier}
\alias{metier-methods}
\alias{metier,FLFleet-method}
\alias{metier,FLFleet,ANY-method}
\alias{metier<-}
\alias{metier<--methods}
\alias{metier<-,FLFleet,ANY,FLMetier-method}
\title{Method metier}
\description{
  \code{metier} returns a \code{linkS4class{FLMetier}} object from a
  \code{linkS4class{FLFleet}}. They can also be accessed directly by using the
  '[' and '[[' methods, see example below.
}
\section{Generic function}{
 metier(object,metier)
}
\section{Methods}{\describe{
\item{ signature(object=FLFleet, metier=character) :}{Returns the FLMetier with the given name.}
}}
\author{The FLR Team}
\seealso{\link{FLMetier}, \link{FLFleet}, \link{FLCatch}, \link{Extract-FLCore}}
\examples{
data(bt4)
names(bt4@metiers)

# calling the FLMetier with the metier method
met <- metier(bt4,metier='TBB')
met <- metier(bt4,'TBB')

# calling the lower levels with the '[' and the '[[' levels
fl1  <- bt4['TBB']      # returns a fleet with only the 'TBB' metier
met1 <- bt4[['TBB']]    # returns the metier

fl2  <- bt4['TBB','ple'] # returns a fleet with only the 'ple' catch for 'TBB' metier
fl3  <- bt4[,'ple']      # returns a fleet with only the 'ple' catch

# possible combinations of the two include:

ca1 <-  bt4[['TBB']][['ple']] # returns the 'ple' FLCatch from metier 'TBB'
met2 <- bt4[['TBB']]['ple']   # returns the 'TBB' metier with only the 'ple' catches

}
\keyword{methods}

