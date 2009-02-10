\name{catchNames}
\docType{methods}
\alias{catchNames}
\alias{catchNames-methods}
\alias{catchNames,FLCatch-method}
\alias{catchNames,FLCatches-method}
\alias{catchNames,FLMetier-method}
\alias{catchNames,FLMetiers-method}
\alias{catchNames,FLFleet-method}
\alias{catchNames,FLFleets-method}
\title{Method catchNames}
\description{
  Returns an unique list of the names of the FLCatch objects present in the inout object.
}
\section{Generic function}{
 catchNames(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLCatch) :}{Get the name of an FLCatch}
\item{ signature(object=FLCatches) :}{Get the unique names of the FLCatch elements}
\item{ signature(object=FLMetier) :}{Get the unique names of the FLCatch elements in catches}
\item{ signature(object=FLMetiers) :}{Get the unique names of the FLCatch elements in the different metiers}
\item{ signature(object=FLFleet) :}{Get the unique names of the FLCatch elements in the object}
\item{ signature(object=FLFleet) :}{Get the unique names of the FLCatch objects across all
elements in the object}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\keyword{methods}
