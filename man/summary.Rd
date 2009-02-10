\name{summary}
\docType{methods}
\alias{summary,FLQuant-method}
\alias{summary,FLQuantPoint-method}
\alias{summary,FLComp-method}
\alias{summary,FLQuants-method}
\alias{summary,FLPar-method}
\alias{summary,FLMetier-method}
\alias{summary,FLFleet-method}
\alias{summary,FLModel-method}
\alias{summary,FLArray-method}
\alias{summary,FLlst-method}
\title{Method summary}
\description{
  Outputs a general summary of the structure and content of the object. The particular
  output obtined depends on the class of the argument object.
}
\section{Generic function}{
 summary(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLQuant) :}{Returns dimensions, quant, units and distribution of data, including percentage of NAs}
\item{ signature(object=FLQuantPoint) :}{Returns dimensions, quant, units and distribution of data}
\item{ signature(object=FLComp) :}{Outputs name, desc, quant and dimensions of each slot}
\item{ signature(object=FLQuants) :}{Returns dimensions, quant, units and distribution of data, including percentage of NAs for each element in the list}
\item{ signature(object=FLPar) :}{Returns the values stored, or tehir basic statistics}
\item{ signature(object=FLFleet) :}{Outputs name, desc, quant and dimensions of each slot}
\item{ signature(object=FLModel) :}{Returns the name, desc and content of model-related slots}
}}
\author{The FLR Team}
\seealso{\link[base]{summary}}
\examples{
summary(ple4)

summary(nsher)
}
\keyword{methods}
