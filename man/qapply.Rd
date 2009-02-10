\name{qapply}
\docType{methods}
\alias{qapply}
\alias{qapply-methods}
\alias{qapply,FLComp,function-method}
\title{Method qapply}
\description{
 Returns a \code{\link[base]{list}} or \code{\link{FLlst}} containing values obtained by 
 applying a function to margins for each FLQuant in a composite FLR object.
}
\section{Generic function}{
 qapply(X,FUN) 
}
\section{Methods}{\describe{
\item{ signature(X=FLComp,FUN=function) :}{

 \code{FUN} is typically a function name to be searched for from the environment
 of the call to \code{qapply}. Additional arguments to the function are specified
 after the function name.
 
 \code{qapply} enables functions to be applied easily to all \code{FLQuants} of
 a composite object rather than repeating the code for each one separately.
 In the example below the \code{apply} function is nested inside \code{qapply}
 to calculate averages across various dimensions for each \code{FLQuant} in an
 \code{FLStock} object.
 
}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} \link{apply} }
\examples{

data(ple4)

# returns a list containing the max value for each quant
qapply(ple4, max)

# returns a FLStock of means across all dimensions except year
qapply(ple4, apply, 2, mean, na.rm=TRUE) 

# returns an FLStock of max values across all dimensions except year and age
qapply(ple4, apply, c(1,2), max)

}
\keyword{methods}
