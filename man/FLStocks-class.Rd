\name{FLStocks}
\docType{class}
\alias{FLStocks-class}
\alias{FLStocks}
\alias{FLStocks-methods}
\alias{FLStocks,ANY-method}
 \alias{FLStocks,missing-method}
 \alias{FLStocks,list-method}
\title{Class FLStocks}
\description{
\code{FLStocks} is a class that extends \code{list} through \code{FLlst} but implements a set of features that give a little bit more structure to list objects. The elements of \code{FLStocks} must all be of class \code{FLStock}. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. 
}
\section{Slots}{\describe{
\item{.Data}{The data. \code{list}.}
\item{names}{Names of the list elements. \code{character}.}
\item{desc}{Description of the object. \code{character}.}
\item{lock}{Lock mechanism, if turned on the length of the list can not be modified by adding or removing elements. \code{logical}.}
}}
\section{Extends}{\describe{
FLlst
list
vector
}}

\section{Constructor}{
\describe{
\item{}{The \code{FLStocks(object)} constructor method allows simple creation of new FLStocks with the methods described below.
}
\item{ signature(object=ANY) :}{Returns an FLStocks object with the given named elements}
\item{ signature(object=missing) :}{Returns an FLStocks object}
\item{ signature(object=list) :}{Returns an FLStocks object with the provided list as its core}
}}
\section{Methods}{\describe{
\item{\link[graphics]{plot}(graphics):}{Default plot for FLStocks. Different lines for each stock object are shown in panel panels, corresponding to \emph{rec}, \emph{ssb}, 
\emph{catch} and \emph{harvest}. A \emph{key} argument turns off (FALSE, default) or on (TRUE) a figure key. A new key can also be provided (see \emph{key} entry in \code{\link{xyplot}}.}
\item{}{\code{signature(x=FLStocks,y=missing)}}
}}
\author{The FLR Team}
\seealso{\link[graphics]{plot}, \link{FLlst}, \link[base]{list}}
\examples{
data(ple4)
fls <- FLStocks(sa=ple4, sb=window(ple4, end=1980))
summary(fls)
}
\keyword{classes}
