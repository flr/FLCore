\docType{class}
\name{FLQuantPoint}
\alias{FLQuantPoint}
\alias{FLQuantPoint,FLQuant-method}
\alias{FLQuantPoint-class}
\alias{FLQuantPoint-methods}
\title{Class FLQuantPoint}
\description{
The \code{FLQuantPoint} class summarizes the contents of an
\code{FLQuant} object with multiple iterations along its
sixth dimension using a number of descriptive statistics.
}
\details{
An object of this class has a set structure along its sixth
dimension (\emph{iter}), which will always be of length 5,
and with dimnames \emph{mean}, \emph{median}, \emph{var},
\emph{uppq} and \emph{lowq}. They refer, respectively, to
the sample mean, sample median, variance, and lower (0.25)
and upper (0.75) quantiles.

Objects of this class wil be typically created from an
\code{FLQuant}. The various statistics are calculated along
the \emph{iter} dimension of the original \code{FLQuant}
using \code{\link[base]{apply}}.
}
\section{Slots}{
  \describe{ \item{.Data:}{The main array holding the
  computed statistics. \code{array}.} \item{units:}{Units
  of measurement. \code{character}.} }
}

\section{Accesors}{
  \describe{ \item{mean,mean<-:}{'mean' element on 6th
  dimension, arithmetic mean.}
  \item{median,median<-:}{'median' element on 6th
  dimension, median.} \item{var,var<-:}{'var' element on
  6th dimension, variance.} \item{lowq,lowq<-:}{'lowq'
  element on 6th dimension, lower quantile (0.25 by
  default).} \item{uppq,uppq<-:}{'uppq' element on 6th
  dimension, upper quantile (0.75 by default).} }
}

\section{Constructor}{
  Inputs can be of class: \describe{
  \item{\code{FLQuant}:}{An FLQuant object with iters (i.e.
  dim[6] > 1)} }
}

\section{Validity}{
  \describe{ \item{iter:}{iter dimension is of length 5.}
  \item{Dimnames:}{iter dimnames are 'mean', 'median',
  'var', 'uppq' and'lowq'} }
}
\examples{
flq <- FLQuant(rnorm(2000), dim=c(10,20,1,1,1,200))
flqp <- FLQuantPoint(flq)
summary(flqp)
mean(flqp)
var(flqp)
rnorm(200, flqp)
}
\author{
The FLR Team
}
\seealso{
\link{FLQuant}
}
\keyword{classes}

