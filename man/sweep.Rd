\name{sweep}
\docType{methods}
\alias{sweep,FLQuant-method}
\title{Sweep out FLQuant Summaries}
\description{
  Return an \code{FLQuant} obtained from an input \code{FLQuant} by sweeping out a summary
  statistic along the selected dimensions.
}
\section{Generic function}{
sweep(x, MARGIN, STATS, FUN="-", check.margin=TRUE, \dots)
}
\section{Method arguments}{\describe{
  \item{x}{an FLQuant.}
  \item{MARGIN}{a vector of indices giving the extents of \code{x}
    which correspond to \code{STATS}.}
  \item{STATS}{the summary statistic which is to be swept out.}
  \item{FUN}{the function to be used to carry out the sweep.  In the
    case of binary operators such as \code{"/"} etc., the function name
    must backquoted or quoted. (\code{FUN} is found by a call to
    \code{\link{match.fun}}.)}
  \item{check.margin}{logical.  If \code{TRUE} (the default), warn if the
    length or dimensions of \code{STATS} do not match the specified
    dimensions of \code{x}.  Set to \code{FALSE} for a small speed gain
    when you \emph{know} that dimensions match.}
  \item{\dots}{optional arguments to \code{FUN}.}
}}
\value{
  An FLQuant with the same shape as \code{x}, but with the summary
  statistics swept out.
}
\author{The FLR Team}
\seealso{
   \code{\link[base]{sweep}}
}
\examples{
   data(ple4)
   mean.f <- apply(harvest(ple4),2,mean)
   scaled.f <- sweep(harvest(ple4),2,mean.f,"/")
}
\keyword{methods}
