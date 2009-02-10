\name{FLSR}
\docType{class}
\alias{FLSR-class}
\title{Class FLSR}
\description{
  Class for stock-recruitment models, based on \code{\link{FLModel-class}}. Two extra
  \code{\link{FLQuant-class}} slots are used for input data: \code{rec} for the 
  recruitment series, and \code{ssb} for Spawning Stock Biomass or any other index of
  reproductive potential.

  All the main methods for this class are inherited from \code{\link{FLModel-class}}, 
  although the plot method is specific to FLSR, and methods exist to access the specific
  \code{FLQuant} slots, \code{rec} and \code{ssb}.

  A series of commonly-used stock-recruitment models are already available, including the
  correspodning likelihood functions and calculation of initial values. See 
  \code{\link{SRModels}} for more details and the exact formulation implemented for each
  of them.
}
\section{Slots}{\describe{
\item{name}{Name of the object. \code{character}.}
\item{desc}{Description of the object. \code{character}.}
\item{range}{Range. \code{numeric}.}
\item{rec}{Recruitment series. \code{FLQuant}.}
\item{ssb}{Index of reproductive potential. \code{FLQuant}.}
\item{fitted}{Estimated values for rec. \code{FLQuant}.}
\item{residuals}{Residuals obtained from the model fit. \code{FLQuant}.}
\item{covar}{Covariates for SR model. \code{FLQuants}.}
\item{model}{Model formula. \code{formula}.}
\item{grad}{Function returning the gradient of the model. \code{function}.}
\item{logl}{Log-likelihood function. \code{function}.}
\item{initial}{Function returning initial parameter values for the optimizer. \code{function}.}
\item{params}{Estimated parameter values. \code{FLPar}.}
\item{logLik}{Value of the log-likelihood. \code{logLik}.}
\item{vcov}{Variance-covariance matrix. \code{array}.}
\item{details}{extra information on the model fit procedure. \code{list}.}
}}
\section{Extends}{\describe{
FLModel
FLComp
}}
\section{Methods}{\describe{
All methods are inherited from \code{\link{FLModel-class}}, except for
\item{\link[graphics]{plot}(graphics) :}{Plot an FLSR object}
\item{}{\code{ signature(x=FLSR,y=missing) }}
}}
\author{The FLR Team}
\seealso{ \link{FLModel}, \link{FLComp} }
\examples{
# NS Herring stock-recruitment dataset
data(nsher)

# already fitted with a Ricker SR model
summary(nsher)

plot(nsher)

# change model
model(nsher) <- bevholt()

# fit through MLE
nsher <- fmle(nsher)

plot(nsher)
}
\keyword{classes}
