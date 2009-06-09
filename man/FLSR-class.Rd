\name{FLSR}
\docType{class}
\alias{FLSR-class}
\alias{FLSR}
\alias{FLSR-methods}
\alias{FLSR,ANY-method}
\alias{FLSR,missing-method}
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
\section{Constructor}{
\describe{
\item{}{The \code{FLSR(object)} constructor method allows simple creation of new FLSR with the methods described below.
}
\item{ signature(model=missing) :}{Creates an empty \code{FLSR} object.}
\item{ signature(model=character) :}{Creates an \code{FLSR} from an already existing stock-recruitment model 
     function named \code{model}. This function must contain at least a formula for the model and additionaly
     it can contain a likelihood function for the model and a function to calculate initial parameters. 
     See \link{SRModels} for details in avaiblable stock-recruitment functions.}
\item{ signature(model=function) :}{Creates an \code{FLSR} object using information given in the function.
    This function can be an empty function in which case an empty \code{FLSR} will be returned or a 
    function that returns a named list with a formula for \code{model} and additionaly functions for 
    \code{logl} and \code{initial} slots.}
\item{ signature(model=formula) :}{Creates an \code{FLSR} object with the given formula in \code{model} slot.}
}}
\section{Constructor details}{\describe{
Any of the slots of the \code{FLSR} class can be used as argument in (...), in which case the corresponding slot is filled with the given argument. 

If no 'FLQuant' slots are supplied in the call to the constructor 'FLSR()', an 'FLSR' object with dimension '(1,1,1,1,1,1)' in the 'FLQuant' slots is returned. Otherwise if one of the arguments in (...) is an 'FLQuant', an 'FLSR' object which slots have the same dimension as the 'FLQuant' argument is returned. If two ore more 'FLQuant' slots are supplied in the call to the function, they must have the same dimension.
}}

\section{Methods}{\describe{
All methods are inherited from \code{\link{FLModel-class}}, except for
\item{\link[graphics]{plot}(graphics) :}{Plot an FLSR object}
\item{}{\code{ signature(x=FLSR,y=missing) }}
}}
\author{The FLR Team}
\seealso{ \link{FLModel}, \link{FLComp} }
\examples{
    # Create an empty FLSR object.
    sr1 <- FLSR()
    
    # Create an  FLSR object using the existing SR models. 
    sr2 <- FLSR(model = 'ricker')
    sr2@model
    sr2@initial
    sr2@logl
    
    sr3 <- FLSR(model = 'bevholt')
    sr3@model
    sr3@initial
    sr3@logl
    
    # Create an FLSR using a function.
    mysr1 <- function(){
        model <- rec ~ a*ssb^b
        return(list(model = model))}
    
    sr4 <- FLSR(model = mysr1)

    # Create an FLSR using a function and check that it works.
    mysr2 <- function(){
        formula <- rec ~ a+ssb*b
        
        logl <- function(a, b, sigma, rec, ssb) sum(dnorm(rec, 
            a + ssb*b, sqrt(sigma), TRUE))
        
       initial <- structure(function(rec, ssb) {
            a     <- mean(rec)
            b     <- 1
            sigma <- sqrt(var(rec))
            
            return(list(a= a, b = b, sigma = sigma))}, lower = c(0, 1e-04, 1e-04), upper = rep(Inf, 3))
        
       return(list(model = formula, initial = initial, logl = logl))
    }
      
    ssb <- FLQuant(runif(10, 10000, 100000))
    rec <- 10000 + 2*ssb + rnorm(10,0,1)  
    sr5 <- FLSR(model = mysr2, ssb = ssb, rec = rec)
    
    sr5.mle <- fmle(sr5)
    sr5.nls <- nls(sr5)
    
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
