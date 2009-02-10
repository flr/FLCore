\name{FLSR}
\docType{methods}
\alias{FLSR}
\alias{FLSR-methods}
\alias{FLSR,ANY-method}
\alias{FLSR,missing-method}
\title{Method FLSR}
\description{
 Creates a new FLSR object.
}
\section{Generic function}{
 FLSR(model,...) 
}
\section{Methods}{\describe{
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
\details{
        Any of the slots of the \code{FLSR} class can be used as argument in (...), in which case the 
  corresponding slot is filled with the given argument. 
    
     If no 'FLQuant' slots are supplied in the call to the constructor 'FLSR()',
an 'FLSR' object with dimension '(1,1,1,1,1,1)' in the 'FLQuant' slots is returned.
Otherwise if one of the arguments in (...) is an 'FLQuant', an 'FLSR' object which slots
have the same dimension as the 'FLQuant' argument is returned. If two ore more 'FLQuant' 
slots are supplied in the call to the function, they must have the same dimension.

}
\author{The FLR Team}
\seealso{ \link{FLComp}, \link{FLModel}, \link{FLSR-class} }
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
    
    }
\keyword{methods}
                
