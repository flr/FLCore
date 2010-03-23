# FLModelDeriv - «Short one line description»
# FLModelDeriv

# Copyright 2010 FLR Team <flr-team@flr-project.org>
# Distributed under the GPL 2 or later
# $Id:  $

# Code inside these methods was taken from the numDeriv package 2009.2-1
# by Paul Gilbert, Copyright 2006-2007, Bank of Canada.
# http://www.bank-banque-canada.ca/pgilbert
# http://cran.r-project.org/web/packages/numDeriv/index.html

# computeHessian {{{
setMethod("computeHessian", signature(object="FLModel"),
  function(object, initial=FALSE, eps=1e-4, d=0.0001,
    zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2) {

    # check params are there unless initial=TRUE
    if(all(is.na(params(object))) && !initial)
      stop("")

    # get params
    parnames <- dimnames(params(object))$params
    params <- as.list(params(object))
    names(params) <- parnames

    # if initial=TRUE
    if(initial) {
      data <- list()
      datanm <- names(formals(logl(object)))
      datanm <- datanm[!datanm %in% parnames]
      for(i in datanm)
        data[[i]] <- slot(object, i)
      params <- do.call(initial(object), data)
    }

    # No NAs in params
    if(any(is.na(params)))
      stop("values in 'params' cannot be NA")

    # call numDeriv's hessian
    D <- computeD(object, params)
    H <- diag(NA,length(parnames))
    u <- length(parnames)
    for(i in 1:length(parnames))
    {
      for(j in 1:i)
      {
        u <- u + 1
        H[i,j] <- D[,u]
      }
    }
    H <- H +t(H)
    diag(H) <- diag(H)/2
    return(array(H, dimnames=list(parnames, parnames), dim=rep(length(parnames), 2)))
  }
)  # }}}

# computeD  {{{
setMethod("computeD", signature(object="FLModel"),
    function(object, params=as(object@params, 'list'), method="Richardson",
    eps=1e-4, d=0.0001, zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2) {
    
    # check v
    if (v!=2) stop("The current code assumes v is 2 (the default).")	 

    # args
    func.args <- list(eps=eps, d=d, r=r, v=v, zero.tol=zero.tol)
    
    # data
    data <- list()
    datanm <- names(formals(logl(object)))
    datanm <- datanm[!datanm %in% names(params)]
    for(i in datanm)
      data[[i]] <- slot(object, i)
    
    # params
    x <- unlist(params)
    if(any(is.na(x)))
      x <- unlist(do.call(object@initial, data))

    # f0
    f0 <- do.call(logl(object), c(data, params))

    # no. of params
    p <- length(params)

    h0 <- abs(d*x) + eps*(abs(x) < zero.tol)
    D <- matrix(0, length(f0),(p*(p + 3))/2)
    
    # length(f0) is the dim of the sample space
    # (p*(p + 3))/2 is the number of columns of matrix D.( first
    #   der. & lower triangle of Hessian)
    Daprox <- matrix(0, length(f0),r) 
    Hdiag  <-  matrix(0,length(f0),p)
    Haprox <-  matrix(0,length(f0),r)
    for(i in 1:p)    # each parameter  - first deriv. & hessian diagonal
  	{
      h <-h0
  	  for(k in 1:r)  # successively reduce h 
  	  {
  	    f1 <- do.call(logl(object), c(as.list(x + (i==(1:p))*h), data))
  	    f2 <- do.call(logl(object), c(as.list(x - (i==(1:p))*h), data))
  	    Daprox[,k] <- (f1 - f2)  / (2*h[i])    # F'(i) 
  	    Haprox[,k] <- (f1-2*f0+f2)/ h[i]^2     # F''(i,i) hessian diagonal
  	    h <- h/v     # Reduced h by 1/v.
  	   }
  	   for(m in 1:(r - 1))
       {
  	      for ( k in 1:(r-m))
  		    {
            Daprox[,k] <- (Daprox[,k+1]*(4^m)-Daprox[,k])/(4^m-1)
            Haprox[,k] <- (Haprox[,k+1]*(4^m)-Haprox[,k])/(4^m-1)
  		    }
       }
  	   D[,i] <- Daprox[,1]
  	   Hdiag[,i] <- Haprox[,1]
  	 }	  
     u <- p
  
     for(i in 1:p)   # 2nd derivative  - do lower half of hessian only
     {
       for(j in 1:i)
         {
           u <- u + 1
  	       if (i==j)
             D[,u] <- Hdiag[,i]
  	       else 
  	       {
             h <-h0
  		       for(k in 1:r)  # successively reduce h 
  		       {
  	           f1 <- do.call(logl(object), c(as.list(x + (i==(1:p))*h + (j==(1:p))*h),
                 data))
  	           f2 <- do.call(logl(object), c(as.list(x - (i==(1:p))*h - (j==(1:p))*h),
                 data))
  		         Daprox[,k]<- (f1 - 2*f0 + f2 -
  				     Hdiag[,i]*h[i]^2 - 
  				     Hdiag[,j]*h[j]^2)/(2*h[i]*h[j])  # F''(i,j)  
  		         h <- h/v	# Reduced h by 1/v.
  		       }
  		       for(m in 1:(r - 1))
  		       for ( k in 1:(r-m))
               Daprox[,k]<-(Daprox[,k+1]*(4^m)-Daprox[,k])/(4^m-1)
             D[,u] <- Daprox[,1]
  	       }
  	    }  
  	 }
     return(D)
  }) # }}}

# gradient {{{
# TODO: check and polish
setMethod('gradient', signature(func='function', x='FLPar'),
  function(func, x, method="Richardson", eps=1e-4, d=0.0001,
      zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2, show.details=FALSE, ...)
  {
    # TODO: Should grad work along all params? Or only for one? How to select it?
    # take data args
    args <- list(...)
    data <- args[names(args)%in%names(formals(func))]
    # & params
    params <- as.list(x)
    names(params) <- dimnames(x)$params 

    # foo: logl with altered param
    foo <- function(...) {
      args <- list(...)
      -1 * do.call(func, c(params[!names(params)%in%names(args)], args, data))
    }

    # Code from Paul Gilbert's numDeriv package
    # get logLik with input params
    f <- -1 * foo()

    # 
    case1or3 <- length(x) == length(f)
    if((1 != length(f)) & !case1or3)
      stop("grad assumes a scalar real valued function.")
  
    # Richardson
    if(method=="Richardson") {
      
      # number of variables.
      n <- length(x)

      #  first order derivatives are stored in the matrix a[k,i], 
      #  where the indexing variables k for rows(1 to r), i for columns (1 to n),
      #  r is the number of iterations, and n is the number of variables.
      a <- matrix(NA, r, n) 
      h <- abs(d*x) + eps * (abs(x) < zero.tol)
      
      # successively reduce h
      for(k in 1:r)  {
        if(case1or3)
          a[k,] <- (func(x + h, ...) -  func(x - h, ...))/(2*h)
        else for(i in 1:n) {
          # some func are unstable near zero
          if((k != 1) && (abs(a[(k-1),i]) < 1e-20))
            a[k,i] <- 0 
	        else
            a[k,i] <- (func(x + h*(i==seq(n)), ...) - 
	            func(x - h*(i==seq(n)), ...))/(2*h[i])
        }
        h <- h/v     # Reduced h by 1/v.
        if(show.details) {
          cat("\n","first order approximations", "\n")		
          print(a, 12)
        }
      }
      for(m in 1:(r - 1)) {	  
        a <- (a[2:(r+1-m),,drop=FALSE]*(4^m)-a[1:(r-m),,drop=FALSE])/(4^m-1)
        if(show.details & m!=(r-1) ) {
    	  cat("\n","Richarson improvement group No. ", m, "\n") 	  
    	  print(a[1:(r-m),,drop=FALSE], 12)
  	    }
      }
      return(c(a))
    }
  }
) # }}}


