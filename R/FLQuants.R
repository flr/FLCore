# FLQuants - A list of FLQuant objects
# FLCore/R/FLQuants.R

# Copyright 2003-2018 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# summary {{{
#' @rdname summary-methods
#' @aliases summary,FLQuants-methods
setMethod('summary', signature(object='FLQuants'),
  function(object)
  {
	  cat("An object of class \"FLQuants\"\n\n", sep="")
		cat("Elements:", names(object), "\n")
    cat("\n")
    for(i in seq(length(object)))
    {
      cat("Name:", names(object)[i], "\n")
      cat("\tdim  : ", dim(object[[i]]), "\n")
  		cat("\tquant: ", quant(object[[i]]), "\n")
	  	cat("\tunits: ", units(object[[i]]), "\n\n")
		  if(all(is.na(object[[i]])))
  		{
	  		cat("\tMin    :  NA\n")
		  	cat("\t1st Qu.:  NA\n")
  			cat("\tMean   :  NA\n")
	  		cat("\tMedian :  NA\n")
		  	cat("\t3rd Qu.:  NA\n")
			  cat("\tMax    :  NA\n")
  		}
	  	else
		  {
  			cat("\tMin    : ", min(object[[i]], na.rm=TRUE), "\n")
	  		cat("\t1st Qu.: ", quantile(as.vector(object[[i]]), 0.25, na.rm=TRUE), "\n")
		  	cat("\tMean   : ", mean(as.vector(object[[i]]), na.rm=TRUE), "\n")
			  cat("\tMedian : ", median(as.vector(object[[i]]), na.rm=TRUE), "\n")
  			cat("\t3rd Qu.: ", quantile(as.vector(object[[i]]), 0.75, na.rm=TRUE), "\n")
	  		cat("\tMax    : ", max(object[[i]], na.rm=TRUE), "\n")
		  }
  		cat("\tNAs    : ", format(length(as.vector(object[[i]])
	  		[!complete.cases(as.vector(object[[i]]))])/length(as.vector(object[[i]]))*100,
		  	digits=2), "%\n")
    }
  }
) # }}}

# iter {{{
setMethod("iter", signature(obj="FLQuants"),
	  function(obj, iter) {

		# simply use lapply and iter from FLQuant methods

		flqs <- FLQuants(lapply(obj,function(x,iter){x <- iter(x,iter)},iter))
		return(flqs)
	  }
) # }}}

# show  {{{
setMethod('show', signature('FLQuants'),
        function(object) {
			if(length(object) == 0)
				cat('An object of class "FLQuants": EMPTY\n')
			else
                for (n in seq(1:length(object))) {
                        cat(paste('$', names(object)[n], '\n'))
                        show(object[[n]])
                        cat('\n')
                }
        }
)   # }}}

# mcf: make compatible flquants     {{{
setMethod("mcf", signature(object="list"), function(object){
	# names
	if(!is.null(names(object))){
		flqnames <- names(object)
	} else {
		flqnames <- paste("v", 1:length(object), sep="")
	}
	# how many flquants exist ?
	v <- unlist(lapply(object, is, 'FLArray'))
	nflq <- sum(v)
	lst0 <- object[v]
	# names and dim of the compatible flq
	dn <- dimnames(lst0[[1]])
	for(i in seq(2, length=nflq-1)){
		# using the first iteraction only
		dn1 <- dimnames(lst0[[i]])
		# checking how to merge quant dim
		quant.vec <- unique(c(dn[[1]],dn1[[1]]))
		if(NA %in% (suppressWarnings(as.numeric(quant.vec))))
			dn[[1]] <- sort(quant.vec)
		else
			dn[[1]] <- as.character(sort(as.numeric(quant.vec)))

		dn[[2]] <- as.character(sort(as.numeric(unique(c(dn[[2]],dn1[[2]])))))
		dn[[3]] <- unique(c(dn[[3]],dn1[[3]]))
		dn[[4]] <- unique(c(dn[[4]],dn1[[4]]))
		dn[[5]] <- unique(c(dn[[5]],dn1[[5]]))
		dn[[6]] <- unique(c(dn[[6]],dn1[[6]]))
	}
	dflq <- unlist(lapply(dn, length))
	# new flquant
	flq <- FLQuant(dim=dflq, dimnames=dn)
	# preparing the list
	lst <- list()
	length(lst) <- nflq
	lst <- object

	# filling up the quants
	for(j in 1:length(lst)){
		dn2 <- dimnames(lst[[j]])
		flq0 <- flq
		flq0[dn2[[1]], dn2[[2]], dn2[[3]], dn2[[4]], dn2[[5]], dn2[[6]]] <- lst[[j]]
		lst[[j]] <- flq0
	}
	names(lst) <- flqnames

	# output
	FLQuants(lst)
})  # }}}

# as.data.frame	{{{
setMethod("as.data.frame", signature(x="FLQuants", row.names="ANY", optional="missing"),
  function(x, row.names, drop=FALSE, qname="qname", ...) {

    # CHECK quant names
    qnms <- unlist(lapply(x, quant))
    if(length(unique(qnms)) > 1)
      stop("'quant' names in objects do not match")

    # names
		if(is.null(names(x)))
			flqnames <- paste("v", 1:length(x), sep="")
		else if(any(is.na(names(x))))
		{
			names(x)[is.na(names(x))] <-
        paste("v", 1:length(x), sep="")[is.na(names(x))]
			flqnames <- names(x)
		}
		else
			flqnames <- names(x)

		# data.frames
		flqs.lst <- lapply(x, as.data.frame, row.names=row.names, drop=drop, ...)

  	# test classes of quant
  	flqs.class <- unlist(lapply(flqs.lst, function(x) class(x[,1])))
  	if(any(flqs.class != flqs.class[1]))
    	flqs.lst <- lapply(flqs.lst, function(x) {x[,1] <- as.factor(x[,1]); x})

		flqs.nlst <- lapply(flqs.lst, nrow)
		flqs.df <- do.call("rbind", flqs.lst)

    flqs.df[,qname] <- factor(rep(flqnames, unlist(flqs.nlst)), levels=flqnames)
  	row.names(flqs.df) <- row.names
		attr(flqs.df, 'units') <- unlist(lapply(flqs.lst, attr, 'units'))
		flqs.df
})

setMethod("as.data.frame", signature(x="FLQuants", row.names="missing",
  optional="missing"),
    function(x, ...) {
      as.data.frame(x, row.names=NULL, ...)
    }
)

# }}}

# combine {{{
setMethod('combine', signature(x='FLQuants', y='missing'),
  function(x) {

    ln <- length(x)
    dm <- matrix(unlist(lapply(x, dim)), ncol=6, nrow=ln, byrow=TRUE)

    # dim(...)[1:5] == dim(...)[1:5]
    if(any(apply(dm[,1:5], 1, function(x) x/dm[1,1:5]) != 1))
      stop("Object dimensions [1:5] must match")

    its <- dm[,6]

    res <- FLQuant(unlist(x),
      dimnames=c(dimnames(x[[1]])[1:5], list(iter=seq(sum(its)))),
      units=units(x[[1]]))

    return(res)
  }
) 

setMethod('combine', signature(x='FLQuants', y='FLQuants'),
  function(x, y) {

    if(any(c(is.na(names(x)), is.na(names(y)))))
      stop("Both FLQuants must have names")

    if(length(setdiff(names(x), names(y))) > 0)
      stop("Both FLQuants must have the same names")

    res <- lapply(names(x), function(a) combine(x[[a]], y[[a]]))
	names(res) <- names(x)
    return(FLQuants(res))
  }
)

# TODO combine(FLQuants, FLQuant)
# }}}

# join {{{

#' @rdname join
#' @examples
#' div <- divide(catch.n(ple4), dim=1)
#' is(div)
#' length(div)
#' join(div)
#' all.equal(join(divide(catch.n(ple4), dim=1)), catch.n(ple4))

setMethod('join', signature(x='FLQuants', y='missing'),
  function(x, y) {

    Reduce(join, x)
  }
)
# }}}

# group {{{

#' @examples
#' group(metrics(ple4), FUN=mean, year=year - year %% 5)

setMethod("group", signature(x="FLQuants", FUN="function"),
  function(x, FUN=sum, ...) {
  
    args <- match.call(expand.dots = FALSE)$...
    
    out <- lapply(x, function(i) do.call(group, c(list(x=i, FUN=FUN), args)))

    return(out)
  }
)
# }}}

# dbind {{{
setMethod("dbind", signature(x="FLQuants", y="missing"),
  function(x, dim=1) {
    res <- Reduce(function(a, b) dbind(a, b, dim=dim), x)
    dimnames(res)[[dim]] <- names(x)
    return(res)
  })
# }}}

# means and sums {{{

setMethod("unitMeans", signature(x="FLQuants"),
  function(x) {
    lapply(x, unitMeans)
  })

setMethod("unitSums", signature(x="FLQuants"),
  function(x) {
    lapply(x, unitSums)
  })

# }}}

# weighted.mean {{{

#' Weighted means along a FLQuants.
#'
#' Facilitates the calculation of weighted means across a FLQuants object.
#'
#' An object of class FLQuants containing elements over which an average is to
#' computed, is combined with another one, of the same length, containing
#' values to be used as weights. The overall weighted mean is calculated by
#' computing the product of each element to its corresponding weight, and
#' dividing by the sum of all weights.
#' NAs in the value elements are substituted for zeroes, so do not influence
#' the mean.
#'
#' @param x Values to be averaged, as an object of class `FLQuants`.
#' @param w weights to be used, as an object of class `FLQuants`.
#'
#' @return A single `FLQuant` object.
#'
#' @author The FLR Team
#' @seealso [FLCore::FLQuants stats::weighted.mean]
#' @keywords methods
#' @md
#' @examples
#' data(ple4)
#' # Weighted mean of landings and discards weights-at-age
#' weighted.mean(FLQuants(L=landings.wt(ple4), D=discards.wt(ple4)),
#'   FLQuants(L=landings.n(ple4), D=discards.n(ple4)))

setMethod("weighted.mean", signature(x="FLQuants", w="FLQuants"),
  function(x, w) {

  # TURN value NAs to 0s
  xa <- lapply(x, function(i) ifelse(is.na(i), 0, i))
  
  # CREATE NA flags
  na <- FLQuants(lapply(x, function(i) FLQuant(ifelse(is.na(i), 0, 1))))
 
  # COMPUTE average
  res <- Reduce('+', xa * (w * na)) / Reduce('+', w * na)

  # COMPUTE arithmetic mean
  arm <- Reduce('+', x) / length(x)

  # SUBSTITUTE NAs with arithmetic mean
  res[is.na(res)] <- c(arm[is.na(res)])

  return(res)
})

# }}}

# merge {{{
setMethod("merge", signature(x="FLQuants", y="FLQuants"),
  function(x, y) {
  return(FLQuants(Map(merge, x=x, y=y)))
  }
)

# }}}

# iterMedians {{{
setMethod("iterMedians", signature(x="FLQuants"),
  function(x) {

  res <- lapply(x, iterMedians)

  return(res)
})
# }}}
