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

#' Weighted means across FLQuants
#'
#' @description
#' Two `weighted.mean` methods are provided for objects of class `FLQuants`,
#' a `list` of `FLQuant` to be averaged element-wise. They differ in the
#' class of the weights argument, `w`, and, more importantly, in how each
#' one handles `NA` values in `x`. See *NA handling* below before choosing
#' between them.
#'
#' @details
#' **`w = "FLQuants"`**: weights can vary across every dimension of `x`
#' (age, year, iter, ...), not just by element, since `w` is itself a list
#' of `FLQuant` matching `x` in length and dimensions. This is the method to
#' use when, for example, combining landings and discards weights-at-age
#' weighted by landings and discards numbers-at-age, where the weights
#' themselves change by age and year.
#'
#' **`w = "numeric"`**: a single scalar weight is given per element of `x`,
#' constant across all dimensions. This is the simpler case of combining a
#' small number of series (e.g. CPUE or survey indices) with fixed relative
#' importance.
#'
#' @section NA handling — the key difference:
#' For `w = "FLQuants"`: `NA` values in `x` are zeroed out for the
#' numerator, and the corresponding weight is also zeroed via an internal
#' NA-flag `FLQuants`, so an `NA` element contributes neither to the
#' weighted sum nor to the sum of weights at that position. A small constant
#' (`1e-36`) is added to the summed weights to avoid division by zero where
#' every weight is zero. If the result is *still* `NA` at some position
#' (e.g. every element of `x` was `NA` there), it is replaced by the plain,
#' unweighted arithmetic mean of `x`, `Reduce('+', x) / length(x)` — note
#' this fallback is computed from the original `x`, so it can itself remain
#' `NA` if `x` is `NA` at every element for that position; this method has
#' no `na.rm` argument, the behaviour above is not toggleable.
#'
#' For `w = "numeric"`: NA handling is controlled by `na.rm`. With the
#' default `na.rm = TRUE`, `NA` values are excluded from both the weighted
#' sum and the sum of weights (`wsum`, tracked explicitly rather than via an
#' epsilon), so a missing index does not dilute the indices that do have
#' data; positions where every element of `x` is `NA` return `NA` directly,
#' with no arithmetic-mean fallback. With `na.rm = FALSE`, no special
#' handling is applied and any `NA` in `x` propagates to the result, as for
#' the default `stats::weighted.mean`.
#'
#' @param x An `FLQuants` object containing the values to be averaged.
#' @param w Weights to be used, either as an `FLQuants` object of the same
#' length and dimensions as `x`, or as a `numeric` vector with one value
#' per element of `x`.
#' @param na.rm Only used when `w` is `numeric`: should `NA` values be
#' excluded element-wise, rather than propagated? `logical`, defaults to
#' `TRUE`. Ignored, with no equivalent behaviour, when `w` is `FLQuants`.
#' @param ... Extra arguments, currently unused.
#'
#' @return A single `FLQuant` object with the weighted mean across `x`.
#'
#' @name weighted.mean
#' @rdname weighted.mean
#' @aliases weighted.mean,FLQuants,FLQuants-method weighted.mean,FLQuants,numeric-method
#' @docType methods
#' @author The FLR Team
#' @seealso [FLCore::FLQuants] [stats::weighted.mean]
#' @keywords methods
#' @md
#' @examples
#' data(ple4)
#'
#' # w = "FLQuants": weights vary by age and year
#' x <- FLQuants(landings.wt(ple4), discards.wt(ple4))
#' w <- FLQuants(landings.n(ple4), discards.n(ple4))
#' weighted.mean(x, w)
#'
#' \dontrun{
#' # w = "numeric": a single fixed weight per element of x
#' fqs <- FLQuants(a=catch(ple4), b=catch(ple4) * 1.1)
#' weighted.mean(fqs, w=c(1, 3))
#' }
NULL

#' @rdname weighted.mean
#' @export
setMethod("weighted.mean", signature(x="FLQuants", w="FLQuants"),
  function(x, w) {
  # TURN value NAs to 0s
  xa <- lapply(x, function(i) ifelse(is.na(i), 0, i))

  # CREATE NA flags
  na <- FLQuants(lapply(x, function(i) FLQuant(ifelse(is.na(i), 0, 1))))

  # COMPUTE average
  res <- Reduce('+', Map('*', x, w * na)) / Reduce('+', lapply(w * na, '+', 1e-36))
  # COMPUTE arithmetic mean
  arm <- Reduce('+', x) / length(x)
  # SUBSTITUTE NAs with arithmetic mean
  res[is.na(res)] <- c(arm[is.na(res)])
  return(res)
})

#' @rdname weighted.mean
#' @export
setMethod("weighted.mean", signature(x="FLQuants", w="numeric"),
  function(x, w, na.rm=TRUE, ...) {
  if(length(w) != length(x))
    stop("'w' must be of the same length as 'x'")
  if(isTRUE(na.rm)) {
    # ACCUMULATE weighted sum, weight sum and non-NA count, NAs as 0
    acc <- Reduce(function(acc, y) {
      ind <- y[[1]]
      wi <- y[[2]]
      isna <- is.na(ind)
      ind[isna] <- 0
      list(
        sum  = acc$sum + ind * wi,
        wsum = acc$wsum + wi * !isna,
        n    = acc$n + !isna
      )
    }, Map(list, x, w), init=list(sum=0, wsum=0, n=0))
    out <- acc$sum / acc$wsum
    out[acc$n == 0] <- NA
  } else {
    # STANDARD weighted mean, NAs propagate
    out <- Reduce(`+`, Map(function(i, wi) i * wi, x, w)) / sum(w)
  }
  return(out)
  }
)

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
