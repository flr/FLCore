# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# readAdaptFile	{{{
readAdaptFile <- function(file., m. = m) {
    skip.hash <- function(i) {
        i <- i + 1
        while (substr(scan(file., skip = i, nlines = 1, what = ("character"),
            quiet = TRUE)[1], 1, 1) == "#") i <- i + 1
        return(i)
    }

    skip.until.minuFLStock.1 <- function(i) {
        i <- i + 1
        while (scan(file., skip = i, nlines = 1, what = ("character"),
            quiet = TRUE)[1] != "-1") i <- i + 1
        return(i)
    }

    FLStock. <- FLStock()
    i <- skip.hash(0)
    FLStock.@range[c("minyear", "maxyear")] <- scan(file.,
        skip = i, nlines = 1, nmax = 2, quiet = TRUE)

    i <- skip.hash(i)
    FLStock.@range[c("min", "max", "plusgroup")] <- scan(file.,
        skip = i, nlines = 1, nmax = 3, quiet = TRUE)
        
    i <- skip.hash(i) + 1
    t. <- scan(file., skip = i, nlines = 1, nmax = 1, quiet = TRUE)

    dims   <- c(FLStock.@range["max"] -FLStock.@range["min"] +1,
                FLStock.@range["maxyear"]-FLStock.@range["minyear"]+1,
                1,1,1)
#   you need a second dims object because sometimes the year labels are also read
    dims2  <- c(dims[1]+1, dims[2])
    dimnms <- list(age  =as.character(FLStock.@range["min"] :FLStock.@range["max"]),
                   year =as.character(FLStock.@range["minyear"]:FLStock.@range["maxyear"]),
                   unit = "unique",
                   season = "all",
                   area = "unique")
    nages  <- FLStock.@range["max"]  - FLStock.@range["min"]  + 1
    nyears <- FLStock.@range["maxyear"] - FLStock.@range["minyear"] + 1

    FLStock.@harvest.spwn <- as.FLQuant(array(t./12, dim = dims, dimnames = dimnms), units="f")
    FLStock.@m.spwn <- FLStock.@harvest.spwn

    i <- skip.hash(i)
    t. <- scan(file., skip = i, nlines = 1, nmax = nages, quiet = TRUE)
    FLStock.@mat <- as.FLQuant(array(rep(t., nyears), dim = dims, dimnames = dimnms))

    i <- skip.hash(i)
    FLStock.@name <- scan(file., skip = i, nlines = 1, nmax = 1,
        quiet = TRUE, what = "character")

    i <- skip.hash(i)
    t. <- scan(file., skip = i, nmax = nyears * (nages+1), quiet = TRUE)
    FLStock.@landings.n <- as.FLQuant(array(t., dim=dims2)[-1,], dimnames=dimnms)

    i <- skip.until.minuFLStock.1(i)
    i <- skip.until.minuFLStock.1(i)
    i <- skip.until.minuFLStock.1(i)
    i <- skip.until.minuFLStock.1(i)
    j <- skip.until.minuFLStock.1(i)
    k <- skip.hash(i)
    if (j == k) {
        i <- skip.hash(j)
        t. <- scan(file., skip = i, nmax = nyears*(nages+1), quiet = TRUE)
        FLStock.@stock.wt <- as.FLQuant(array(t., dim = dims2)[-1,], dimnames=dimnms)
    }
    else {
        i <- k
        t. <- scan(file., skip = i, nmax = nyears*(nages+1), quiet = TRUE)
        FLStock.@landings.wt <- as.FLQuant(array(t., dim = dims2)[-1,], dimnames=dimnms)

        i <- skip.until.minuFLStock.1(i)
        i <- skip.hash(i)
        t. <- scan(file., skip = i, nmax = nyears * (nages+1), quiet = TRUE)

        FLStock.@stock.wt <- as.FLQuant(array(t., dim=dims2)[-1,], dimnames=dimnms)}
    FLStock.@landings.wt<-FLStock.@stock.wt
    
    FLStock.@m <- as.FLQuant(array(m., dim = dims, dimnames=dimnms))
    return(FLStock.)
}	# }}}

# readCSAFile		{{{
readCSAFile <- function(file.) {
    t.    <-scan(file=file.,skip=1,sep=",")
    nrow. <-length(t.)/9
    t.    <-t(array(t.,dim=c(9,nrow.)))
    t.    <-array(t.[,-1],dim=c(20,8),
    dimnames=list(t.[,1],c("m","c.rec","c.full","w.rec","w.full","s.rat","u.rec","u.full")))

    dmns<-list(year=dimnames(t.)[[1]],age=c("r","full"))
    s. <- FLStock(catch.n=as.FLQuant(t(array(cbind(t.[,"c.rec"],t.[,"c.full"]), dim=c(nrow.,2)))))
    s.@catch.n  <- as.FLQuant(t(array(cbind(t.[,"c.rec"],t.[,"c.full"]), dim=c(nrow.,2), dimnames=dmns)))
    s.@stock.wt <- as.FLQuant(t(array(cbind(t.[,"w.rec"],t.[,"w.full"]), dim=c(nrow.,2), dimnames=dmns)))
    s.@catch.wt <- as.FLQuant(t(array(cbind(t.[,"w.rec"],t.[,"w.full"]), dim=c(nrow.,2), dimnames=dmns)))
    s.@m        <- as.FLQuant(t(array(t.[,"m"], dim=c(nrow.,2),                          dimnames=dmns)))
    mat.0       <- FLQuant( 0,dimnames=dmns)
    mat.na      <- FLQuant(NA,dimnames=dmns)

    s.@harvest.spwn <- mat.0
    s.@m.spwn <- mat.0

    s.@landings.n  <- s.@catch.n
    s.@landings.wt <- s.@catch.wt
    s.@discards.n  <- mat.0
    s.@discards.wt <- mat.0

    s.@catch       <- apply(s.@catch.wt   *s.@catch.n,   2,sum)
    s.@landings    <- apply(s.@landings.wt*s.@landings.n,2,sum)
    s.@discards    <- apply(s.@discards.wt*s.@discards.n,2,sum)

    s.@mat         <-mat.na
    s.@stock.n     <-mat.na
    s.@harvest     <-FLQuant(NA,dimnames=dmns, units='f')

    s.@range["minyear"]     <-min(as.numeric(dimnames(t.)[[1]]))
    s.@range["maxyear"]     <-max(as.numeric(dimnames(t.)[[1]]))

    s.@desc		<-"read in from CSA file"

    return(s.)
}	# }}}

# readPAFile		{{{
readPAFile <- function(file.) {
    getmatrix <- function(file., start, nlines, yrs, ages) {
        m. <- t(as.matrix(read.table(file = file., skip = start - 
            1, row.names = 1, nrows = nlines, sep = ",",colClasses = "numeric")[, 
            (ages[1]:ages[2]) + 1 - ages[1]]))
        return(as.FLQuant(m. <- array(m., dim = c(ages[2] - 
            ages[1] + 1, yrs[2] - yrs[1] + 1), dimnames = list(ages[1]:ages[2], 
            yrs[1]:yrs[2]))))
    }
    range <- scan(file., skip = 2, nlines = 2, sep = ",")
    ages <- range[4:5]
    yrs <- range[1:2]
    FLStock. <- FLStock()
    FLStock.@m.spwn <- getmatrix(file., 6, yrs[2] - yrs[1] + 
        1, yrs, ages)
    FLStock.@harvest.spwn <- getmatrix(file., 7 + (yrs[2] - yrs[1] + 
        1), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@harvest <- getmatrix(file., 7 + 3 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
		harvest(FLStock.) <- "f"
    FLStock.@stock.wt <- getmatrix(file., 7 + 4 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@catch.wt <- getmatrix(file., 7 + 5 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@mat <- getmatrix(file., 7 + 6 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    FLStock.@m <- getmatrix(file., 7 + 7 * (yrs[2] - yrs[1] + 
        2), yrs[2] - yrs[1] + 1, yrs, ages)
    yrs[2] <- yrs[2] + 1
    FLStock.@stock.n <- getmatrix(file., 6 + 2 * (yrs[2] - yrs[1] + 
        1), yrs[2] - yrs[1] + 2, yrs, ages)
    FLStock.@range <- c(minage = ages[1], maxage = ages[2], 
        plusgroup = ages[2], minyear = yrs[1], maxyear = yrs[2])
    FLStock.@desc <- "read in from PA file"
    FLStock.@name <- scan(file., nlines = 1, what = character(0))[1]
    return(FLStock.)
}	# }}}


