# zzz.R
# FLCore/R/zzz.R

# Copyright 2003-2015 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03
# $Id: zzz.R 1635 2012-06-12 09:20:08Z imosqueira $


.onAttach <- function(lib,pkg) {
  pkgdesc <- packageDescription("FLCore")
  builddate <- gsub(';.*$', '', pkgdesc$Packaged)
  if(length(builddate) == 0)
    builddate <- date()
  packageStartupMessage(paste("FLCore (Version ", pkgdesc$Version, ", packaged: ", builddate, ")", sep = ""))
}

# ac
ac <- function(x, ...)
  as.character(x, ...)

# an
an <- function(x, ...)
  as.numeric(x, ...)

run.info <- function(pkgs) {

  info <- as.matrix(c(unlist(setNames(lapply(pkgs, function(x)
    # pkgs Version
    packageDescription(x)$Version), pkgs)),
    # R.version and .platform
    setNames(unlist(R.version[c("version.string", "platform")]),
    c("R", "platform")),
    # date
    date=format(Sys.time(), usetz=TRUE)))

  colnames(info) <- "Version"

  return(info)
}
