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

# SET iter S3 to call iter(ANY) S4
.onLoad <- function(lib, pkg) {
  setOldClass(c("containeriter", "iter"))
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

# find_original_name(s) {{{

find_original_name <- function(fun) {

  # CHECK input
  if(!is.function(fun))
    stop("Input must be a function")

  # RETURN cached attribute, works across save & load
  nm <- attr(fun, ".name")
  if(!is.null(nm) && nzchar(nm))
    return(nm)

  # 'NULL' function
  if(is.null(formals(fun)))
    if(is.null(tryCatch(do.call(fun, args=list()), error=function(e) NA)))
      return("NULL")

  fun_body    <- body(fun)
  fun_formals <- formals(fun)

  # FUNCTIONS to match function body and formals
  .match_strict <- function(obj)
    is.function(obj) &&
    identical(body(obj),    fun_body) &&
    identical(formals(obj), fun_formals)

  .match_body <- function(obj)
    is.function(obj) &&
    identical(body(obj), fun_body)

  # FUNCTION to search a namespace
  .search_ns <- function(ns, pkg) {
    objs <- ls(envir=ns, all.names=TRUE)
    for(i in objs) {
      obj <- tryCatch(get(i, envir=ns, inherits=FALSE), error=function(e) NULL)
      if(!is.null(obj) && .match_strict(obj))
        return(paste(pkg, i, sep="::"))
    }
    for(i in objs) {
      obj <- tryCatch(get(i, envir=ns, inherits=FALSE), error=function(e) NULL)
      if(!is.null(obj) && .match_body(obj))
        return(paste(pkg, i, sep="::"))
    }
    NULL
  }

  # GET environment of function
  ns <- environment(fun)

  # SEARCH the global environment
  objs <- setdiff(ls(envir = .GlobalEnv, all.names = TRUE), ".Last.value")

  for(i in objs) {
    obj <- tryCatch(get(i, envir = .GlobalEnv, inherits = FALSE),
                    error = function(e) NULL)
    if(!is.null(obj) && .match_strict(obj))
      return(paste(".GlobalEnv", i, sep = "::"))
  }

  for(i in objs) {
    obj <- tryCatch(get(i, envir = .GlobalEnv, inherits = FALSE),
                    error = function(e) NULL)
    if(!is.null(obj) && .match_body(obj))
      return(paste(".GlobalEnv", i, sep = "::"))
  }

  # SEARCH current namespace
  if(isNamespace(ns)) {
    hit <- .search_ns(ns, getNamespaceName(ns))
    if(!is.null(hit)) return(hit)
  }

  # SEARCH deserialised namespace
  pkg_guess <- sub("^package:", "", environmentName(ns))
  if(nchar(pkg_guess) > 0 && pkg_guess %in% loadedNamespaces()) {
    hit <- .search_ns(asNamespace(pkg_guess), pkg_guess)
    if(!is.null(hit)) return(hit)
  }

  # SCAN all loaded namespaces
  for(pkg in loadedNamespaces()) {
    hit <- .search_ns(asNamespace(pkg), pkg)
    if(!is.null(hit)) return(hit)
  }

  return("NULL")
}

find_original_names <- function(funs) {
  unlist(lapply(funs, find.original.name))
}
# }}}
