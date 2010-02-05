\name{createFLAccesors}
\alias{createFLAccesors}
\title{Create accesor methods for a given class}
\description{
  This function creates a complete set of standard S4 class accessors and replacers. Not intended for direct use.
}
\usage{
createFLAccesors(class, exclude = character(1), include=missing)
}
\arguments{
  \item{class}{name of the class}
  \item{exclude}{Slot names to exclude}
  \item{include}{Slot names to include}
}
\author{The FLR Team}
\keyword{methods}
