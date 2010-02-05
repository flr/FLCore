%$Id$
\name{FLtest}
\alias{FLtest}
\alias{startTest}
\alias{finishTest}
\alias{tagTest}
\alias{setCon}
\alias{checkIdentical}
\alias{checkEqual}
\alias{checkTrue}
\alias{checkFalse}
\alias{checkFail}
\alias{checkRun}
\title{FLtest functions for running simple units tests}
\description{
	A set of functions is provided to create and run simple unit tests. The basic functions
	provided allow checking whether two objects are identical (\code{checkIdentical}),
	equal (\code{checkEqual}), if a expression is \code{TRUE} (\code{checkTrue}) 
	or \code{FALSE} (\code{checkFalse}), and if a piece of code fails (\code{checkFail})
	or runs (\code{checkRun}).

	Output of all tests is directed to a file or connection, specified by a call to 
	\code{setCon()} to set the connection, and then to \code{startTest} to specify the
	file to output to. At the end of the test the connection needs to be closed by using
	\code{finishTest}.

	Tests can be labelled with a text message for easier reading by using \code{tagTest}.

	Be warned that when any of these checks fails, failure is reported accordingly, but the
	test run will not be stopped. i.e. \code{R CMD check} will successfully finish. Test
	reports should be inspected after running them to ensure problems are detected and
	corrected.
}
\usage{
setCon(con='zz')
startTest(file="testReport.txt")
finishTest(con=getOption("con"))
tagTest(tag="My tag is better than yours !", con=getOption("con"))
checkIdentical(x, y, con=getOption("con"))
checkEqual(x, y, con=getOption("con"), tolerance =.Machine$double.eps^0.5, ...)
checkTrue(x, con=getOption("con"))
checkFalse(x, con=getOption("con"))
checkFail(x, con=getOption("con"))
checkRun(x, con=getOption("con"))
}
\arguments{
	\item{x, y}{Objects to compare or expression to run}
	\item{file}{File to direct output report to}
	\item{con}{Option for connection}
	\item{tag}{Text to label the test output file}
	\item{tolerance}{Allowed difference between two values in a comparison}
	\item{...}{Extra arguments to be passed on to all.equal}
}
\value{
	Output is directed to the file specified, so no objects are created in the workspace
	by the check functions. A \code{connection} is created by \code{startTest} but is
	subsequently closed by \code{finishTest}.
}
\seealso{ \code{\link[base]{all.equal}}, \code{\link[base]{identical}}, \code{\link[base]{connection}}
}
\examples{
	setCon()
	tmpfile <- tempfile()
	zz <- startTest(file=tmpfile)
	tagTest("Testing FLlst testing ...")

	checkTrue(is(FLQuant(), 'FLQuant'))

	finishTest()

	\dontrun{
	# You can now inspect the output by opening tmpfile in your editor
	edit(file=tmpfile)
	}
}
\references{
	\url{http://en.wikipedia.org/wiki/Unit_testing}
}
\author{Ernesto Jardim, IPIMAR. FLR Team}
\keyword{utilities}
