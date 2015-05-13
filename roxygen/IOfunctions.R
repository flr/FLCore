#' Input/Output of FLR objects
#' 
#' These functions read and save FLR objects of class \code{FLStock},
#' \code{FLIndex} and \code{FLQuant} to and from various datafile formats
#' commonly used in fisheries work and stock assessment.
#' 
#' These functions are used for reading and writing stock and CPUE data used to
#' conduct stock assessment. A number of data input formats are currently
#' supported. These include the 'Lowestoft VPA Suite file format' which
#' comprises a number of flat ascii data files for catch numbers at age, catch
#' weights at age, maturity, etc. and the 'ICCAT Adapt Format'
#' 
#' For the 'Lowestoft VPA Suite file format' each input file contains header
#' information specifying the dimensions of the data matrix which may be comma,
#' space or tab delimited.  Any comments in the file must be prefixed with a
#' '\#'. An index file gives the names of the individual data files to be read
#' in. This is the file that should be passed to \code{readFLStock}. A single
#' file can be read by \code{readVPA} into an \code{FLQuant}.
#' 
#' The 'Adapt file format' comprises a single file for input containing both
#' the biological parameters, catches and catch per unit effort data.
#' 
#' If information on discards numbers at age and discards weights at age are
#' available and these files are specified in the index file then they will be
#' read into the \code{FLStock} object otherwise the discards slots in the
#' \code{FLStock} object will remain empty.
#' 
#' For reading CPUE data into an FLindex the file containing the CPUE data
#' should be passed to the \code{readFLIndex} function if only one CPUE series
#' is given or else to \code{readFLIndices} if multiple series are given.
#' 
#' Confusingly, the file giving the names of the individual \code{FLStock}
#' input files is often called the index file. It is different to the file
#' containing the CPUE data used for 'tuning' an assessment which is also
#' sometimes called the index file.
#' 
#' \code{writeFLStock} creates a set of files in 'Lowestoft File format'. This
#' function takes \code{output.file} as its argument, which is the base
#' filename from which the output files will be named according to their
#' content. A directory can be included in this argument (e.g.  via
#' \code{file.path}) to specify where the files should be written.
#' 
#' @aliases readVPAFile readFLStock writeFLStock readFLIndex readFLIndices
#' read.FLStock read.FLIndex read.FLIndices
#' @param file,file2 name of file containing data in correct format.
#' @param output.file directory and base filename where to place 'Lowestoft VPA
#' Suite FOrmat' files
#' @param type this can either be "VPA" or "adapt" for 'Lowestoft' or 'ICCAT'
#' format
#' @param name name for object created
#' @param index.names names for individual objects in FLIndices
#' @param desc description for object created
#' @param descs descriptions for individual objects in FLIndices
#' @param m natural mortality, default = 0.2, only used for 'ICCAT Adapt
#' Format'
#' @param quant name for quant dimension default is "age"
#' @param quiet logical, suppress chit-chat
#' @param sep character separating columns of data
#' @param na.strings string used to represent NA values in the input files
#' @param no.discards should discards be assumed to be zero?
#' @param units units of measurement to be stored in the units attribute
#' @param FLStock FLStock object to be saved
#' @return An object of class \code{FLQuant}, \code{FLStock} or \code{FLIndex}
#' depending on the function.
#' @author The FLR Team
#' @seealso \link{FLQuant}, \link{FLStock}, \link{FLIndex}.
#' @references Darby, C.D. and Flatman, F. Virtual Population Analysis: version
#' 3.1 (Windows/DOS) user guide.  Information technology series, No 1. CEFAS,
#' Lowestoft, UK
#' 
#' Porch, C. E. 1997. A user's manual for VPA-2BOX Version 2.0.  National
#' Marine Fisheries Service, Miami, USA.
#' @keywords IO
#' @examples
#' 
#' \dontrun{
#' path  <- getwd()
#' 
#' ## reads a set of 'Lowestoft File Format' for a stock and creates an FLStock object
#' ple4        <-readFLStock(paste(path, "pleindex.txt", sep=""))
#' 
#' ## reads a single file in 'Lowestoft File Format' and creates an FLQuant
#' ple4.catch.n<-readVPAFile(paste(path, "plecanum.txt", sep=""))
#' 
#' ## reads a set of tuning data in 'Lowestoft File Format' and creates an FLIndices object
#' ple4.indices<-readFLIndices(paste(path, "plecpue.txt", sep=""))
#' 
#' ## reads a single index from a tuning file in 'Lowestoft File Format' and creates an FLIndex object
#' ple4.index  <-readFLIndex(paste(path, "plecpue1.txt", sep=""))
#' 
#' ## writes an FLStock to 'Lowestoft File Format' in the current working directory
#' writeFLStock(ple4,output.file=file.path(getwd(),"Ple4"))
#' }
#' 
#' 