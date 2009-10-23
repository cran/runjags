\name{new_unique}
\alias{new_unique}
\title{Create a Unique Filename}
\description{
   Search the current working directory for a file or directory matching the input name, and if it exists suggest a new name by appending a counter to the input name.  Alternatively, the function can ask the user if the existing file should be overwritten, in which case the existing file will be erased if the answer is 'yes'.  The function also checks for write access permissions at the current working directory.
}
\usage{
new_unique(name = NA, suffix = "", ask = FALSE, 
   prompt = "A file or directory with this name already exists.  Overwrite?")
}
\arguments{
   \item{name}{the filename to be used (character string).  A vector of character strings is also permissible, in which case they will be pasted together.  One or more missing (NA) values can also be used, which will be replaced with a randomly generated 9 character alphanumeric string.  Default NA.}
   \item{suffix}{the file extension (including '.') to use (character string).  Default none.}
   \item{ask}{if a file exists with the input name, should the function ask to overwrite the file? (logical)  If FALSE, a new filename is used instead and no files will be over-written.  Default FALSE.}
   \item{prompt}{what text string should be used to prompt the user? (character string)  Ignored is ask==FALSE.  A generic default is supplied.}
}
\value{A unique filename that is safe to use without fear of destroying existing files
}
\seealso{
   \code{\link{ask}}
}

\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk} funded as part of the DEFRA VTRI project 0101.}

\examples{
#  Create a file name that is unlikely to exist already, with a .R extension.
new_unique(c("new_file", NA), ".R", ask=FALSE)
}
\keyword{methods}
