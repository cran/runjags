\name{dump.format}
\alias{dump.format}
\title{Produce a Character String in the R Dump Format to Be Used With Jags}
\description{
   Convert a named list of numeric vector(s) or array(s) of data or
   initial values to a character string in the correct format to be read
   by JAGS as either data or initial values, using the run.jags
   function.
}
\usage{
dump.format(data=list())
}
\arguments{
   \item{data}{A named list of numeric or integer (or something that can
   be coerced to numeric) vectors, matrices or arrays.  The name of each
   list item will be used as the name of the resulting dump.format
   variables.  Alternatively, if the list is of length 2 and is not
   named, and the first element is a character string, the first element
   will be used as the name for the second element.}
}
\value{A character string in the R dump format.
}

\details{This function creates a character string of the supplied
variables in the same way that dump() would, except that the result is
returned as a character string rather than written to file. 
Additionally, dump.format() will look for any variable with the name
'.RNG.name' and double quote the value if not already double quoted (to
ensure compatibility with JAGS).}

\seealso{
   \code{\link{run.jags}}
}

\author{Matthew Denwood \email{matthew.denwood@glasgow.ac.uk}}

\examples{

initial.values.1 <- dump.format(list(mean="1", sd="0.1", 
	lambda=matrix(1, ncol=3, nrow=10)))
initial.values.2 <- dump.format(list(mean="10", sd="10", 
	lambda=matrix(10, ncol=3, nrow=10)))
data <- dump.format(list(N="10", Count=c(4,2,7,0,6,9,1,4,12,1)))

}
\keyword{methods}