\name{runjags.options}

\alias{runjags.options}
\alias{runJAGS.options}

\title{Options for the runjags package}

\description{

Utility function to change the default options for the runjags package.
Options will be used for all runjags function calls for the remainder
of the JAGS session; for a permanent solution consider adding this function
call to your R profile file.

}

\usage{

	runjags.options(...)
	
}

\arguments{

\item{...}{named option(s) to change - for a list of available options,
call the function with no parameters.}

}



\value{

The current value of all available runjags options (after applying any
changes specified), as a named list.

}


\seealso{

\code{\link{run.jags}},

\code{\link{runjags}-class}

}



\author{Matthew Denwood \email{matthew.denwood@glasgow.ac.uk}}



\keyword{methods}
