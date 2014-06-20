\name{runjags.options}

\alias{runjags.options}
\alias{runJAGS.options}
\alias{runjags.getOption}
\alias{runJAGS.getOption}


\title{Options for the runjags package}

\description{

Utility function to change the default options for the runjags package.
Options will be used for all runjags function calls until the runjags
package is unloaded.  For a permanent solution, create a list named 
named '.runjags.options' containing the desired options in an R profile
file - on loading, runjags will check to see if this object exists in
the global environment and set the options automatically.
}

\usage{

	runjags.options(...)
	runjags.getOption(name)
	
}

\arguments{

\item{...}{named option(s) to change - for a list of available options,
see details below.}
\item{name}{the name of the option to get the current value of - for a 
list of available options, see details below.}

}

\details{
The following default options can be specified:

jagspath - the path to JAGS to use unless over-ridden in a function call
(uses the findjags() function by default)

method - the runjags method to use unless over-ridden in a function call
(default is 'rjags' if the rjags package is installed, or 'interruptible'
otherwise)

tempdir - default to temporary directory unless over-ridden in a
function call (default TRUE)

newwindows - use multiple windows for plots unless over-ridden in a
function call (default is platform dependent)

modules - the modules to load unless over-ridden in a function call
(default none)

linenumbers - display line numbers when printing runjags model, data and
inits class objects unless over-ridden in a function call (default none)

factories - the factories to load unless over-ridden in a function call
(default none)

inits.warning - display warning messages about initial values being not
specified or re-used (set to FALSE to suppress warnings)

rng.warning - display warning messages relating to pseudo-random number
generation for parallel chains (set to FALSE to suppress warnings)

summary.warning - display a warning message if summary statistics are
requested for a small number of samples (and a few other similar
situations)

blockcombine.warning - display a warning message if multiple data or
inits blocks are combined in a WinBUGS style model file

silent.jags - suppress output of JAGS (or rjags) when updating models

silent.runjags - suppress feedback provided by the runjags functions

predraw.plots - automatically pre-calculate convergence diagnostic plots
(this will save time when displaying plots at the cost of increased
storage requirement for the runjags object)

}

\value{

The current value of all available runjags options (after applying any
changes specified) is returned invisibly as a named list.

}


\seealso{

\code{\link{run.jags}},

\code{\link{findjags}},

\code{\link{runjags}-class}

}



\author{Matthew Denwood \email{matthew.denwood@glasgow.ac.uk}}

\examples{

\dontrun{

# Create a list of options in the global environment (perhaps in an
# R startup profile file) BEFORE load()ing runjags:
.runjags.options <- list(inits.warning=FALSE, rng.warning=FALSE)
# Or if it is run in a different environment:
# .runjags.options <<- list(inits.warning=FALSE, rng.warning=FALSE)

# Then load runjags and verify that the options have been set:
library('runjags')
print(runjags.options())


# Change the default option to remove all feedback provided by
# runjags and JAGS/rjags (only errors will be printed to screen):
runjags.options(silent.jags=TRUE, silent.runjags=TRUE)

}

}

\keyword{methods}
