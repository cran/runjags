\name{run.jags.study}

\alias{run.jags.study}
\alias{run.JAGS.study}

\title{Run an MCMC Model in JAGS Using Multiple Simulated Datasets}


\description{

This function can be used to fit a user specified JAGS model to multiple
datasets with automatic control of run length and convergence, over a
distributed computing cluster such as that provided by snow.  The
results for monitored variables are compared to the target values
provided and a summary of the model performance is returned.  This may
be used to facilitate model validation using simulated data, or to
assess model fit using a 'drop-k' type cross validation study where one
or more data points are removed in turn and the model's ability to
predict that datapoint is assessed.

}

\usage{

run.jags.study(simulations, model=NULL, datafunction=NULL,
	targets=list(), confidence=0.95, record.chains=FALSE,
	max.time="15m", runjags.options=list(), cat.progress=FALSE, 
	test=TRUE, parallel.method=parLapply, ...)

}

\arguments{

\item{simulations}{the number of datasets to run the model on}

\item{model}{the JAGS model to use, in the same format as would be
specified to \code{\link{run.jags}}, except that use of the #inits#
tag is not allowed (initial values must be specified using the inits
argument, which may be a function - see the example below).}

\item{datafunction}{an optional function that will be used to specify
the data. If provided, this must take either zero arguments, or one
argument representing the simulation number, and return either a named
list or character vector in the R dump format containing the data
specific to that simulation.  It is possible to specify any data that
does not change for each simulation using a #data# <variable> tag in 
the model code.  If a datafunction is not provided, the data will be the
same for all simulations (and a warning will be printed).}

\item{targets}{a named list of variables (which can include
vectors/arrays) with values to which the model outputs are compared (if
stochastic).  The target variable names are also automatically included
as monitored variables.}

\item{confidence}{a probability (or vector of probabilities) to use when
calculating the proportion of credible intervals containing the true
target value.  Default 95\% CI.}

\item{record.chains}{option to return the full runjags objects returned
from each simulation as a list item named 'runjags'.  Default FALSE.}

\item{max.time}{the maximum time for which each individual simulation
is allowed to run by the underling autorun.jags function. Acceptable units
include 'seconds', 'minutes', 'hours', 'days', 'weeks', or the first
letter(s) of each.  Default is 15 minutes.}

\item{runjags.options}{a named list of options to be passed to the
underlying \code{\link{autorun.jags}} function used to run the models.}

\item{cat.progress}{option to print a message when individual
simulations have finished running.  This is available for use with
lapply, but messages will not be printed for some parallel methods (such
as the default parLapply).  Default FALSE.}

\item{test}{option to test the model compilation on a single (randomly
chosen) dataset, to ensure that the model compiles before calling the
parallel method.  Default TRUE.}

\item{parallel.method}{a function that will be used to call the repeated
simulations.  This must take the first two arguments 'X' and 'FUN' as
for \code{\link[base]{lapply}}, with other optional arguments passed
through from the parent function call.  Default uses
\code{\link[parallel]{parLapply}}, but \code{\link[base]{lapply}} or
\code{\link[parallel]{mclapply}} could also be used.}

\item{...}{optional arguments to be passed directly to the parallel
method function, such as 'cl' in the case of parLapply.}

}

\value{ 

An object of class \code{\link{runjags.study-class}}, containing a
summary of the performance of the model with regards to the target
variables specified.  If record.chains==TRUE, an element named 'runjags'
containing a list of all the runjags objects returned will also be
present.  Any error messages given by individual simulations will
be contained in the $errors element of the returned list.

}

\seealso{

\code{\link{run.jags}},
\code{\link{runjags.study-class}}

}



\author{Matthew Denwood \email{matthew.denwood@glasgow.ac.uk}}



\examples{

# Perform a drop-1 validation study for a simple model:

themodel <- "
model{

	for(i in 1:N){
		Y[i] ~ dnorm(true.y[i], precision)
		true.y[i] <- (m * X[i]) + c
	}
	m ~ dunif(-1000,1000) 
	c ~ dunif(-1000,1000)
	precision ~ dexp(1)
	
	#data# N, X
}"

# Simulate the data
set.seed(1)
N <- 20
X <- 1:N
Y <- rnorm(length(X), 2*X + 1, 1)

# Some initial values to use for 2 chains:

initfun <- function(chain){

	# data is made available within this function when it
	# is evaluated for each simulation:
	stopifnot(length(data$X) == data$N)
	
	m <- c(-10,10)[chain]
	c <- c(10,-10)[chain]
	precision <- c(0.01,100)[chain]

	.RNG.seed <- chain
	.RNG.name <- c("base::Super-Duper", 
	"base::Wichmann-Hill")[chain]
	
	return(list(m=m, c=c, precision=precision, 
	.RNG.seed=.RNG.seed, .RNG.name=.RNG.name))
}

# A simple function that removes (over-writes with NA) one datapoint at a time:
datafun <- function(s){
	simdata <- Y
	simdata[s] <- NA
	return(list(Y=simdata))
}

\dontrun{

# Set up a cluster to use with the parLapply method:
library(parallel)
cl <- makeCluster(20)

# Call the 20 simulations over the snow cluster:
results <- run.jags.study(simulations=20, model=themodel, datafunction=datafun, 
targets=list(Y=Y, m=2, c=1), runjags.options=list(n.chains=2, inits=initfun),
cl=cl)

# Examine the results:

results

}

}

\references{
	M. J. Denwood, "runjags: An R Package Providing Interface Utilities, Distributed 
	Computing Methods and Additional Distributions For MCMC Models in JAGS," Journal 
	of Statistical Software, [Under review].
}

\keyword{methods}
