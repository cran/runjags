
\name{run.jagsfile}

\alias{run.jagsfile}

\alias{run.JAGSfile}

\title{Read a User Specified Model in a WinBUGS Type Textfile or
Character Variable, and Run the Simulation in JAGS from Within R}

\description{

Runs a user specified JAGS (similar to WinBUGS) model in a WinBUGS type
textfile or character variable from within R, returning a list of the
MCMC chain(s) along with optional convergence diagnostics,
autocorrelation diagnostics and monitored variable summaries.  JAGS is
called using the lower level function \code{\link{run.jags}}, and
\code{\link{read.winbugs}} is used to extract model and data definitions
from the specified file or string.

}

\usage{

run.jagsfile(path=stop("No path or model string supplied"),
datalist = NA, initlist = NA, n.chains=NA, data=NA,
model=NA, inits=NA, monitor=NA, call.jags=TRUE,
autorun=FALSE, ...)

}

\arguments{

\item{path}{either a relative or absolute path to a textfile (including
the file extension) containing a model in the JAGS language and possibly
monitored variable names, data and/or initial values, or a character
string of the same.  May also be a vector of paths to different text
files, possibly separately containing the model, data and intitial
values.  No default.  The model must be started with the string
'model\{' and ended with '\}' on new lines.  Data must be similarly
started with 'data\{', monitored variables with 'monitor\{', and initial
values as 'inits\{', and all ended with '\}'.  If multiple models are
found, all but the first one are ignored with a warning. Multiple data
blocks and monitor blocks are combined, multiple inits blocks are used
for different chains.  The model block may also contain automatically
generated data and initial values variables using '#data# variable' and
'#inits# variable', and more monitored variables using '#monitor#
variable'.  See \code{\link{read.winbugs}} for more information.}

\item{datalist}{an optional named list containing variables used as
data, or alternatively a function (with no arguments) that returns a
named list.  If any variables are specified in the model block using
'#data# variable', the value for the corresponding named variable is
taken from datalist if present (or the result of datalist() if specified
as a function which is useful for specifying randomly generated data),
or the parent environment, or finally the global environment if not
found anywhere else.  Ignored if '#data# variable' is not used in the
model block.  Default NA.}

\item{initlist}{an optional named list containing variables used as
initial values, or alternatively a function (with a single argument
representing the chain number) that returns a named list.  If any
variables are specified in the model block using '#inits# variable',
the value for the corresponding named variable is taken from initlist if
present (or the result of datalist(chain.no) if specified as a function
which allows both randomly generated initial values and different values
for each chain), or the parent environment, or finally the global
environment if not found anywhere else.  Ignored if '#inits# variable'
is not used in the model block.  Note: different chains are all given
the same starting values if specified as a named list or taken from any
envirnoment; if different values are desired for each chain initlist
should be specified as a function.  Default NA.}

\item{n.chains}{the number of chains to use with the simulation.  More
chains will improve the sensitivity of the convergence diagnostic, but
will cause the simulation to run more slowly.  If NA, the number of
chains will be taken from the number of inits blocks in the model file. 
If NA and no inits blocks are found, 2 chains are used with a warning. 
Default NA.}

\item{data}{OPTIONAL character vector in the R dump format (or named
list) containing the data.  If supplied (!=NA), all data in the model
file is ignored.  Default NA.}

\item{model}{OPTIONAL model in JAGS syntax.  If supplied (!=NA), the
model in the model file is ignored.  Default NA.}

\item{inits}{OPTIONAL character vector(s) in the R dump format
containing the initial value(s).  If supplied (!=NA), all inits in the
model file are ignored.  Default NA.}

\item{monitor}{OPTIONAL character vector containing the monitored
variables.  If supplied (!=NA), all monitor statements in the model
block are ignored.  Default NA.}

\item{call.jags}{option results in either simulation being called if
TRUE, or returns a named list of the data, model, initial values,
monitored variables and number of chains (which can be supplied to
run.jags) if FALSE.}

\item{autorun}{option to call autorun.jags rather than run.jags for the
simulation, which allows automatic calculation of the necessary run
length and convergence diagnostics.  If TRUE, burnin, sample and
check.conv are ignored.  See also \code{\link{autorun.jagsfile}} for a
wrapper for this function.  Default FALSE.}

\item{...}{other options to be passed directly to run.jags (see
\code{\link{run.jags}}) or autorun.jags (see
\code{\link{autorun.jags}}).}

}



\value{The output of run.jags (or autorun.jags if autorun==TRUE).  See
the help file \code{\link{run.jags}} or \code{\link{autorun.jags}} for
more information.

}

\seealso{

\code{\link{autorun.jagsfile}},

\code{\link{run.jags}},

\code{\link{autorun.jags}},

\code{\link{read.winbugs}}

}



\author{Matthew Denwood \email{matthew.denwood@glasgow.ac.uk}}



\examples{

# run a model to calculate the intercept and slope of the expression 
# y = m x + c, assuming normal observation errors for y:

\dontrun{

# Model in the JAGS format

# Model in the JAGS format
model <- "model {
for(i in 1 : N){ #data# N
Y[i] ~ dnorm(true.y[i], precision); #data# Y
true.y[i] <- (m * X[i]) + c; #data# X
}
m ~ dunif(-1000,1000); #inits# m
c ~ dunif(-1000,1000);
precision ~ dexp(1);
#monitor# m, c, precision
}"

# Simulate the data
X <- 1:100
Y <- rnorm(length(X), 2*X + 10, 1)
N <- length(X)

initfunction <- function(chain) return(switch(chain, 
	"1"=list(m=-10), "2"=list(m=10)))

results <- run.jagsfile(model, n.chains=2, initlist=initfunction)

# Analyse the results
results$summary}

}



\keyword{models}