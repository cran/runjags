\name{run.jags}

\alias{run.jags}
\alias{run.JAGS}
\alias{extend.jags}
\alias{extend.JAGS}
\alias{results.jags}
\alias{results.JAGS}



\title{Run or Extend a User Specified Bayesian MCMC Model in JAGS from Within R}

\description{

Runs or extends a user specified JAGS model from within R,
returning an object of class \code{\link{runjags-class}}.  
Data and initial values can either be supplied in the R dump
format (see dump.format() for an easy way to do this), or as a named
list.  A character vector of variables to monitor must also be supplied,
either to the monitor argument or inside the model code.

Requires Just Another Gibbs Sampler (JAGS), see
\url{http://www-fis.iarc.fr/~martyn/software/jags/}.

}

\usage{

run.jags(model=stop("No model supplied"), monitor = NA, data=NA,
	n.chains=NA, inits = NA, burnin = 5000, sample = 10000,
	adapt=max(200-burnin, 0), datalist=NA, initlist=NA, jags =
	findjags(), silent.jags = FALSE, summarise = TRUE, confidence=0.95,
	plots = summarise, psrf.target = 1.05, normalise.mcmc = TRUE,
	check.stochastic = TRUE, modules=c(""), factories=c(""), thin = 1,
	monitor.deviance = FALSE, monitor.pd = FALSE, monitor.pd.i = FALSE,
	monitor.popt = FALSE, check.conv = summarise, keep.jags.files =
	FALSE, tempdir=TRUE, jags.refresh=0.1, batch.jags=silent.jags,
	method=if ('rjags' \%in\% .packages()) 'rjags' else 'interruptible',
	method.options=list())


extend.jags(runjags.object=stop("The output of a runjags function (with class 'runjags') must be supplied"), 
    add.monitor=character(0), drop.monitor=character(0), drop.chain=numeric(0),
	combine=length(c(add.monitor,drop.monitor,drop.chain))==0, burnin =
	0, sample = 10000, adapt=max(200-burnin, 0), jags = findjags(),
	silent.jags = FALSE, summarise = TRUE, confidence=0.95, plots =
	summarise, psrf.target = 1.05, normalise.mcmc = TRUE,
	check.stochastic = TRUE, thin = runjags.object$thin, keep.jags.files
	= FALSE, tempdir=TRUE, jags.refresh=0.1, batch.jags=silent.jags,
	method=NA, method.options=NA)

results.jags(background.runjags.object=stop("An object produced by a background runjags method must be supplied"))

}

\arguments{

\item{model}{either a relative or absolute path to a textfile (including
the file extension) containing a model in the JAGS language and possibly
monitored variable names, data and/or initial values, or a character
string of the same.  No default.  The model must be started with the
string 'model\{' and ended with '\}' on new lines.  Data must be
similarly started with 'data\{', monitored variables with 'monitor\{',
and initial values as 'inits\{', and all ended with '\}'.  If multiple
models are found, all but the first one are ignored with a warning.
Multiple data blocks and monitor blocks are combined, multiple inits
blocks are used for different chains.  The model block may also contain
automatically generated data and initial values variables using '#data#
variable' and '#inits# variable', and more monitored variables using
'#monitor# variable'.  See \code{\link{read.winbugs}} for more
information.  No default.}

\item{monitor}{a character vector of the names of variables to monitor. 
The special node names 'deviance', 'pd', 'pd.i', 'popt' and 'dic' are
used to monitor these model fit diagnostics (see the JAGS user manual
for more information), but with the exception of 'deviance' these
monitored nodes won't appear as variables in the summary statistics or
plots.  Note: multiple chains are required for calculation of 'pd.i',
'pd', 'popt' and 'dic'.  No default.}

\item{data}{a character string in the R dump format (or a named list)
containing the data.  If left as NA, no external data is used in the
model.  Default NA.}

\item{n.chains}{the number of chains to use with the simulation.  More
chains will improve the sensitivity of the convergence diagnostic, but
will cause the simulation to run more slowly (although this may be
improved by using a method such as 'parallel' or 'snow').  The minimum
(and default) number of chains is 2.}

\item{inits}{either a character vector with length equal to the number
of chains the model will be run using, or a list of named lists
representing names and corresponding values of inits for each chain.  If
a vector, each element of the vector must be a character string in the R
dump format representing the initial values for that chain, or NA.  If
not all initialising variables are specified, the unspecified variables
are sampled from the prior distribution by JAGS.  Values left as NA
result in all initial values for that chain being sampled from the prior
distribution.  The special variables '.RNG.seed', '.RNG.name', and
'.RNG.state' are allowed for explicit control over random number
generators in JAGS.  Default NA.}

\item{runjags.object}{the model to be extended - the output of a
run.jags (or autorun.jags or extend.jags etc) function, with class
'runjags'.  No default.}

\item{background.runjags.object}{the output of a run.jags (or
extend.jags) function call using a background JAGS method, with class
'runjags.bginfo'.  No default.}

\item{add.monitor}{a character vector of variables to add to the
monitored variable list.  All previously monitored variables are
automatically included - although see the 'drop.monitor' argument.
Default no additional monitors.}

\item{drop.monitor}{a character vector of previously monitored variables
to remove from the monitored variable list for the extended model.
Default none.}

\item{drop.chain}{a numeric vector of chains to remove from the extended
model. Default none.}

\item{combine}{a logical flag indicating if results from the new JAGS
run should be combined with the previous chains.  Default TRUE if not
adding or removing variables or chains, and FALSE otherwise.}

\item{burnin}{the number of burnin iterations (not sampled) to use
(numeric).  Default 5000 iterations.}

\item{sample}{the number of sampling iterations to use (numeric).
Default 10000 iterations.}

\item{adapt}{advanced option to control the length of the adaptive phase
directly, which is otherwise half the length of the burnin period.
Default is 0, unless burnin is less than 200 in which case 100 adapitve
iterations are used.}

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
variables are specified in the model block using '#inits# variable', the
value for the corresponding named variable is taken from initlist if
present (or the result of datalist(chain.no) if specified as a function
which allows both randomly generated initial values and different values
for each chain), or the parent environment, or finally the global
environment if not found anywhere else.  Ignored if '#inits# variable'
is not used in the model block.  Note: different chains are all given
the same starting values if specified as a named list or taken from any
envirnoment; if different values are desired for each chain initlist
should be specified as a function.  Default NA.}

\item{jags}{the system call or path for activating JAGS.  Default calls
findjags() to attempt to locate JAGS on your system.}

\item{silent.jags}{should the JAGS output be suppressed? (logical)  If
TRUE, no indication of the progress of individual models is supplied.
Default FALSE.}

\item{summarise}{should summary statistics be assessed after the model
has completed?  Default TRUE.}

\item{confidence}{the prob argument to be passed to HPDinterval for
calculation of confidence intervals.  Default 0.95 (95\% confidence
intervals).}

\item{plots}{should traceplots and density plots be produced for each
monitored variable?  If TRUE, the returned list will include elements
'trace' and 'density' which consist of a list of lattice objects
accessible using the plot method for the \code{\link{runjags-class}}. 
The alternative is to use (for example) plot(as.mcmc.list(results)) to
look at the density and traceplots for each variable using the
traditional graphics system. Default follows the summarise argument
(which is required to be TRUE for plots to be produced).}

\item{psrf.target}{the value of the point estimate for the potential
scale reduction factor of the Gelman Rubin statistic below which the
chains are deemed to have converged (must be greater than 1).  Ignored
if check.conv==FALSE.  Default 1.05.}

\item{normalise.mcmc}{the Gelman Rubin statistic is based on the
assumption that the posterior distribution of monitored variables is
roughly normal.  For very skewed posterior distributions, it may help to
log/logit transform the posterior before calculating the Gelman Rubin
statistic.  If normalise.mcmc == TRUE, the normality of the
untransformed and log/logit transformed posteriors are compared for each
monitored variable and the least skewed is used to calculate the Gelman
Rubin statistic.  If FALSE, the data are left untransformed (this may
give problems calculating the statistic in extreme cases).  Ignored if
check.conv==FALSE.  Default TRUE.}

\item{check.stochastic}{non-stochastic monitored variables will cause
errors when calculating the Gelman-Rubin statistic, if
check.stochastic==TRUE then all monitored variables will be checked to
ensure they are stochastic beforehand.  This has a computational cost,
and can be bypassed if check.stochastic==FALSE.  Default TRUE.}

\item{modules}{external modules to be loaded into JAGS.  More than 1
module can be used.  Default none.}

\item{factories}{factory modules to be loaded into JAGS.  More than 1
factory can be used.  Factories should be in the format
'<facname>(<factype>)', for example:
factories='mix::TemperedMix(sampler)'.  Default none.}

\item{thin}{the thinning interval to be used in JAGS.  Increasing the
thinning interval may reduce autocorrelation, and therefore reduce the
number of samples required, but will increase the time required to run
the simulation.  Default 1.}

\item{monitor.deviance}{this argument is deprecated and remains for
backwards compatibility only.  See the 'monitor' variable.}
\item{monitor.pd}{this argument is deprecated and remains for backwards
compatibility only.  See the 'monitor' variable.}
\item{monitor.pd.i}{this argument is deprecated and remains for
backwards compatibility only.  See the 'monitor' variable.}
\item{monitor.popt}{this argument is deprecated and remains for
backwards compatibility only.  See the 'monitor' variable.}
\item{check.conv}{this argument is deprecated and remains for backwards
compatibility only.  See the 'summarise' variable.}

\item{keep.jags.files}{option to keep the folder with files needed to
call JAGS, rather than deleting it.  May be useful for attempting to bug
fix models.  A character string can also provided, in which case this
folder name will be used instead of the default (existing folders will
NOT be over-written).  Default FALSE.}

\item{tempdir}{option to use the temporary directory as specified by the
system rather than creating files in the working directory.  If
keep.jags.files==TRUE then the folder is copied to the working directory
after the job has finished (with a unique folder name based on
'runjagsfiles').  Any files created in the temporary directory are
removed when the function exits for any reason.  Default TRUE.}

\item{jags.refresh}{the refresh interval (in seconds) for monitoring
JAGS output using the 'interactive' and 'parallel' methods (see the
'method' argument).  Longer refresh intervals will use less processor
time.  Default 0.1 seconds.}

\item{batch.jags}{option to call JAGS in batch mode, rather than using
input redirection.  On JAGS >= 3.0.0, this suppresses output of the
status which may be useful in some situations.  Default TRUE if
silent.jags is TRUE, or FALSE otherwise.}

\item{method}{the method with which to call JAGS; probably a character
vector specifying one of 'rjags', 'simple', 'interruptible', 'parallel',
'background', 'bgparallel' or 'snow' (and see also
\code{\link{xgrid.autoextend.jags}}). The former runs JAGS using the
rjags package, whereas other options do not require the rjags package
and call JAGS as an external executable.  'simple' runs JAGS as a
foreground process (the default behaviour for runjags < 0.9.6),
'interruptible' allows the JAGS process to be terminated immediately
using the interrupt signal, 'parallel' runs each chain as a separate
process on a separate core, 'snow' uses a simple network of workstations
(which may be passed into the method.options list as 'cl'), and
'background' & 'bgparallel' starts JAGS as one or more background
processes and returns the information needed to be passed to
\code{\link{results.jags}} to retrieve the simulations when they have
finished.  The default for run.jags is to use 'rjags' if the 'rjags'
package is load()ed, or the 'interactive' method otherwise.  The default
for the extend.jags function is to use the same method as used for the
previous JAGS call.

Note that the parallel, snow and bgparallel methods all use separate
JAGS instances to speed up execution of models with multiple chains (at
the expense of using more RAM), but cannot be used to monitor
pd/popt/pd.i (and therefore DIC).  Each chain is specified using a
different random number generator (.RNG.name) for up to 4 chains (the
number of different RNG available in JAGS), unless .RNG.name is
specified in the initial values. Because each chain uses a separate JAGS
instance, JAGS has no way of ensuring independence between multiple
chains using the same random number generator (as would normally be done
when calling a single JAGS instance with multiple chains).  Using more
than 4 chains with one of these methods without the use of new RNG
factories may therefore produce dependence between chains, and is not
recommended (a warning is given if trying to do so).

Starting with runjags version 1.0.0, there has been an attempt to
abstract the method used to call JAGS - as a result a user-specified
function may also be passed as the method.  This function must call JAGS
on a given batch script contained within 'sim' folders in the working
directory, and either wait for JAGS to output simulation results to file
then return TRUE or return FALSE which is assumed to mean that the JAGS
processes are still running, in which case the behaviour is as for
'background'.  The return may also be a list including the named element
'complete' which refers to the same thing, as well as other elements
that are returned to the top level.  If you are interested in developing
another method please feel free to contact the package author.

}

\item{method.options}{an optional named list of argument to be passed to
the method function (including a user specified method function). Of the
default arguments, only 'nsims' indicating the number of separate
simulations (for parallel, snow and bgparallel methods) and 'cl'
specifying an existing snow cluster and/or 'remote.jags' specifying the
path to JAGS on the remote machines (for the snow method only) can be
used.  Others are ignored with a warning.}


}



\value{Usually an object of class 'runjags', or an object of class
'runjags.bginfo' for background methods (see
\code{\link{runjags-class}}).}



\seealso{

\code{\link{runjags-class}}

\code{\link{autorun.jags}},

\code{\link{xgrid.run.jags}},

\code{\link{combine.mcmc}},

\code{\link{testjags}},

\code{\link{dump.format}}

}



\author{Matthew Denwood \email{matthew.denwood@glasgow.ac.uk}}



\examples{

# run a model to calculate the intercept and slope of the expression 
# y = m x + c, assuming normal observation errors for y:

\dontrun{

# Simulate the data
X <- 1:100
Y <- rnorm(length(X), 2*X + 10, 1)

# Model in the JAGS format
model <- "model { 
for(i in 1 : N){ 
Y[i] ~ dnorm(true.y[i], precision); 
true.y[i] <- (m * X[i]) + c;
} 
m ~ dunif(-1000,1000); 
c ~ dunif(-1000,1000); 
precision ~ dexp(1); 
}"

# Use dump.format to convert the data and initial values files 
# into the R dump format, with explicit control over the random 
# number generator used for each chain (optional): 
data <- dump.format(list(X=X, Y=Y, N=length(X))) 
inits1 <- dump.format(list(m=1, c=1, precision=1,
.RNG.name="base::Super-Duper", .RNG.seed=1)) 
inits2 <- dump.format(list(m=0.1, c=10, precision=1,
.RNG.name="base::Wichmann-Hill", .RNG.seed=2))

# Run the model and produce plots 
results <- run.jags(model=model, monitor=c("m", "c", "precision"), 
data=data, n.chains=2, inits=c(inits1,inits2), plots = TRUE)

# Plot the monitored variables:
plot(results)

# Look at the summary statistics:
print(results)

# Extract only the coefficient as an mcmc.list object:
coeff <- as.mcmc.list(results,vars="m")

}


# The same model but using embedded shortcuts to specify data, inits and monitors:

\dontrun{

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

results <- run.jags(model, n.chains=2, initlist=initfunction)

# Look at a traceplot of the intercept and slope on a 2x1 grid:
plot(results,type="trace",vars=c("m","^c"),layout=c(2,1))

}


}



\keyword{models}