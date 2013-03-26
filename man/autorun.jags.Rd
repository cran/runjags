\name{autorun.jags}

\alias{autorun.jags}
\alias{autorun.JAGS}
\alias{autoextend.jags}
\alias{autoextend.JAGS}

\title{Run or Extend a User Specified Bayesian MCMC Model in JAGS with Automatically Calculated Run Length and Convergence Diagnostics}
\description{
   Runs or extends a user specified JAGS (similar to WinBUGS) model from 
   within R, returning an object of class \code{\link{runjags-class}}.  
   Chain convergence over the first run of the simulation is
   assessed using the Gelman and Rubin's convergence diagnostic.  If
   necessary, the simulation is extended to improve chain convergence
   (up to a user-specified maximum time limit), before the required
   sample size of the Markov chain is calculated using Raftery and
   Lewis's diagnostic.  The simulation is extended to the required
   sample size dependant on autocorrelation and the number of chains.

   This function is provided primarily for automated running of large
   simulated data studies, and is not a replacement for manually
   assessing convergence and Monte Carlo error when parameter estimates
   are being made from real data.  For more complex models, the use of
   \code{\link{run.jags}} directly with manual assessment of necessary
   run length may be preferable.

Requires Just Another Gibbs Sampler (JAGS), see
\url{http://www-fis.iarc.fr/~martyn/software/jags/}.

}

\usage{

autorun.jags(model=stop("No model supplied"), monitor = NA, data=NA,
	n.chains=NA, inits = NA, startburnin = 5000, startsample = 10000,
	datalist=NA, initlist=NA, psrf.target = 1.05, normalise.mcmc = TRUE,
	check.stochastic = TRUE, modules=c(""), factories=c(""),
	raftery.options = list(), crash.retry=1, summarise = TRUE,
	confidence=0.95, plots = summarise, thin.sample = FALSE, jags =
	findjags(), silent.jags = FALSE, interactive=FALSE, max.time=Inf,
	adaptive=list(type="burnin", length=200), thin = 1, monitor.deviance
	= FALSE, monitor.pd = FALSE, tempdir=TRUE, jags.refresh=0.1,
	batch.jags=silent.jags, method=if ('rjags' \%in\% .packages())
	'rjags' else 'interruptible', method.options=list())

autoextend.jags(runjags.object=stop("The output of a runjags function (with class 'runjags') must be supplied"), 
    add.monitor=character(0), drop.monitor=character(0), drop.chain=numeric(0),
	combine=length(c(add.monitor,drop.monitor,drop.chain))==0,
	startburnin = 0, startsample = 10000, psrf.target = 1.05,
	normalise.mcmc = TRUE, check.stochastic = TRUE, raftery.options =
	list(), crash.retry=1, summarise = TRUE, confidence=0.95, plots =
	summarise, thin.sample = FALSE, jags = findjags(), silent.jags =
	FALSE, interactive=FALSE, max.time=Inf, adaptive=list(type='burnin', length=200), 
	thin = runjags.object$thin, tempdir=TRUE,
	jags.refresh=0.1, batch.jags=silent.jags, method=NA,
	method.options=NA)

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
'#monitor# variable'.  See \code{\link{read.winbugs}} for more details. 
No default.}

\item{monitor}{a character vector of the names of variables to monitor. 
The special node names 'deviance', 'pd', 'pd.i', 'popt' and 'dic' are
used to monitor these model fit diagnostics (see the JAGS user manual
for more information), but with the exception of 'deviance' these
monitored nodes won't appear as variables in the summary statistics or
plots.  Note: multiple chains are required for calculation of 'pd.i',
'pd', 'popt' and 'dic'.}

\item{data}{either a named list or a character string in the R dump
format containing the data.  If left as NA, the model will be run
without external data.}

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

\item{startburnin}{the number of burnin iterations. Default 0.}

\item{startsample}{the total number of samples (including the chains
supplied in runjags.object for autoextend.jags) on which to assess
convergence.  If the runjags.object already contains this number of
samples then convergence will be assessed on this object, otherwise the
required number of additional samples will be obtained before combining
the chains with the old chains. More samples will give a better chance
of allowing the chain to converge, but will take longer to achieve. 
Also controls the length of the pilot chain used to assess the required
sampling length.  The minimum is 4000 samples, which is the minimum
required number of samples for a model with no autocorrelation and good
convergence.  Default 10000 iterations.}

\item{datalist}{an optional named list containing variables used as
data, or alternatively a function (with no arguments) that returns a
named list.  If any variables are specified in the model block using
'#data# <variable>', the value for the corresponding named variable is
taken from datalist if present (or the result of datalist() if specified
as a function which is useful for specifying randomly generated data),
or the parent environment, or finally the global environment if not
found anywhere else.  Ignored if '#data# <variable>' is not used in the
model block.  Default NA.}

\item{initlist}{an optional named list containing variables used as
initial values, or alternatively a function (with a single argument
representing the chain number) that returns a named list.  If any
variables are specified in the model block using '#inits# <variable>',
the value for the corresponding named variable is taken from initlist if
present (or the result of datalist(chain.no) if specified as a function
which allows both randomly generated initial values and different values
for each chain), or the parent environment, or finally the global
environment if not found anywhere else.  Ignored if '#inits# <variable>'
is not used in the model block.  Note: different chains are all given
the same starting values if specified as a named list or taken from any
envirnoment; if different values are desired for each chain initlist
should be specified as a function.  Default NA.}

\item{psrf.target}{the value of the point estimate for the potential
scale reduction factor of the Gelman Rubin statistic below which the
chains are deemed to have converged (must be greater than 1).  Default
1.05.}

\item{normalise.mcmc}{the Gelman Rubin statistic is based on the
assumption that the posterior distribution of monitored variables is
roughly normal.  For very skewed posterior distributions, it may help to
log/logit transform the posterior before calculating the Gelman Rubin
statistic.  If normalise.mcmc == TRUE, the normality of the
untransformed and log/logit transformed posteriors are compared for each
monitored variable and the least skewed is used to calculate the Gelman
Rubin statistic (this may take some time for large numbers of monitored
variables).  If FALSE, the data are left untransformed (this may give
problems calculating the statistic in extreme cases).  Default TRUE.}

\item{check.stochastic}{non-stochastic monitored variables will cause
errors when calculating the Gelman-Rubin statistic, if
check.stochastic==TRUE then all monitored variables will be checked to
ensure they are stochastic beforehand.  This has a small computational
cost, which can be avoided by specifying check.stochastic==FALSE. 
Default TRUE.}

\item{modules}{a character vector of external modules to be loaded into
JAGS.  More than 1 module can be used.  Default none.}

\item{factories}{a character vector of factory modules to be loaded into
JAGS.  More than 1 factory can be used.  Factories should be provided in
the format '<facname>(<factype>)', for example:
factories='mix::TemperedMix(sampler)'.  Also ensure that any required
modules are also specified (in this case 'mix', for example).  Default
none.}

\item{raftery.options}{a named list which is passed as additional
arguments to \code{\link[coda]{raftery.diag}}.  Default none (default
arguments to raftery.diag are used).}

\item{crash.retry}{the number of times to re-attempt a simulation if the
model returns an error.  Default 1 retry (simulation will be aborted
after the second crash).}

\item{summarise}{should summary statistics be automatically calculated
for the output chains?  Default TRUE.}

\item{confidence}{the prob argument to be passed to HPDinterval for
calculation of confidence intervals.  Default 0.95 (95\% confidence
intervals).}

\item{plots}{should traceplots and density plots be produced for each
monitored variable?  If TRUE, the object of class 'runjags' returned
will include elements 'trace' and 'density' which consist of a list of
lattice objects, with a specific print function that can also be
accessed using plot(results).  The alternative is to use
plot(as.mcmc.list(results)) to look at the density and traceplots for
each variable using the traditional graphics system. See also
\code{\link{runjags-class}}. Default TRUE.}

\item{thin.sample}{option to thin the final MCMC chain(s) before
calculating summary statistics and returning the chains.  Thinning very
long chains allows summary statistics to be calculated more quickly.  If
TRUE, the chain is thinned to as close to a minimum of startsample
iterations as possible (i.e. using a thinning interval of
floor(chain.length/thin.sample) since the value must be an integer) and
any excess iterations discarded to ensure the chain length matches
thin.sample.  If FALSE the chains are not thinned.  A positive integer
can also be specified as the desired chain length after thinning; the
chains will be thinned to as close to this minimum value as possible.
Default TRUE (thinned chains of length startsample returned).  This
option does NOT carry out thinning in JAGS, therefore R must have enough
available memory to hold the chains BEFORE thinning.  To avoid this
problem use the 'thin' option instead.}

\item{jags}{the system call or path for activating JAGS.  Default calls
findjags() to attempt to locate JAGS on your system.}

\item{silent.jags}{should the JAGS output be suppressed? (logical)  If
TRUE, no indication of the progress of individual models is supplied.
Default FALSE.}

\item{interactive}{option to allow the simulation to be interactive, in
which case the user is asked if the simulation should be extended when
run length and convergence calculations are performed and the extended
simulation will take more than 1 minute.  The function will wait for a
response before extending the simulations.  If FALSE, the simulation
will be run until the chains have converged or until the next extension
would extend the simulation beyond 'max.time'.  Default FALSE.}

\item{max.time}{the maximum time for which the function is allowed to
extend the chains to improve convergence, as a character string
including units or as an integer in which case units are taken as
seconds.  Ignored if interactive==TRUE.  If the function thinks that the
next simulation extension to improve convergence will result in a total
time of greater than max.time, the extension is aborted.  The time per
iteration is estimated from the first simulation.  Acceptable units
include 'seconds', 'minutes', 'hours', 'days', 'weeks', or the first
letter(s) of each.  Default "1hr".}

\item{adaptive}{a list of advanced options controlling the length of the
adaptive mode of each simulation.  Extended simulations do not require
an adaptive phase, but JAGS prints a warning if one is not performed.
Reduce the length of the adpative phase for very time consuming models.
'type' must be one of 'adaptive' or 'burnin'.}

\item{thin}{the thinning interval to be used in JAGS.  Increasing the
thinning interval may reduce autocorrelation, and therefore reduce the
number of samples required, but will increase the time required to run
the simulation.  Using this option thinning is performed directly in
JAGS, rather than on an existing MCMC object as with thin.sample.
Default 1.}

\item{monitor.deviance}{this argument is deprecated and remains for
backwards compatibility only.  See the 'monitor' variable.}
\item{monitor.pd}{this argument is deprecated and remains for backwards
compatibility only.  See the 'monitor' variable.}

\item{tempdir}{option to use the temporary directory as specified by the
system rather than creating files in the working directory.  Any files
created in the temporary directory are removed when the function exits
for any reason.  Default TRUE.}

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
or 'snow' (and see also \code{\link{xgrid.autoextend.jags}}). The former runs JAGS using the
rjags package, whereas other options do not require the rjags package
and call JAGS as an external executable.  'simple' runs JAGS as a
foreground process (the default behaviour for runjags < 0.9.6),
'interruptible' allows the JAGS process to be terminated immediately
using the interrupt signal, 'parallel' runs each chain as a separate
process on a separate core, and 'snow' uses a simple network of workstations
(which may be passed into the method.options list as 'cl').  The default for 
autorun.jags is to use 'rjags' if the 'rjags' package is load()ed, or the 
'interactive' method otherwise.  The default for the autoextend.jags function is
to use the same method as used for the previous JAGS call.

Note that the parallel, snow and bgparallel methods all use separate
JAGS instances to speed up execution of models with multiple chains (at
the expense of using more RAM), but cannot be used to monitor
pd/popt/pd.i (and therefore DIC).  If no .RNG.name is specified in the
initial values, each chain is assigned a different random number
generator (.RNG.name) for up to 4 chains (the number of different RNG
available in JAGS), or using the lecuyer module for 5 or more chains
(requires rjags to be installed).

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



\value{an object of class 'runjags' (see \code{\link{runjags-class}}).}



\seealso{

\code{\link{run.jags}},

\code{\link{read.winbugs}},

\code{\link{xgrid.autoextend.jags}},

\code{\link{runjags-class}}

}



\author{Matthew Denwood \email{matthew.denwood@glasgow.ac.uk}}



\examples{
# run a model to calculate the intercept and slope of the expression 
# y = m x + c, assuming normal observation errors for y:

\dontrun{
# Simulate the data
N <- 100
X <- 1:N
Y <- rnorm(N, 2*X + 10, 1)

# Model in the JAGS format
model <- "model {
for(i in 1 : N){
Y[i] ~ dnorm(true.y[i], precision);
true.y[i] <- (m * X[i]) + c;
}
m ~ dunif(-1000,1000);
c ~ dunif(-1000,1000);
precision ~ dexp(1);

#data# N, X, Y
}"


# Run the model
results <- autorun.jags(model=model, monitor=c("m", "c", "precision"))

# Analyse traceplots of the results to assess convergence:
plot(results, type="trace", layout=c(3,1))

# Summary of monitored variables:
results

}
}

\keyword{models}