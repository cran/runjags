\name{autorun.jags}
\alias{autorun.jags}
\alias{autorun.JAGS}
\title{Run a User Specified Bayesian MCMC Model in JAGS with Automatically Calculated Run Length and Convergence Diagnostics}
\description{
   Runs a user specified JAGS (similar to WinBUGS) model from within R,
   returning a list of the MCMC chain(s) along with convergence
   diagnostics, autocorrelation diagnostics and monitored variable
   summaries.  Chain convergence over the first run of the simulation is
   assessed using the Gelman and Rubin's convergence diagnostic.  If
   necessary, the simulation is extended to improve chain convergence
   (up to a user-specified maximum time limit), before the required
   sample size of the Markov chain is calculated using the Raftery and
   Lewis's diagnostic.  The simulation is extended to the required
   sample size dependant on autocorrelation and the number of chains.

   This function is provided as an educational tool only, and is not a
   replacement for manually assessing convergence and Monte Carlo error
   for real-world applications.  For more complex models, the use of
   run.jags directly with manual assessment of necessary run length is
   recommended.  JAGS is called using the lower level function
   \code{\link{run.jags}}.
}
\usage{
autorun.jags(model=stop("No model supplied"), 
   monitor = stop("No monitored variables supplied"), 
   data=NA, n.chains=2, inits = replicate(n.chains, NA), 
   startburnin = 5000, startsample = 10000,
   psrf.target = 1.05, normalise.mcmc = TRUE,
   check.stochastic = TRUE, raftery.options = list(),
   crash.retry = 1, plots = TRUE, thin.sample = TRUE,
   jags = findjags(), silent.jags = FALSE, 
   interactive=TRUE, max.time=Inf, 
   adaptive=list(type="burnin", length=200), modules=c(""), 
   thin = 1, monitor.deviance = FALSE, monitor.pd = FALSE, 
   monitor.popt = FALSE, keep.jags.files = FALSE, 
   tempdir=TRUE, method=if(.Platform$OS.type=='unix' & 
   .Platform$GUI!="AQUA") 'interruptible' else 'simple')
}
\arguments{

\item{model}{a character string of the model in the JAGS language.  No
default.}

\item{monitor}{a character vector of the names of variables to monitor. 
For all models, specifying 'deviance' as a monitored variable will
calculate the model deviance.  No default.}

\item{data}{either a named list or a character string in the R dump
format containing the data.  If left as NA, the model will be run
without external data.}

\item{n.chains}{the number of chains to use with the simulation.  More
chains will improve the sensitivity of the convergence diagnostic, but
will cause the simulation to run more slowly.  The minimum (and default)
number of chains is 2.}

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

\item{startburnin}{the number of initial updates to discard before
sampling.  Only used on the initial run before checking convergence. 
Default 5000 iterations.}

\item{startsample}{the number of samples on which to assess convergence.
 More samples will give a better chance of allowing the chain to
converge, but will take longer to achieve.  Also controls the length of
the pilot chain used to assess the required sampling length.  The
minimum is 4000 samples, which is the minimum required number of samples
for a model with no autocorrelation and good convergence.  Default 10000
iterations.}

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
ensure they are stochastic beforehand.  This has a computational cost,
and can be bypassed if check.stochastic==FALSE.  Default TRUE.}

\item{raftery.options}{a named list which is passed as additional
arguments to \code{\link[coda]{raftery.diag}}.  Default none (default
arguments to raftery.diag are used).}

\item{crash.retry}{the number of times to re-attempt a simulation if the
model returns an error.  Default 1 retry (simulation will be aborted
after the second crash).}

\item{plots}{should traceplots and density plots be produced for each
monitored variable?  If TRUE, the returned list will include elements
'trace' and 'density' which consist of a list of lattice objects.  The
alternative is to use plot(results\$mcmc) to look at the density and
traceplots for each variable using the traditional graphics system. 
Default TRUE.}

\item{thin.sample}{option to thin the final MCMC chain(s) before
calculating summary statistics and returning the chains.  Thinning very
long chains allows summary statistics to be calculated more quickly.  If
TRUE, the chain is thinned to as close to a minimum of startsample
iterations as possible (i.e. using a thinning interval of
floor(chain.length/thin.sample) since the value must be an integer).  If
FALSE the chains are not thinned.  A positive integer can also be
specified as the desired chain length after thinning; the chains will be
thinned to as close to this minimum value as possible.  Default TRUE
(thinned chains of length startsample returned).  This option does NOT
carry out thinning in JAGS, therefore R must have enough available
memory to hold the chains BEFORE thinning.  To avoid this problem use
the 'thin' option instead.}

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

\item{modules}{external modules to be loaded into JAGS.  More than 1
module can be used.  Default none.}

\item{thin}{the thinning interval to be used in JAGS.  Increasing the
thinning interval may reduce autocorrelation, and therefore reduce the
number of samples required, but will increase the time required to run
the simulation.  Using this option thinning is performed directly in
JAGS, rather than on an existing MCMC object as with thin.sample. 
Default 1.}

\item{monitor.deviance}{option to monitor the deviance of each monitored
variable using the DIC module for JAGS.  If TRUE, a 'deviance' element
is returned representing this value for each monitored variable at each
iteration.  For more information see the JAGS user manual section 4.4. 
Default FALSE.}

\item{monitor.pd}{option to monitor the effective number of parameters
using the DIC module for JAGS.  If TRUE, a 'pd' element is returned
representing this value at each iteration.  For more information see the
JAGS user manual section 4.4.  Default FALSE.}

\item{monitor.popt}{option to monitor the optimism of the expected
deviance using the DIC module for JAGS.  If TRUE, a 'popt' element is
returned representing this value at each iteration.  For more
information see the JAGS user manual section 4.4.  Default FALSE.}

\item{keep.jags.files}{option to keep the folder with files needed to
call JAGS, rather than deleting it.  May be useful for attempting to bug
fix models.  Since autorun.jags typically makes several calls to JAGS,
all folders are kept - the order in which the folders were used can be
ascertained from the creation dates of the files.  Default FALSE.}

\item{tempdir}{option to use the temporary directory as specified by the
system rather than creating files in the working directory.  If
keep.jags.files==TRUE then the folder is copied to the working directory
after the job has finished (with a unique folder name based on
'runjagsfiles').  Any files created in the temporary directory are
removed when the function exits for any reason.  Default TRUE.}

\item{method}{the method with which to call JAGS; one of 'simple',
'interruptible' or 'parallel'.  The former runs JAGS as a foreground
process (the default behaviour for runjags < 0.9.6), 'interruptible'
allows the JAGS process to be terminated immediately using the interrupt
signal 'control-c' (terminal/console versions of R only), and 'parallel'
runs each chain as a separate process on a separate core (this will
speed up execution of models with multiple tasks at the expense of using
more RAM).  Only the 'simple' method is available for Windows.  On
machines running Mac OS X and with access to an Apple Xgrid cluster, the
method may be a list with an element 'xgrid.method="simple"' (see
\code{\link{xgrid.run.jags}} for more information).  Default
'interruptible' on terminal/console versions of R, or 'simple' on GUI
versions of R.}

}



\value{A list including the following elements:

\item{mcmc}{an MCMC object representing the model output for all
chain(s).  Renamed pilot.mcmc if the simulation is aborted before
convergence or before the required sampling length is achieved}

\item{end.state}{the end state of the last simulation extension
performed.  Can be used as initial values to extend the simulation
further if required}

\item{req.samples}{the minimum sample size required for the Markov chain
as calculated using the Raftery and Lewis's diagnostic.  This sample
size is dependent on the thinning interval in JAGS}

\item{samples.to.conv}{the number of sampled iterations discarded due to
poor convergence.  The total number of iterations performed for the
simulation is equal to req.samples + req.burnin + samples.to.conv
(unless the simulation was aborted before reaching the required sampling
length)}

\item{thin}{the thinning interval in JAGS used for the chains.}

\item{summary}{the summary statistics for the monitored variables from
the combined chains.  Renamed pilot.summary if pilot.only==TRUE or if
the simulation is aborted before the required sampling length is
achieved}

\item{psrf}{the Gelman Rubin statistic for the monitored variables
(similar to output of gelman.diag())}

\item{autocorr}{the autocorrelation diagnostic for the monitored
variables (output of autocorr.diag())}

\item{trace}{a list of lattice objects representing traceplots for each
monitored variable, of class 'plotindpages'.  Calling each individual
element will result in the traceplot for that variable being shown,
calling the entire list will result in all traceplots being shown in new
windows (which may cause problems with your R session if there are
several monitored variables).  To override the individual window
plotting behaviour (to combine plots and/or save the plots to a file),
either change the class of each object using for(i in
1:nvar(results$mcmc)) class(results$trace[[i]]) <- 'trellis' and then
combine using the c.trellis method in the latticeExtra package, or
simply re-generate the plots using the raw mcmc output.  Not produced if
plots==FALSE.}

\item{density}{a list of lattice objects representing density plots for
each monitored variable, of class 'plotindpages'.  Calling each
individual element will result in the  density plot for that variable
being shown, calling the entire list will result in all density plots
being shown in new windows (which may cause problems with your R session
if there are several monitored variables).  To override the individual
window plotting behaviour (to combine plots and/or save the plots to a
file), either change the class of each object using for(i in
1:nvar(results$mcmc)) class(results$density[[i]]) <- 'trellis' and then
combine using the c.trellis method in the latticeExtra package, or
simply re-generate the plots using the raw mcmc output.    Not produced
if plots==FALSE.}

}

\details{This function runs the specified model until the point estimate
of the potential scale reduction factor of the Gelman-Rubin statistic
for each monitored parameter is less than psrf.target (default 1.05). 
This is intended to make sure that the chains have converged before
sampling from them (although this does not guarantee convergence so
manual checking of traceplots is recommended).  If convergence is not
achieved within the time limit, then the function returns pilot.mcmc
rather than mcmc, which will always be of length startsample since it is
the unconverged mcmc object returned from the last run of the model. 
This chain is not suitable for making inference from, so a different
name is used to avoid confusion.  If convergence is achieved, the
Raftery and Lewis's diagnostic is used to calculate the required number
of samples based on the most heavily autocorrelated monitored variable. 
The chains are then extended to increase the sample size of each
variable as necessary (so that sufficient samples are obtained from the
COMBINED chains to satisfy the Raftery and Lewis's diagnostic) before
being summarised and returned.  Heavily autocorrelated models with large
numbers of monitored variables may result in a required sample size
larger than the available memory in R.  If this is the case, try using
the thin option to reduce autocorrelation (and therefore the required
sample size) or monitor less variables.
}

\seealso{
   \code{\link{run.jags}},
   \code{\link{autorun.jagsfile}},
   \code{\link{combine.mcmc}},
   \code{\link[coda]{raftery.diag}},
   \code{\link[coda]{gelman.diag}},
   \code{\link[coda]{autocorr.diag}}
}

\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk}}

\examples{
# run a model to calculate the intercept and slope of the expression y = m x + c, assuming normal observation errors for y:

\dontrun{
# Simulate the data
x <- 1:100
y <- rnorm(length(x), 2*x + 10, 1)

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

# Convert the data to a named list
data <- list(X=x, Y=y, N=length(x))

# Run the model
results <- autorun.jags(model=model, monitor=c("m", "c", "precision"), data=data)

# Analyse traceplots of the results to assess convergence:
results$trace

# Summary of monitored variables:
results$summary
}
}

\keyword{models}