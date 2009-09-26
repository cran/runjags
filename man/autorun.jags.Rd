\name{autorun.jags}
\alias{autorun.jags}
\alias{autorun.JAGS}
\title{Run a User Specified Bayesian MCMC Model in JAGS with Automatically Calculated Run Length and Convergence Diagnostics}
\description{
   Runs a user specified JAGS (similar to WinBUGS) model from within R, returning a list of the MCMC chain(s) along with convergence diagnostics, autocorrelation diagnostics and monitored variable summaries.  Chain convergence over the first run of the simulation is assessed using the Gelman and Rubin's convergence diagnostic.  If necessary, the simulation is extended to improve chain convergence (up to a user-specified maximum time limit), before the required sample size of the Markov chain is calculated using the Raftery and Lewis's diagnostic.  The simulation is extended to the required sample size dependant on autocorrelation.  This function is provided as an educational tool only, and is not a replacement for manually assessing convergence and monte carlo error for real-world applications.  JAGS is called using the lower level function run.jags.
}
\usage{
autorun.jags(model=stop("No model supplied"), 
   monitor = stop("No monitored variables supplied"), 
   data=NA, n.chains=2, inits = replicate(n.chains, NA), 
   startburnin = 5000, startsample = 10000,
   psrf.target = 1.05, normalise.mcmc = TRUE,
   crash.retry = 1, thin.sample = FALSE, jags = findjags(), 
   silent.jags = FALSE, interactive=TRUE, max.time=Inf, 
   adaptive=list(type="burnin", length=200))
}
\arguments{
   \item{model}{a character string of the model in the JAGS language.  No default.}
   \item{monitor}{a character vector of the names of variables to monitor.  No default.}
   \item{data}{either a named list or a character string in the R dump format containing the data.  If left as NA, the model will be run without external data.}
   \item{n.chains}{the number of chains to use with the simulation.  More chains will improve the sensitivity of the convergence diagnostic, but will cause the simulation to run more slowly.  The minimum (and default) number of chains is 2.}
   \item{inits}{a character vector with length equal to either n.chains or length 1.  Each element of the vector must be a character string in the R dump format representing the initial values for that chain.  If inits is of length 1, all chains will be run using the same initial values (this is NOT recommended and will cause problems with convergence diagnostics).  If left as NA, or if only a partial list of initial values is supplied, JAGS will sample initial values from the priors for remaining variables in each chain.}
   \item{startburnin}{the number of initial updates to discard before sampling.  Only used on the initial run before checking convergence.  Default 5000 iterations.}
   \item{startsample}{the number of samples on which to assess convergence.  More samples will give a better chance of allowing the chain to converge, but will take longer to achieve.  Also controls the length of the pilot chain used to assess the required sampling length.  The minimum is 4000 samples, which is the minimum required number of samples for a model with no autocorrelation and good convergence.  Default 10000 iterations.}
   \item{psrf.target}{the value of the point estimate for the potential scale reduction factor of the Gelman Rubin statistic below which the chains are deemed to have converged (must be greater than 1).  Default 1.05.}
   \item{normalise.mcmc}{the Gelman Rubin statistic is based on the assumption that the posterior distribution of monitored variables is roughly normal.  For very skewed posterior distributions, it may help to log/logit transform the posterior before calculating the Gelman Rubin statistic.  If normalise.mcmc == TRUE, the normality of the untransformed and log/logit transformed posteriors are compared for each monitored variable and the least skewed is used to calculate the Gelman Rubin statistic.  If FALSE, the data are left untransformed (this may give problems calculating the statistic in extreme cases).  Default TRUE.}
   \item{crash.retry}{the number of times to re-attempt a simulation if the model returns an error.  Default 1 retry (simulation will be aborted after the second crash).}
   \item{thin.sample}{option to thin the final MCMC chain(s) before calculating summary statistics and returning the chains.  Thinning heavily autocorrelated, very long chains allows summary statistics to be calculated more quickly.  If TRUE, the chain is thinned to as close to a minimum of startsample iterations as possible, if FALSE the chains are not thinned.  A positive integer can also be specified as the desired chain length after thinning; the chains will be thinned to as close to this minimum value as possible.  Default FALSE (no thinning performed).}
   \item{jags}{the system call or path for activating JAGS.  Default calls findjags() to attempt to locate JAGS on your system.}
   \item{silent.jags}{should the JAGS output be suppressed? (logical)  If TRUE, no indication of the progress of individual models is supplied.  Default FALSE.}
   \item{interactive}{option to allow the simulation to be interactive, in which case the user is asked if the simulation should be extended when run length and convergence calculations are performed and the extended simulation will take more than 1 minute.  The function will wait for a response before extending the simulations.  If FALSE, the simulation will be run until the chains have converged or until the next extension would extend the simulation beyond 'max.time'.  Default FALSE.}
   \item{max.time}{the maximum time the function is allowed to run for, as a character string including units or as an integer in which case units are taken as seconds.  Ignored if interactive==TRUE.  If the function thinks that the next simulation extension will result in a total time of greater than max.time, the extension is aborted.  The time per iteration is estimated from the pilot chain.  Acceptable units include 'seconds', 'minutes', 'hours', 'days', 'weeks', or the first letter(s) of each.  Default "1hr".}
   \item{adaptive}{a list of advanced options controlling the length of the adaptive mode of each simulation.  Extended simulations do not require an adaptive phase, but JAGS prints a warning if one is not performed.  Reduce the length of the adpative phase for very time consuming models.  'type' must be one of 'adaptive' or 'burnin'.}
}

\value{A list including the following elements:
   \item{mcmc}{an MCMC object representing the model output for all chain(s).  Renamed pilot.mcmc if the simulation is aborted before convergence or before the required sampling length is achieved}
   \item{end.state}{the end state of the last simulation extension performed.  Can be used as initial values to extend the simulation further if required}
   \item{req.samples}{the minimum sample size required for the Markov chain as calculated using the Raftery and Lewis's diagnostic}
   \item{req.burnin}{the minimum burnin period for the chains to stabilise as calculated using the Raftery and Lewis's diagnostic}
   \item{samples.to.conv}{the number of sampled iterations discarded due to poor convergence.  The total number of iterations performed for the simulation is equal to req.samples + req.burnin + samples.to.conv (unless the simulation was aborted before reaching the required sampling length)}
   \item{summary}{the summary statistics for the monitored variables.  Renamed pilot.summary if pilot.only==TRUE or if the simulation is aborted before the required sampling length is achieved}
   \item{psrf}{the Gelman Rubin statistic for the monitored variables (similar to output of gelman.diag())}
   \item{autocorr}{the autocorrelation diagnostic for the monitored variables (output of autocorr.diag())}
}

\seealso{
   \code{\link{run.jags}}
   \code{\link{autorun.jagsfile}}
   \code{\link[coda]{raftery.diag}}
   \code{\link[coda]{gelman.diag}}
   \code{\link[coda]{autocorr.diag}}
}

\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk} funded as part of the DEFRA VTRI project 0101.}

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
results <- run.jags(model=model, monitor=c("m", "c", "precision"), data=data)

# Analyse the results
results$summary
}
}

\keyword{models}