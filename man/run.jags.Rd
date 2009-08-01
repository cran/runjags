\name{run.jags}
\alias{run.jags}
\alias{run.JAGS}
\title{Run a User Specified Bayesian MCMC Model in JAGS from Within R}
\description{
   Runs a user specified JAGS (similar to WinBUGS) model from within R, returning a list of the MCMC chain(s) along with convergence diagnostics, autocorrelation diagnostics and monitored variable summaries.  Data and initial values must be supplied in the R dump format (see dump.format() for an easy way to do this).  A character vector of variables to monitor must also be supplied.  

Requires Just Another Gibbs Sampler (JAGS), see http://www-fis.iarc.fr/~martyn/software/jags/.  The GUI interface for R in Windows may not continually refresh the output window, making it difficult to track the progress of the simulation (if silent.jags is FALSE).  To avoid this, you can run the function from the terminal version of R (located in the Program Files/R/bin/ folder).
}
\usage{
run.jags(model=stop("No model supplied"), 
   monitor = stop("No monitored variables supplied"),
   data = NA, n.chains = 2, inits = replicate(n.chains, NA),
   burnin = 5000*thin, sample = 10000*thin, 
   adapt=if(burnin<200) 100 else 0, jags=findjags(), 
   silent.jags = FALSE, check.conv = TRUE, plots = TRUE,
   psrf.target = 1.05, normalise.mcmc = TRUE, 
   check.stochastic = TRUE, modules=c(""), thin = 1, 
   monitor.deviance = FALSE, monitor.pd = FALSE, 
   monitor.popt = FALSE, keep.jags.files = FALSE)}

\arguments{
   \item{model}{a character string of the model in the JAGS language.  No default.}
   \item{monitor}{a character vector of the names of variables to monitor.  For all models, specifying 'deviance' as a monitored variable will calculate the model deviance.  No default.}
   \item{data}{a character string in the R dump format (or a named list) containing the data.  If left as NA, no external data is used in the model.  Default NA.}
   \item{n.chains}{the number of chains to use for the simulation.  Must be a positive integer.  Default 2.}
   \item{inits}{a character vector with length equal to the number of chains the model will be run using.  Each element of the vector must be a character string in the R dump format representing the initial values for that chain, or NA.  If not all initialisable variables are specified, the unspecified variables are sampled from the prior distribution by JAGS.  Values left as NA result in all initial values for that chain being sampled from the prior distribution.  Default NA.}
   \item{burnin}{the number of burnin iterations (not sampled) to use (numeric).  Default 5000 iterations.}
   \item{sample}{the number of sampling iterations to use (numeric).  Default 10000 iterations.}
   \item{adapt}{advanced option to control the length of the adaptive phase directly, which is otherwise half the length of the burnin period.  Default is 0, unless burnin is less than 200 in which case 100 adapitve iterations are used.}
   \item{jags}{the system call or path for activating JAGS.  Default calls findjags() to attempt to locate JAGS on your system.}
   \item{silent.jags}{should the JAGS output be suppressed? (logical)  If TRUE, no indication of the progress of individual models is supplied.  Default FALSE.}
   \item{check.conv}{should the convergence be assessed after the model has completed?  If TRUE, each monitored variable will be assessed for a potential scale reduction factor of the Gelman Rubin statistic of less than 1.05, which indicates adequate convergence.  2 or more chains are required to assess convergence.  Default TRUE.}
   \item{plots}{should traceplots and density plots be produced for each monitored variable?  If TRUE, the returned list will include elements 'trace' and 'density' which consist of a list of lattice objects.  The alternative is to use plot(results\$mcmc) to look at the density and traceplots for each variable using the traditional graphics system.  Default TRUE.}
   \item{psrf.target}{the value of the point estimate for the potential scale reduction factor of the Gelman Rubin statistic below which the chains are deemed to have converged (must be greater than 1).  Ignored if check.conv==FALSE.  Default 1.05.}
   \item{normalise.mcmc}{the Gelman Rubin statistic is based on the assumption that the posterior distribution of monitored variables is roughly normal.  For very skewed posterior distributions, it may help to log/logit transform the posterior before calculating the Gelman Rubin statistic.  If normalise.mcmc == TRUE, the normality of the untransformed and log/logit transformed posteriors are compared for each monitored variable and the least skewed is used to calculate the Gelman Rubin statistic.  If FALSE, the data are left untransformed (this may give problems calculating the statistic in extreme cases).  Ignored if check.conv==FALSE.  Default TRUE.}
   \item{check.stochastic}{non-stochastic monitored variables will cause errors when calculating the Gelman-Rubin statistic, if check.stochastic==TRUE then all monitored variables will be checked to ensure they are stochastic beforehand.  This has a computational cost, and can be bypassed if check.stochastic==FALSE.  Default TRUE.}
   \item{modules}{external modules to be loaded into JAGS.  More than 1 module can be used.  Default none.}
   \item{thin}{the thinning interval to be used in JAGS.  Increasing the thinning interval may reduce autocorrelation, and therefore reduce the number of samples required, but will increase the time required to run the simulation.  Default 1.}
   \item{monitor.deviance}{option to monitor the deviance of each monitored variable using the DIC module for JAGS.  If TRUE, a 'deviance' element is returned representing this value for each monitored variable at each iteration.  For more information see the JAGS user manual section 4.4.  Default FALSE.}
   \item{monitor.pd}{option to monitor the effective number of parameters using the DIC module for JAGS.  If TRUE, a 'pd' element is returned representing this value at each iteration.  For more information see the JAGS user manual section 4.4.  Default FALSE.}
   \item{monitor.popt}{option to monitor the optimism of the expected deviance using the DIC module for JAGS.  If TRUE, a 'popt' element is returned representing this value at each iteration.  For more information see the JAGS user manual section 4.4.  Default FALSE.}
   \item{keep.jags.files}{option to keep the folder with files needed to call JAGS, rather than deleting it.  Could be useful for attempting to bug fix models.  Default FALSE.}
}

\value{The results of the simulation are returned as list including the following items (omitting the last 3 items if check.conv==FALSE):
   \item{mcmc}{an MCMC list of MCMC objects representing the chains}
   \item{end.state}{a character vector of length equal to the number of chains representing a description of the model state in the R dump format for each chain at the last iteration.  This can be used as an initial values vector to restart a new simulation in the same place as the previous simulation ended.}
   \item{burnin}{number of burnin iterations discarded before sampling.}
   \item{sample}{number of sampled iterations.}
   \item{thin}{the thinning interval in JAGS used for the chains.}
   \item{summary}{the summary of each monitored variable from the combined chains, equivalent to summary(combine.mcmc(mcmc, collapse.chains=TRUE)).}
   \item{psrf}{the output of the Gelman Rubin dignostic (similar [but not exactly equivalent] to gelman.diag(mcmc)).}
   \item{autocorr}{the output of the autocorrelation diagnostic (equivalent to autocorr.diag(mcmc)).}
   \item{trace}{a list of lattice objects representing traceplots for each monitored variable.  Calling each individual element will result in the traceplot for that variable being shown, calling the entire list will result in all traceplots being shown in new windows (which may cause problems with your R session if there are several monitored variables).  Not produced if plots==FALSE.}
   \item{density}{a list of lattice objects representing density plots for each monitored variable.  Calling each individual element will result in the  density plot for that variable being shown, calling the entire list will result in all density plots being shown in new windows (which may cause problems with your R session if there are several monitored variables).  Not produced if plots==FALSE.}

}

\seealso{
   \code{\link{autorun.jags}},
   \code{\link{run.jagsfile}},
   \code{\link{combine.mcmc}},
   \code{\link{testjags}},
   \code{\link{dump.format}},
   \code{\link[coda]{summary.mcmc}},
   \code{\link[coda]{gelman.diag}},
   \code{\link[coda]{autocorr.diag}}
}

\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk} funded as part of the DEFRA VTRI project 0101.}

\examples{
# run a model to calculate the intercept and slope of the expression y = m x + c, assuming normal observation errors for y:

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

# Use dump.format to convert the data and initial values files into the R dump format
data <- dump.format(list(X=X, Y=Y, N=length(X)))
inits1 <- dump.format(list(m=1, c=1, precision=1))
inits2 <- dump.format(list(m=0.1, c=10, precision=1))

# Run the model and produce plots
results <- run.jags(model=model, monitor=c("m", "c", "precision"), data=data, n.chains=2, inits=c(inits1,
inits2), plots = TRUE)

# Density plots of the monitored variables:
results$density

# Analyse the results
results$summary
}

}
\keyword{models}