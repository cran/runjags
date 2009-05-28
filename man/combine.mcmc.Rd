\name{combine.mcmc}
\alias{combine.mcmc}
\alias{combine.MCMC}
\title{Combine Two or More MCMC Objects With the Same Number of Chains Into One Longer MCMC Object}
\description{
   Allows an MCMC object (with 1 or more chains) to be combined with another (or several other) MCMC object(s) representing extensions of the same simulation, to produce one MCMC object that contains the continuous combined Markov chains of the other MCMC objects.  Also provides a safe way to thin a single MCMC object or list.
}
\usage{
combine.mcmc(mcmc.objects=list(), thin=1, return.samples=NA, collapse.chains=FALSE)
}
\arguments{
   \item{mcmc.objects}{a list of MCMC objects, all with the same number of chains, or a single MCMC object or list.  No default.}
   \item{thin}{an integer to use to thin the (final) MCMC object by, in addition to any thinning already applied to the objects before being passed to combine.mcmc.  Ignored if return.samples is specified (!=NA).  Default 1 (no additional thinning is performed).}
   \item{return.samples}{the number of samples to return after thinning.  The chains will be thinned to as close to this minimum value as possible.  Supersedes thin if both are specified.  Ignored if niter(mcmc.objects) < return.samples.  Default NA.}
   \item{collapse.chains}{option to combine all MCMC chains into a single MCMC chain with more iterations.  Can be used for combining chains prior to calculating results in order to reduce the Monte Carlo error of estimates.  Default FALSE.}
}
\value{A single MCMC object of length equal to the sum of the lengths of the input MCMC objects (if thin=1)
}

\seealso{
   \code{\link{run.jags}},
   \code{\link{testjags}}
}

\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk} funded as part of the DEFRA VTRI project 0101.}

\examples{

# run a model, then extend the simulation and combine the two MCMC objects, with thinning to 5000 samples.

\dontrun{

# Model in the JAGS format
model <- "model {
for(i in 1 : N){ #data# N
Y[i] ~ dnorm(true.y[i], precision); #data# Y
true.y[i] <- (m * X[i]) + c; #data# X
}
m ~ dunif(-1000,1000);
c ~ dunif(-1000,1000);
precision ~ dexp(1);
#monitor# m, c, precision
}"

# Simulate the data
X <- 1:100
Y <- rnorm(length(X), 2*X + 10, 1)
N <- length(X)

results1 <- run.jagsfile(model, n.chains=2, burnin=5000, sample=10000)
results2 <- run.jagsfile(model, inits=results1$end.state, burnin=0, sample=10000)

results <- combine.mcmc(list(results1$mcmc, results2$mcmc), return.samples=5000)

# Analyse the results
summary(results)
}
}

\keyword{methods}