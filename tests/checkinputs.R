# Test some runjags inputs with the rjags method:

library(runjags)

# Required for nvar etc:
library(coda)

if(!require(rjags)) stop("The rjags library is required to run this check")

runjags.options(inits.warning=FALSE, rng.warning=FALSE)

load.runjagsmodule()

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

initfunction <- function(chain) return(switch(chain, "1"=list(m=-10), "2"=list(m=10)))
initfunction2 <- function() return(switch(sample(c(1,2),1), "1"=list(m=-10), "2"=list(m=10)))

datalist <- list(X=X, Y=Y, N=N)

results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=initfunction, method='rjags')
results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=initfunction2, method='rjags')
results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=lapply(1:2,initfunction), method='rjags')
results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=initfunction(1), method='rjags')

results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, data=datalist, inits=initfunction, method='rjags')
results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, data=datalist, inits=initfunction2, method='rjags')
results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, data=datalist, inits=lapply(1:2,initfunction), method='rjags')
results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, data=datalist, inits=initfunction(1), method='rjags')


results <- extend.jags(results,sample=1000)
stopifnot(niter(as.mcmc.list(results))==2000)
stopifnot(nvar(as.mcmc.list(results))==3)

results2 <- extend.jags(results, drop.chain=1, summarise=FALSE)
stopifnot(nchain(as.mcmc.list(results2))==1)
stopifnot(list.format(results$end.state[[2]])$.RNG.name == list.format(results2$end.state[[1]])$.RNG.name)

results2 <- extend.jags(results, drop.monitor="precision", summarise=FALSE)
stopifnot(nvar(as.mcmc.list(results2))==2)

results2 <- extend.jags(results, add.monitor="true.y", summarise=FALSE)
stopifnot(nvar(as.mcmc.list(results2))==(3+N))

cat("All input checks passed\n")
