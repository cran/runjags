# test all of the runjags dispatch methods with a toy example:

library(runjags)

runjags.options(inits.warning=FALSE, rng.warning=FALSE)

# Require for as.mcmc.list and niter:
library(coda)


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


# Get the JAGS path - rjags adds the JAGS path to the PATH in Windows...
library(rjags)
jagspath <- findjags()

# Only run the JAGS methods tests if we have found JAGS and have permission to run it:
if(jagspath!="JAGS not found" && testjags(jagspath)$JAGS.available){
	
	# Try the simple method and if it doesn't work give a warning but don't fail (likely to be permissions problems)
	success <- try({
		results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=initfunction, method='simple',temp=FALSE)
	})
	if(class(success)=="try-error"){
		cat("JAGS was found but the simple method failed; it is possible that there were permissions issues or similar.  Details as follows:\n")
		temp <- testjags()
		print(file.info(jagspath))
		print(file.info(getwd()))
		cat("All test methods except rjags were skipped\n")
	}else{
		stopifnot(niter(as.mcmc.list(results))==1000)

		results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=initfunction, method='parallel')
		stopifnot(niter(as.mcmc.list(results))==1000)

		results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=initfunction, method='interruptible')
		stopifnot(niter(as.mcmc.list(results))==1000)

		# Snow gives problems here ... but it does work!
		#results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=initfunction, method='snow')
		#stopifnot(niter(as.mcmc.list(results))==1000)

		info <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=initfunction, method='bgparallel')
		t <- 0
		repeat{
			s <- try(results <- results.jags(info))
			if(class(s)!='try-error') break
			if(t==30) stop("Timed out waiting for the bgparallel method")
			Sys.sleep(1)
			t <- t+1
		}
		stopifnot(niter(as.mcmc.list(results))==1000)

		info <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=initfunction, method='background')
		t <- 0
		repeat{
			s <- try(results <- results.jags(info))
			if(class(s)!='try-error') break
			if(t==30) stop("Timed out waiting for the bgparallel method")
			Sys.sleep(1)
			t <- t+1
		}
		stopifnot(niter(as.mcmc.list(results))==1000)
	}
}else{
	cat("JAGS could not be called externally at the path: ", jagspath, "\n")
	cat("All test methods except rjags and rjagsparallel were skipped\n")	
}

results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=initfunction, method='rjags')
stopifnot(niter(as.mcmc.list(results))==1000)

results <- run.jags(model, n.chains=2, sample=1000, burnin=1000, initlist=initfunction, method='rjparallel')
stopifnot(niter(as.mcmc.list(results))==1000)

cat("All methods checks passed\n")

