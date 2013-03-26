runjags.summaries <- function(mcmclist, pd, popt, pd.i, monitor, plots, psrf.target, normalise.mcmc, check.stochastic, confidence, silent=FALSE){
	
	n.chains <- nchain(mcmclist)
	
	dic <- "To calculate DIC add 'dic' as a monitored variable"
	if(any(monitor=="dic")){
		meandeviance.chain <- sapply(mcmclist, function(x) return(mean(x[,'deviance'])))
		meandeviance <- mean(combine.mcmc(mcmclist, collapse.chains=TRUE)[,'deviance'])
		if(identical(pd, NA)){
			meanpd <- NA
		}else{
			meanpd <- mean(pd)
		}
		if(!identical(popt, NA)){
			sumpopt <- sum(popt)
			dic.stats <- list(dic=meandeviance+meanpd, dic.chains=meandeviance.chain+meanpd, ped=meandeviance+sumpopt, ped.chains=meandeviance.chain+sumpopt, meandeviance=meandeviance, meandeviance.chains=meandeviance.chain, meanpd=meanpd, sumpopt=sumpopt)
		}else{
			dic.stats <- list(dic=meandeviance+meanpd, dic.chains=meandeviance.chain+meanpd, ped=NA, ped.chains=NA, meandeviance=meandeviance, meandeviance.chains=meandeviance.chain, meanpd=meanpd, sumpopt=NA)			
		}
		class(dic.stats) <- 'dic.stats'
		dic <- dic.stats
	}


	normalisedmcmc <- normalise.mcmcfun(mcmclist, normalise = normalise.mcmc, warn=TRUE, check.stochastic = check.stochastic)
	
	success <- try({
		
		if(n.chains > 1){
			if(!silent) swcat("Calculating the Gelman-Rubin statistic for ", nvar(mcmclist), " variables....\n", sep="")
			convergence <- safe.gelman.diag(normalisedmcmc, transform=FALSE, autoburnin=FALSE)
		
			convergence <- c(convergence, psrf.target=psrf.target)
			class(convergence) <- "gelman.with.target"
		
			n.params <- nrow(convergence$psrf)

		}else{
			warning("Convergence cannot be assessed with only 1 chain", call.=FALSE)		
			convergence <- "Convergence cannot be assessed using only 1 chain"
			param.conv <- 1
			n.params <- 1
		}

		autocorrelation <- safe.autocorr.diag(normalisedmcmc)		
		crosscorrelation <- crosscorr(normalisedmcmc)
		class(crosscorrelation) <- "crosscorr.stats"	
		
		autocorrelated <- 0
		unconverged <- 0
		crosscorrelated <- 0
	
		for(j in 1:n.params){
			if(n.chains > 1){
				param.conv <- convergence$psrf[j, 1]
				if(!is.na(param.conv)){
					if(param.conv > psrf.target){
						unconverged <- unconverged + 1
					}
				}
			}
			param.autocorr <- autocorrelation[3,j]
			if(!is.na(param.autocorr)){
				if(param.autocorr > 0.1){
					autocorrelated <- autocorrelated + 1
				}
			}
			param.crosscorr <- crosscorrelation
			param.crosscorr[1:nrow(param.crosscorr), 1:ncol(param.crosscorr)] <- 0
			# print("0.3 is an arbitrary figure for crosscorr")
			# param.crosscorr is symmetrical - so divide by 2 to get number cross correlated:
			crosscorrelated <- sum(param.crosscorr > 0.3)/2
		}
		
		if(n.chains > 1){
			if(class(convergence$mpsrf)!="numeric"){
				mpsrfstring <- " (Unable to calculate the multi-variate psrf)"
			}else{
				mpsrfstring <- paste(" (multi-variate psrf = ", round(convergence$mpsrf, digits=3), ")", sep="")
			}
		}
		
		updates <- niter(mcmclist)
		
		if(!is.na(param.conv)){
			if(unconverged > 0){
				if(n.params==1 & !silent) swcat("Convergence failed for this run after ", updates, " iterations (psrf = ", round(convergence$psrf[1,1], digits=3), ")\n", sep="") else swcat("Convergence failed for this run for ", unconverged, " parameter", if(unconverged>1) "s", " after ", updates, " iterations", mpsrfstring, "\n", sep="")
			}else{
				if(n.chains > 1 & !silent) swcat("The Gelman-Rubin statistic is below ", psrf.target, " for all parameters\n", sep="")
				if(n.chains==1 & !silent) swcat("Calculating the Gelman-Rubin statistic requires two or more chains\n")
			}
		}else{
			if(!silent) swcat("There was an unexpected error calculating the Gelman-Rubin statistic\n")
		}
	
		if(!is.na(param.autocorr)){
			if(autocorrelated > 0){
#				if(!silent) swcat("IMPORTANT:  There was a high degree of autocorrelation for ", autocorrelated, " parameter", if(autocorrelated>1) "s", " (see the $autocorr element of the runjags object for more details)\n", sep="")
			}
		}else{
			if(!silent) swcat("There was an unexpected error calculating the autocorrelation dependence\n")
		}
		if(crosscorrelated > 0){
#			if(!silent) swcat("IMPORTANT:  There was a high degree of cross-correlation for ", crosscorrelated, " parameter pair", if(crosscorrelated>1) "s", " (see the $crosscorr element of the runjags object for more details)\n", sep="")
		}
	
	}, silent=FALSE)
	
	if(class(success)=="try-error"){
		if(!silent) swcat("An unexpected error occured when assessing convergence\n")
		convergence <- "An unexpected error occured when assessing convergence"
		autocorrelation <- "An unexpected error occured when assessing the autocorrelation"
		crosscorrelation <- "An unexpected error occured when assessing the cross-correlation"
	}
	
	options(show.error.messages = FALSE)
	success <- try({
	suppressWarnings(tsummary <- summary(combine.mcmc(mcmclist, collapse.chains=FALSE)))
	if(class(tsummary$statistics)=="numeric"){
		tsummary$statistics <- t(as.matrix(tsummary$statistics))
		dimnames(tsummary$statistics)[[1]] <- varnames(mcmclist)
		tsummary$quantiles <- t(as.matrix(tsummary$quantiles))
		dimnames(tsummary$quantiles)[[1]] <- varnames(mcmclist)
	}
	})
	if(class(success)=="try-error") tsummary <- "An unexpected error occured while calculating summary statistics"
	options(show.error.messages = TRUE)			
	
	collapsed <- combine.mcmc(mcmclist, collapse.chains=TRUE)
	
	options(show.error.messages = FALSE)
	success <- try({
	suppressWarnings(thpd <- HPDinterval(collapsed, prob=confidence))
	thpd <- cbind(lower=thpd[,1,drop=FALSE], median=apply(collapsed,2,median), upper=thpd[,2,drop=FALSE])
	dimnames(thpd) <- list(dimnames(thpd)[[1]], c(paste("Lower",round(confidence*100),sep=""), "Median", paste("Upper",round(confidence*100),sep="")))
	})

	if(class(success)=="try-error") thpd <- "An unexpected error occured while calculating summary statistics"
	options(show.error.messages = TRUE)			

	options(show.error.messages = FALSE)

	stochastic <- replicate(nvar(mcmclist), TRUE)
	success <- try({
		if(check.stochastic) stochastic <- tsummary$statistics[,2] != 0
		sseff <- effectiveSize(collapsed)
		se <- tsummary$statistics[,2]
		mcse <- se / sqrt(sseff)
		sseff <- sseff[stochastic]
		mcse <- mcse[stochastic]
		se <- se[stochastic]
		thmcse <- list(sseff=sseff, ssd=se, mcse=mcse)
		})	
	if(class(success)=="try-error") thmcse <- "An unexpected error occured while calculating Monte Carlo error"
	options(show.error.messages = TRUE)			
	
	class(thmcse) <- 'mcse.stats'
	
	
	if(plots){
		
		success <- try({

		thinned.mcmc <- combine.mcmc(list(mcmclist), collapse.chains=FALSE, return.samples=min(1000, niter(mcmclist)))
		thinned.mcmc <- normalise.mcmcfun(thinned.mcmc, normalise = FALSE, warn=FALSE, check.stochastic = check.stochastic)
		
		plot1 = plot2 = vector('list', length=length(varnames(thinned.mcmc)))
		names(plot1) = names(plot2) <- varnames(thinned.mcmc)
		
		# Was using this to manually over-write the x axis labels to start at burnin+1, but it's too complicated:
#		iternames <- dimnames(thinned.mcmc[[1]])[[1]]
#		iterrange <- as.numeric(c(iternames[1],iternames[length(iternames)]))
#		ilabels <- signif(seq(iterrange[1], iterrange[2], length.out=5)+1,3)
#		iat <- seq(0,niter(thinned.mcmc),length.out=5)
		
		for(i in 1:length(varnames(thinned.mcmc))){
			# Too flowery:  paste("Value of '", dimnames(plotdata[[1]])[[2]], "'", sep="")
			plotdata <- as.mcmc.list(lapply(thinned.mcmc, function(x) return(x[,i,drop=FALSE]))) # xyplot throws an error if not a matrix
			plot1[[i]] <- xyplot(plotdata, ylab=dimnames(plotdata[[1]])[[2]], xlab="Iteration")  #doesn't work: , scales=list(x=list(at=iat, labels=ilabels)))
			plot2[[i]] <- densityplot(plotdata, plot.points=FALSE, ylab="Density", xlab=dimnames(plotdata[[1]])[[2]])
		}

		class(plot1) <- 'runjags.plots'
		class(plot2) <- 'runjags.plots'
		
		#if(!is.null(startdev)){
		#	for(i in dev.list()){
		#		if(!any(startdev==i)) dev.off(i)
		#	}
		#}else{
		#	try(a <- dev.off(), silent=TRUE)
		#}
	
		})
		if(class(success)=="try-error"){
			plot1 = plot2 <- "An unexpected error occured while attempting to plot graphs"
			warning("An unexpected error occured while attempting to plot graphs")
		}
	}else{
		plot1 = plot2 <- "Plots not produced when plots=FALSE"
	}
	return(list(summary=tsummary, HPD=thpd, hpd=thpd, mcse=thmcse, psrf=convergence, autocorr=autocorrelation, crosscorr=crosscorrelation, stochastic=stochastic, dic=dic, trace=plot1, density=plot2))	
	
}