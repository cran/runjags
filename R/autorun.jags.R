autorun.jags <- function(model=stop("No model supplied"), monitor = stop("No monitored variables supplied"), data=NA, n.chains=2, inits = replicate(n.chains, NA), startburnin = 5000, startsample = 10000, psrf.target = 1.05, normalise.mcmc = TRUE, check.stochastic = TRUE, raftery.options = list(), crash.retry=1, plots = TRUE, thin.sample = TRUE, jags = findjags(), silent.jags = FALSE, interactive=TRUE, max.time=Inf, adaptive=list(type="burnin", length=200), modules=c(""), factories=c(""), thin = 1, monitor.deviance = FALSE, monitor.pd = FALSE, monitor.pd.i = FALSE, monitor.popt = FALSE, keep.jags.files=FALSE, tempdir=TRUE, method=if(.Platform$OS.type=='unix' & .Platform$GUI!="AQUA" & Sys.info()['user']!='nobody') 'interruptible' else 'simple'){
	
	if(any(tolower(monitor)=='dic')){
		dic <- TRUE
		monitor.deviance <- TRUE
		monitor.pd <- TRUE
		monitor.popt <- TRUE
	}else{
		dic <- FALSE
	}

	if(any(tolower(monitor)=='deviance')){
		monitor.deviance <- TRUE
	}
	
	if(any(c(monitor.deviance, monitor.pd, monitor.popt, monitor.pd.i))) modules <- c(modules, "dic")
	modules <- unique(modules)
	modules <- as.character(na.omit(modules[modules!=""]))
	
	if(as.integer(thin)!=thin | thin < 1) stop("The value supplied for thin must be a positive integer")
	
	if(thin.sample==TRUE) thin.sample <- startsample
	if(thin.sample==FALSE) thin.sample <- Inf
	
	startburnin <- startburnin * thin
	startsample <- startsample * thin
	
	if(startsample < 4000) stop("A startsample of 4000 or more iterations is required to complete the Raftery and Lewis's diagnostic")
	
	testmodel <- model
	testmonitor <- monitor
	
	pilot <- NA
	
	if(n.chains < 2) stop("A minimum of 2 chains should be used so that convergence can be assessed")
	
	if(psrf.target <= 1) stop("psrf target must be greater than 1")
	
	if(!is.list(raftery.options)) stop("Options to raftery.diag must be provided as a named list")
	
	if(any(names(raftery.options)=="data")) warning("The 'data' argument specified in raftery.options was ignored")
	
	raftery.args <- formals(raftery.diag)
	raftery.names <- names(raftery.args)

	if(length(raftery.options) > 0){
		if(is.null(names(raftery.options))) stop("Options to raftery.diag must be provided as a named list")
		raft.opt.names <- names(raftery.options)
		for(i in 1:length(raftery.options)){
			if(any(raft.opt.names[i]==raftery.names)){
				raftery.args[raft.opt.names[i]] <- raftery.options[i]
			}else{
				if(raft.opt.names[i]=="") stop("All arguments to raftery.diag must be named") else stop(paste(raft.opt.names[i], " is not a recognised argument to raftery.diag", sep=""))	
			}	
		}	

	}
	
	success <- try({
	raftery.args$data <- mcmc(1:startsample)
	class(raftery.args) <- "list"
	test <- do.call("raftery.diag", raftery.args)
	})
	
	if(class(success)=="try-error") stop("The arguments specified for raftery.diag are not valid")
	
	if(test$resmatrix[1]=="Error") stop(paste("You need a startsample size of at least", test$resmatrix[2], "with the values of q, r and s specified for the raftery.options", sep=" "))
	
	if(class(silent.jags)=="list"){
		killautocorr <- silent.jags$killautocorr
		silent.jags = silent.jags$silent.jags
	}else{
		killautocorr <- FALSE
	}

	if(class(max.time)=="numeric" | class(max.time)=="integer"){
		max.time <- max.time #DEFAULT NOW SECONDS * 60
	}else{
		if(class(max.time)!="character") stop("max.time must be either a numeric or character value")
		str.time <- strsplit(max.time, "")[[1]]

		time.unit <- suppressWarnings(str.time[is.na(as.numeric(str.time)!=str.time)])
		time.unit <- tolower(time.unit[time.unit!=" "][1])
		max.time <- suppressWarnings(as.numeric(paste(na.omit(str.time[as.numeric(str.time)==str.time]) ,collapse="")))
		
		max.time <- max.time * switch(time.unit, d=24*60*60, w=24*60*60*7, h=60*60, m=60, s=1, NA)
		if(is.na(max.time)) stop("Unrecognised unit of maximum time -'", time.unit, "'")
	}
	
	if(adaptive$type=="adapt"){
		burnadapt <- 0
		adapt <- adaptive$length
	}else{
		if(adaptive$type!="burnin") warning("Adaptive type not recognised")
		burnadapt <- adaptive$length
		adapt <- 0
	}
	
	newlines <- if(silent.jags) "\n" else "\n\n"
	
	swcat("\nAuto-run JAGS",newlines,"Running a pilot chain...\n",sep="")
	pre.time <- Sys.time()
	
	
	pilot <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = inits, burnin=startburnin, sample=startsample, adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE, jags = jags, psrf.target = psrf.target, normalise.mcmc=normalise.mcmc, check.stochastic = check.stochastic, modules=modules, factories=factories, thin=thin, monitor.deviance=monitor.deviance, monitor.pd=monitor.pd, monitor.pd.i=monitor.pd.i, monitor.popt=monitor.popt, keep.jags.files=keep.jags.files, tempdir=tempdir, method=method)
	if(any(pilot=="Unable to load coda files")){
		swcat("An error occured during the simulation\n\n")
		return(c("Error", "An error occured during the simulation"))
	}	
	
	if(any(is.na(pilot))){
		swcat("An error occured during the simulation\n\n")
		return(c("Error", "An error occured during the simulation"))
	}
	if(class(pilot)=="character"){
		swcat("An error occured during the simulation\n\n")
		return(c("Error", "An error occured during the simulation"))
	}
	
	if(any(names(pilot)=="crash.end")){
		repeat{
			time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
			if(time.taken > max.time | crash.retry==0){
				swcat("The simulation exceeded the number of crashes allowed by crash.retry and so was aborted\n\n")
				return(c("Error", "The simulation was aborted due to crashes"))
			}
			swcat("\nThe simulation crashed; retrying...",newlines,sep="")			
			crash.retry <- crash.retry - 1
			oldpilot <- pilot
			pilot <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = inits, burnin=startburnin, sample=startsample, adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE, jags = jags, psrf.target = psrf.target, normalise.mcmc=normalise.mcmc, check.stochastic = check.stochastic, modules=modules, factories=factories, thin=thin, monitor.deviance=monitor.deviance, monitor.pd=monitor.pd, monitor.pd.i=monitor.pd.i, monitor.popt=monitor.popt, keep.jags.files=keep.jags.files, tempdir=tempdir, method=method)
			
			if(!any(names(pilot)=="crash.end")) break
		}
	}

	firsttimetaken = time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
	swcat("\n")
	
	otheroutputs <- vector("list")
	if(monitor.pd) otheroutputs <- c(otheroutputs, pd=list(pilot$pd))
	if(monitor.pd.i) otheroutputs <- c(otheroutputs, pd.i=list(pilot$pd.i))
	if(monitor.popt) otheroutputs <- c(otheroutputs, popt=list(pilot$popt))
	if(dic) otheroutputs <- c(otheroutputs, dic=list(pilot$dic))

	final.mcmc <- pilot$mcmc
	
	swcat("Calculating the Gelman-Rubin statistic for ", nvar(final.mcmc), " variables....\n", sep="")
	
	suppressWarnings(success <- try(convergence <- safe.gelman.diag(normalise.mcmcfun(final.mcmc, normalise=normalise.mcmc, warn=FALSE, check.stochastic = check.stochastic), transform=FALSE, autoburnin=TRUE), silent=TRUE))
	if(class(success)=="try-error"){
		swcat("An error occured while calculating the Gelman-Rubin statistic; aborting simulation.  Check that different chains have not been given the same starting values and random seeds.\n")
		return(c("Error", "An error occured while calculating the Gelman-Rubin statistic"))
	}
	
	convergence <- c(convergence, psrf.target=psrf.target)
	class(convergence) <- "gelman.with.target"
	
	n.params <- nrow(convergence$psrf)
	n.iters <- niter(final.mcmc)
	
	if(n.params==1) convergence$mpsrf <- convergence$psrf[1,1]
	unconverged <- 0

	for(j in 1:n.params){
		param.conv <- convergence$psrf[j, 1]
		if(!is.na(param.conv)){
			if(param.conv > psrf.target){
				unconverged <- unconverged + 1
			}
		}else{
			warning(paste("The Gelman-Rubin statistic for '", varnames(final.mcmc)[j], "' could not be calculated", sep=""))
		}
	}

	updatesdone <- startsample
	updatesthrown <- 0
	
	#if(!is.na(param.conv)){
		if(unconverged > 0){
			if(class(convergence$mpsrf)!="numeric"){
				mpsrfstring <- " (Unable to calculate the multi-variate psrf)"
			}else{
				mpsrfstring <- paste(" (multi-variate psrf = ", round(convergence$mpsrf, digits=3), ")", sep="")
			}
			
			swcat("The Gelman-Rubin statistic was above ", psrf.target, " for ", unconverged, " parameter", if(unconverged>1) "s", " after ", n.iters, " iterations", mpsrfstring, ".  This may indicate poor convergence.\n", sep="")
			updatesthrown <- updatesthrown + startsample
			time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
			stop <- time.taken > max.time
			
			if(interactive){
				time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
				stop <- !ask("Extend the simulation to attempt to improve convergence?")
				pre.time <- Sys.time()-time.taken
			}
			if(stop){
				
				swcat("Calculating autocorrelation and summary statistics...\n")
				
				suppressWarnings(success <- try(autocorr <- safe.autocorr.diag(normalise.mcmcfun(final.mcmc, normalise=FALSE, warn=FALSE, check.stochastic = check.stochastic)), silent=TRUE))
				if(class(success)=="try-error"){
					swcat("An error occured while calculating the autocorrelation; aborting simulation\n")
					return(c("Error", "An error occured while calculating the autocorrelation"))
				}

				options(show.error.messages = FALSE)
				suppressWarnings(fsummary <- summary(combine.mcmc(final.mcmc, collapse.chains=FALSE)))
				options(show.error.messages = TRUE)
				
				options(show.error.messages = FALSE)
				suppressWarnings(thpd <- HPDinterval(combine.mcmc(final.mcmc, collapse.chains=TRUE)))
				thpd <- cbind(lower95=thpd[,1], median=apply(combine.mcmc(final.mcmc, collapse.chains=TRUE),2,median), upper95=thpd[,2])
				options(show.error.messages = TRUE)	
				
				swcat("Returning UNCONVERGED simulation results\n\n")
				
				return(c(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=NA, pilot.summary=fsummary, pilot.HPD=thpd, samples.to.conv=NA, psrf=convergence, autocorr=autocorr), otheroutputs))
			}else{
				finishconv <- FALSE
				swcat("Extending the simulation to attempt to improve convergence...\n")
				thrownaway <- 0
			}
			
			additional <- pilot
			
			while(finishconv==FALSE){
			
				time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
				availableupdates <- updatesdone / time.taken * (max.time - time.taken)
				neededupdates <- as.numeric(round(min(availableupdates, startsample)))
				# if less updates are available than startsample, then some of the last run are used to make the total up to startsample using window() in about 35 lines time
				
				if(neededupdates > 0){
					
					oldadditional <- additional
					additional <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits=pilot$end.state, burnin=burnadapt, sample=neededupdates, adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE, jags = jags, psrf.target = psrf.target, normalise.mcmc=normalise.mcmc, check.stochastic = check.stochastic, modules=modules, factories=factories, thin=thin, monitor.deviance=monitor.deviance, monitor.pd=monitor.pd, monitor.pd.i=monitor.pd.i, monitor.popt=monitor.popt, keep.jags.files=keep.jags.files, tempdir=tempdir, method=method)
					
					if(any(pilot=="Unable to load coda files")){
						swcat("\nThere was an error in the second simulation, possibly due to bad initial values or Random Number Seed values obtained from the first simulation.  You could try using fewer chains.")
					}
					
					if(any(additional=="Unable to load coda files")){
						swcat("An error occured during the simulation\n\n")
						return(c("Error", "An error occured during the simulation"))
					}
					if(any(names(additional)=="crash.end")){
						repeat{
							time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
							
							if(time.taken > max.time | crash.retry==0){
								swcat("The simulation exceeded the number of crashes allowed by crash.retry and so was aborted\n\n")
								return(c("Error", "The simulation was aborted due to crashes"))
							}
							swcat("\nThe simulation crashed; retrying...",newlines,sep="")
							crash.retry <- crash.retry - 1
							oldadd <- additional
							additional <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = oldadditional$end.state, burnin=burnadapt, sample=(neededupdates), adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE, jags = jags, psrf.target = psrf.target, normalise.mcmc=normalise.mcmc, check.stochastic = check.stochastic, modules=modules, factories=factories, thin=thin, monitor.deviance=monitor.deviance, monitor.pd=monitor.pd, monitor.pd.i=monitor.pd.i, monitor.popt=monitor.popt, keep.jags.files=keep.jags.files, tempdir=tempdir, method=method)
							
							if(!any(names(additional)=="crash.end")) break
						}
					}
					
					pilot <- additional
					
					suppressWarnings(final.mcmc <- window(combine.mcmc(list(final.mcmc, additional$mcmc)), collapse.chains=FALSE,  start=(((niter(final.mcmc)+niter(additional$mcmc))-startsample)+1)))
					
					swcat("Calculating the Gelman-Rubin statistic for ", nvar(final.mcmc), " variables....\n", sep="")
					thrownaway <- thrownaway+neededupdates
					suppressWarnings(success <- try(convergence <- safe.gelman.diag(normalise.mcmcfun(final.mcmc, normalise=normalise.mcmc, warn=FALSE, check.stochastic = check.stochastic), transform=FALSE, autoburnin=TRUE), silent=TRUE))
					if(class(success)=="try-error"){
						swcat("An error occured while calculating the Gelman-Rubin statistic; aborting simulation.  Check that different chains have not been given the same starting values and random seeds.\n")
						return(c("Error", "An error occured while calculating the Gelman-Rubin statistic"))
					}
					
					convergence <- c(convergence, psrf.target=psrf.target)
					class(convergence) <- "gelman.with.target"
					
					n.params <- nrow(convergence$psrf)
					
					unconverged <- 0
					if(n.params==1) convergence$mpsrf <- convergence$psrf[1,1]
				
					for(j in 1:n.params){
						param.conv <- convergence$psrf[j, 1]
						if(!is.na(param.conv)){
							if(param.conv > psrf.target){
								unconverged <- unconverged + 1
							}
						}else{
							warning(paste("The Gelman-Rubin statistic for '", varnames(final.mcmc)[j], "' could not be calculated", sep=""))
						}	
					}
					
					if(monitor.pd) otheroutputs$pd <- additional$pd
					if(monitor.pd.i) otheroutputs$pd.i <- additional$pd.i
					if(monitor.popt) otheroutputs$popt <- additional$popt
					if(dic) otheroutputs$dic <- additional$dic
					
					if(class(convergence$mpsrf)!="numeric"){
						mpsrfstring <- " (Unable to calculate the multi-variate psrf)"
					}else{
						mpsrfstring <- paste(" (multi-variate psrf = ", round(convergence$mpsrf, digits=3), ")", sep="")
					}
					
					if(unconverged > 0){
						swcat("The Gelman-Rubin statistic was still above ", psrf.target, " for ", unconverged, " parameter", if(unconverged>1) "s", " after ", updatesdone + neededupdates, " iterations", mpsrfstring, ".\n", sep="")
						
						stop <- time.taken > max.time
						if(interactive){
							time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
							stop <- !ask("Extend the simulation to attempt to improve convergence?")
							pre.time <- Sys.time()-time.taken
						}
						if(stop){
							
							swcat("Calculating autocorrelation and summary statistics...\n")
							
							suppressWarnings(success <- try(autocorr <-  safe.autocorr.diag(normalise.mcmcfun(final.mcmc, normalise=FALSE, warn=FALSE, check.stochastic = check.stochastic)), silent=TRUE))
							if(class(success)=="try-error"){
								swcat("An error occured while calculating the autocorrelation; aborting simulation\n")
								return(c("Error", "An error occured while calculating the autocorrelation"))
							}

							options(show.error.messages = FALSE)
							suppressWarnings(fsummary <- summary(combine.mcmc(final.mcmc, collapse.chains=FALSE)))
							options(show.error.messages = TRUE)
							
							options(show.error.messages = FALSE)
							suppressWarnings(thpd <- HPDinterval(combine.mcmc(final.mcmc, collapse.chains=TRUE)))
							thpd <- cbind(lower95=thpd[,1], median=apply(combine.mcmc(final.mcmc, collapse.chains=TRUE),2,median), upper95=thpd[,2])
							options(show.error.messages = TRUE)	
							
							swcat("Returning UNCONVERGED simulation results\n\n")
							
							return(c(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=NA, pilot.summary=fsummary, pilot.HPD=thpd, samples.to.conv=NA, psrf=convergence, autocorr=autocorr), otheroutputs))
						}else{
							swcat("Extending the simulation to attempt to improve convergence...\n")
							updatesdone <- updatesdone + neededupdates
							updatesthrown <- updatesthrown + neededupdates
						}
						
					}else{
						swcat(paste("The Gelman-Rubin statistic is now below ", psrf.target, " for all parameters\n", sep=""))
						finishconv <- TRUE
					}
				}else{
					swcat("Maximum time limit exceeded; chains still unconverged.  Try restarting the simulation using the end state values of the chains provided\n")
					swcat("Calculating autocorrelation and summary statistics...\n")
					
					suppressWarnings(success <- try(autocorr <-  safe.autocorr.diag(normalise.mcmcfun(final.mcmc, normalise=FALSE, warn=FALSE, check.stochastic = check.stochastic)), silent=TRUE))
					if(class(success)=="try-error"){
						swcat("An error occured while calculating the autocorrelation; aborting simulation\n")
						return(c("Error", "An error occured while calculating the autocorrelation"))
					}

					options(show.error.messages = FALSE)
					suppressWarnings(fsummary <- summary(combine.mcmc(final.mcmc, collapse.chains=FALSE)))
					options(show.error.messages = TRUE)
					
					options(show.error.messages = FALSE)
					suppressWarnings(thpd <- HPDinterval(combine.mcmc(final.mcmc, collapse.chains=TRUE)))
					thpd <- cbind(lower95=thpd[,1], median=apply(combine.mcmc(final.mcmc, collapse.chains=TRUE),2,median), upper95=thpd[,2])
					options(show.error.messages = TRUE)	
					
				
					swcat("Returning UNCONVERGED simulation results\n\n")
					return(c(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=NA, pilot.summary=fsummary, pilot.HPD=thpd, samples.to.conv=NA, psrf=convergence, autocorr=autocorr), otheroutputs))
				}
			
			}
	
		}else{
			swcat(paste("The Gelman-Rubin statistic is below ", psrf.target, " for all parameters\n", sep=""))
			updatesthrown <- 0
		}
	#}else{
	#	return(c("Error", "The Gelman-Rubin statistic could not be calculated for these chains"))
	#}
	
	
	
	swcat("\nCalculating the necessary sample length based on the Raftery and Lewis's diagnostic...\n")
			
	success <- try({
	raftery.args$data <- normalise.mcmcfun(final.mcmc, normalise=FALSE, warn=FALSE, check.stochastic = check.stochastic)
	class(raftery.args) <- "list"
	raftery <- do.call("raftery.diag", raftery.args)
	})
	
	#print(niter(final.mcmc))
	#print(raftery)
	
	if(class(success)=="try-error") return(c("Error", "An error occured while calculating the Raftery and Lewis's diagnostic"))
	if(raftery[[1]]$resmatrix[1]=="error") return(c("Error", "An error occured while calculating the Raftery and Lewis diagnostic"))
			
	
	# to correct for monitoring arrays and non-stochastic nodes:
	newmonitor <- dimnames(raftery[[1]]$resmatrix)[[1]]
	
	dependance = burnin = sample <- matrix(ncol=n.chains, nrow=length(newmonitor), dimnames=list(dimnames(raftery[[1]]$resmatrix)[[1]], 1:n.chains))
	
	for(i in 1:n.chains){	
		dependance[,i] <- raftery[[i]]$resmatrix[,"I"]
		burnin[,i] <- raftery[[i]]$resmatrix[,"M"]
		sample[,i] <- raftery[[i]]$resmatrix[,"N"]
	}
	
	dependancethreshold <- 3
	
	if(any(dependance > dependancethreshold) & killautocorr==FALSE){
				swcat("IMPORTANT:  The sample size of monitored node(s) '", paste(dimnames(dependance)[[1]][apply(dependance, 1, function(x) if(any(x>dependancethreshold)) return(TRUE) else return(FALSE))], collapse="' & '"), "' have a high autocorrelation dependance in chain(s) ", paste(seq(1, n.chains)[apply(dependance, 2, function(x) if(any(x>dependancethreshold)) return(TRUE) else return(FALSE))], collapse= " & "), ".  Re-running the model with a different formulation or better initial values may help to reduce autocorrelation.\n", sep="")
	}
	
	#moreupdates <- ((max(sample+(max(0, burnin-startburnin)))-(startsample*n.chains))/n.chains)*1.05
	moreupdates <- ((max(sample) - (startsample*n.chains)) / n.chains)#*1.05
	moreupdates <- ceiling(moreupdates/thin)*thin

	if(moreupdates > 0){
	
		success <- try({
			testmatrix <- matrix(nrow=(max(sample)/thin)*n.chains, ncol=length(newmonitor)+as.numeric(monitor.deviance))
			if(monitor.pd.i) testmatrix2 <- matrix(nrow=nrow(pilot$pd.i), ncol=1) else testmatrix2 <- NA
			if(monitor.popt) testmatrix3 <- matrix(nrow=nrow(pilot$popt), ncol=1) else testmatrix3 <- NA
			if(monitor.pd) testmatrix4 <- matrix(nrow=(max(sample)/thin), ncol=1) else testmatrix4 <- NA
			rm(testmatrix, testmatrix2, testmatrix3, testmatrix4)
			
		})
		if(class(success)=="try-error"){
			swcat("The model needs to be run for a further ", moreupdates, " iterations.  This would create a vector too large to be read into R.  Try re-parameterising the model to reduce autocorrelation, using the thin option to reduce autocorrelation, or monitoring less variables.  The simulation can be restarted using the chain end states provided", sep="")
			return(list(error="Unable to create vector of necessary sample size", req.samples=max(sample), end.state=pilot$end.state))
		}
	
		
		if(FALSE){  ###### START NOT RUN
			## BELOW WAS FOR ADDING RAFTERY EXTENSION TO TIME LIMIT (MODIFIED AND BROKEN...)
		if((timestring(pre.time, Sys.time(), units="s", show.units=FALSE)+(timestring((time.taken*moreupdates/(startsample+startburnin)), units="s", show.units=FALSE))) < max.time){
			swcat("The model will need to be run for a further ", moreupdates, " updates.  This will take approximately ", timestring((firsttimetaken*moreupdates/(startsample+startburnin))), ".\n", sep="")
			if(interactive & (timestring((time.taken*moreupdates/(startsample+startburnin)), units="s", show.units=FALSE)>60)) if(!ask("Continue with the simulation?")){
				swcat("Simulation aborted\n\n")
				suppressWarnings(success <- try(autocorr <-  safe.autocorr.diag(normalise.mcmcfun(final.mcmc, normalise=FALSE, warn=FALSE, check.stochastic = check.stochastic)), silent=TRUE))
				if(class(success)=="try-error"){
					swcat("An error occured while calculating the autocorrelation; aborting simulation\n")
					return(c("Error", "An error occured while calculating the autocorrelation"))
				}

				options(show.error.messages = FALSE)
				suppressWarnings(fsummary <- summary(combine.mcmc(final.mcmc, collapse.chains=FALSE)))
				options(show.error.messages = TRUE)
				
				options(show.error.messages = FALSE)
				suppressWarnings(thpd <- HPDinterval(combine.mcmc(final.mcmc, collapse.chains=TRUE)))
				thpd <- cbind(lower95=thpd[,1], median=apply(combine.mcmc(final.mcmc, collapse.chains=TRUE),2,median), upper95=thpd[,2])
				options(show.error.messages = TRUE)	
				
				swcat("Calculating autocorrelation and summary statistics...\n")
				return(c(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=max(sample), pilot.summary=fsummary, pilot.HPD=thpd, samples.to.conv=NA, psrf=convergence, autocorr=autocorr), otheroutputs))
				
			}
		}else{
			swcat("The model will need to be run for a further ", moreupdates, " updates.  This will take approximately ", timestring((firsttimetaken*moreupdates/(startsample+startburnin))), ".\n", sep="")
			#cat("The model would need to be run for a further ", moreupdates, " updates.  This will take approximately ", timestring((time.taken*moreupdates/(startsample+startburnin))), ", taking the total simulation length to over ", timestring(max.time), ".\n", sep="")  NOW NO LONGER ADDING THIS TO TIME LIMIT
			stop <- TRUE
			if(interactive & (timestring((time.taken*moreupdates/(startsample+startburnin)), units="s", show.units=FALSE)>60)) stop <- !ask("Continue with the simulation?")
			if(stop){
				suppressWarnings(success <- try(autocorr <-  safe.autocorr.diag(normalise.mcmcfun(final.mcmc, normalise=FALSE, warn=FALSE, check.stochastic = check.stochastic)), silent=TRUE))
				if(class(success)=="try-error"){
					swcat("An error occured while calculating the autocorrelation; aborting simulation\n")
					return(c("Error", "An error occured while calculating the autocorrelation"))
				}

				options(show.error.messages = FALSE)
				suppressWarnings(fsummary <- summary(combine.mcmc(final.mcmc, collapse.chains=FALSE)))
				options(show.error.messages = TRUE)
				
				options(show.error.messages = FALSE)
				suppressWarnings(thpd <- HPDinterval(combine.mcmc(final.mcmc, collapse.chains=TRUE)))
				thpd <- cbind(lower95=thpd[,1], median=apply(combine.mcmc(final.mcmc, collapse.chains=TRUE),2,median), upper95=thpd[,2])
				options(show.error.messages = TRUE)	
				
				swcat("Simulation aborted\n\n")
				return(c(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=max(sample), pilot.summary=fsummary, pilot.HPD=thpd, samples.to.conv=NA, psrf=convergence, autocorr=autocorr), otheroutputs))
			}
		}
		}##### END NOT RUN
		
		swcat("The model will need to be run for a further ", moreupdates, " updates.  This will take approximately ", timestring((firsttimetaken*moreupdates/(startsample+startburnin))), ".\n", sep="")
		if(interactive & (timestring((time.taken*moreupdates/(startsample+startburnin)), units="s", show.units=FALSE)>60)){
			time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
			if(!ask("Continue with the simulation?")){
				
				swcat("Calculating autocorrelation and summary statistics...\n")
				suppressWarnings(success <- try(autocorr <-  safe.autocorr.diag(normalise.mcmcfun(final.mcmc, normalise=FALSE, warn=FALSE, check.stochastic = check.stochastic)), silent=TRUE))
				if(class(success)=="try-error"){
					swcat("An error occured while calculating the autocorrelation; aborting simulation\n")
					return(c("Error", "An error occured while calculating the autocorrelation"))
				}

				options(show.error.messages = FALSE)
				suppressWarnings(fsummary <- summary(combine.mcmc(final.mcmc, collapse.chains=FALSE)))
				options(show.error.messages = TRUE)
				options(show.error.messages = FALSE)
				suppressWarnings(thpd <- HPDinterval(combine.mcmc(final.mcmc, collapse.chains=TRUE)))
				thpd <- cbind(lower95=thpd[,1], median=apply(combine.mcmc(final.mcmc, collapse.chains=TRUE),2,median), upper95=thpd[,2])
				options(show.error.messages = TRUE)	
				
				pre.time <- Sys.time()-time.taken
				swcat("Simulation aborted\n\n")
				return(c(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=max(sample), pilot.summary=fsummary, pilot.HPD=thpd, samples.to.conv=NA, psrf=convergence, autocorr=autocorr), otheroutputs))
			}
			pre.time <- Sys.time()-time.taken
		}
		
		
		swcat("\n")
		additional <- run.jags(data=data, model=model, monitor=monitor,  n.chains=n.chains, inits=pilot$end.state, burnin=burnadapt, sample=moreupdates, adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE, jags = jags, psrf.target = psrf.target, normalise.mcmc=normalise.mcmc, check.stochastic = check.stochastic, modules=modules, factories=factories, thin=thin, monitor.deviance=monitor.deviance, monitor.pd=monitor.pd, monitor.pd.i=monitor.pd.i, monitor.popt=monitor.popt, keep.jags.files=keep.jags.files, tempdir=tempdir, method=method)
		if(any(additional=="Unable to load coda files")){
			swcat("An error occured during the simulation\n\n")
			return(c("Error", "An error occured during the simulation"))
		}
		
		if(any(names(additional)=="crash.end")){
			repeat{
				time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
				if(time.taken > max.time | crash.retry==0){
					swcat("The simulation exceeded the number of crashes allowed by crash.retry and so was aborted\n\n")
					return(c("Error", "The simulation was aborted due to crashes"))
				}
				swcat("\nThe simulation crashed; retrying...",newlines,sep="")			
				crash.retry <- crash.retry - 1
				oldadd <- additional
				additional <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = pilot$end.state, burnin=burnadapt, sample=moreupdates, adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE, jags = jags, psrf.target = psrf.target, normalise.mcmc=normalise.mcmc, check.stochastic = check.stochastic, modules=modules, factories=factories, thin=thin, monitor.deviance=monitor.deviance, monitor.pd=monitor.pd, monitor.pd.i=monitor.pd.i, monitor.popt=monitor.popt, keep.jags.files=keep.jags.files, tempdir=tempdir, method=method)
				if(any(additional=="Unable to load coda files")){
					swcat("An error occured during the simulation\n\n")
					return(c("Error", "An error occured during the simulation"))
				}
				if(!any(names(additional)=="crash.end")) break
			}
		}
		
		final.mcmc <- combine.mcmc(list(final.mcmc, additional$mcmc), collapse.chains=FALSE)
		
		
	}else{
		additional <- pilot # for end.state at the end, pilot is output of either first sim or extended convergence sim
	}	
	
	swcat("Necessary sample length achieved\n")
	
	final.mcmc <- combine.mcmc(final.mcmc, collapse.chains=FALSE, return.samples=thin.sample)
	
	if(monitor.pd) otheroutputs$pd <- additional$pd
	if(monitor.pd.i) otheroutputs$pd.i <- additional$pd.i
	if(monitor.popt) otheroutputs$popt <- additional$popt
	if(dic) otheroutputs$dic <- additional$dic
	
	#final.mcmc <- window(final.mcmc, start=max(burnin))
	n.iters <- niter(final.mcmc)
	
	suppressWarnings(success <- try(convergence <- safe.gelman.diag(normalise.mcmcfun(final.mcmc, normalise=normalise.mcmc, warn=FALSE, check.stochastic = check.stochastic), warn=TRUE, transform=FALSE, autoburnin=TRUE), silent=TRUE))
	if(class(success)=="try-error"){
		swcat("An error occured while calculating the Gelman-Rubin statistic; aborting simulation.  Check that different chains have not been given the same starting values and random seeds.\n")
		return(c("Error", "An error occured while calculating the Gelman-Rubin statistic"))
	}
	suppressWarnings(success <- try(autocorrelation <- safe.autocorr.diag(normalise.mcmcfun(final.mcmc, normalise=FALSE, warn=FALSE, check.stochastic = check.stochastic)), silent=TRUE))
	if(class(success)=="try-error"){
		swcat("An error occured while calculating the autocorrelation; aborting simulation\n")
		return(c("Error", "An error occured while calculating the autocorrelation"))
	}
	
	
	options(show.error.messages = FALSE)
	suppressWarnings(fsummary <- summary(combine.mcmc(final.mcmc, collapse.chains=FALSE)))
	options(show.error.messages = TRUE)
	
	options(show.error.messages = FALSE)
	suppressWarnings(thpd <- HPDinterval(combine.mcmc(final.mcmc, collapse.chains=TRUE)))
	thpd <- cbind(lower95=thpd[,1], median=apply(combine.mcmc(final.mcmc, collapse.chains=TRUE),2,median), upper95=thpd[,2])
	options(show.error.messages = TRUE)	
	
	convergence <- c(convergence, psrf.target=psrf.target)
	class(convergence) <- "gelman.with.target"

	unconverged <- 0
	
	n.params <- nrow(convergence$psrf)
	if(n.params==1) convergence$mpsrf <- convergence$psrf[1,1]
	
	for(j in 1:n.params){
		param.conv <- convergence$psrf[j, 1]
		if(!is.na(param.conv)){
			if(param.conv > psrf.target){
				unconverged <- unconverged + 1
			}
		}else{
			warning(paste("The Gelman-Rubin statistic for '", varnames(final.mcmc)[j], "' could not be calculated", sep=""))
		}	
	}
	
	if(class(convergence$mpsrf)!="numeric"){
		mpsrfstring <- " (Unable to calculate the multi-variate psrf)"
	}else{
		mpsrfstring <- paste(" (multi-variate psrf = ", round(convergence$mpsrf, digits=3), ")", sep="")
	}
	
	if(unconverged > 0 & !killautocorr){
		swcat(paste("WARNING:  The Gelman-Rubin statistic for ", unconverged, " parameter", if(unconverged>1) "s", " are above ", psrf.target, " for the final chains", mpsrfstring, ".  The chains may have fallen out of convergence.  It is important to assess convergence manually before relying on inference from these chains.\n", sep=""))
	}
	
	if(n.params==1) convergence$mpsrf <- NULL
	
	autocorrelated <- 0
	
	for(j in 1:n.params){
		param.autocorr <- autocorrelation[3,j]
		if(!is.na(param.autocorr)){
			if(param.autocorr > 0.1){
				autocorrelated <- autocorrelated + 1
			}
		}
	}
	
	if(killautocorr==FALSE){
		if(!is.na(param.autocorr)){
			if(autocorrelated > 0 & moreupdates > 0){
				swcat("REMINDER:  There was a high degree of autocorrelation for ", autocorrelated, " parameter", if(autocorrelated>1) "s", "\n", sep="")
			}else{
				#cat("Convergence achieved for this run\n")
			}
		}else{
			swcat("Autocorrelation could not be calculated for these chains\n")
		}
		unused <- normalise.mcmcfun(final.mcmc, normalise=FALSE, warn="warning", check.stochastic = check.stochastic)  # SO THAT WARNING OF NON STOCHASTICITY IS PRINTED
	}
	
	if(plots==TRUE){
		success <- try({
		plot1 = plot2 = vector('list', length=length(varnames(final.mcmc)))
		names(plot1) = names(plot2) <- varnames(final.mcmc)
		thinned.mcmc <- combine.mcmc(list(final.mcmc), collapse.chains=FALSE, return.samples=min(1000, niter(final.mcmc)))

		#startdev <- dev.list()

		#a <- dev.new()
		#if(options("device")$device=="x11") x11()
	
		for(i in 1:length(varnames(final.mcmc))){

			plotdata <- thinned.mcmc[,c(i,i)] # xyplot throws an error if there is only 1 variable, so double the plot...
			varnames(plotdata)[2] <- 'dummy' # Different name prevents warning about duplicate factors
			plot1[[i]] <- xyplot(plotdata, layout=c(1,1), ylab="Value", xlab="Iteration")
			class(plot1[[i]]) <- "plotindpages"
			plot2[[i]] <- densityplot(plotdata, plot.points=FALSE, layout=c(1,1), ylab="Density", xlab="Value")
			class(plot2[[i]]) <- "plotindpages"

			# ...and then remove the index for the unnecessary second plot afterwards:
			plot1[[i]]$index.cond[[1]] <- 1
			plot2[[i]]$index.cond[[1]] <- 1

		}
	
		#if(!is.null(startdev)){
		#	for(i in dev.list()){
		#		if(!any(startdev==i)) dev.off(i)
		#	}
		#}else{
		#	a <- dev.new()
		#	while(!is.null(dev.list())){
		#		dev.off()
		#	}
		#}
		
		})
		if(class(success)=="try-error"){
			plot1 = plot2 <- "An unexpected error occured while attempting to plot graphs"
			warning("An unexpected error occured while attempting to plot graphs")
		}
	}else{
		plot1 = plot2 <- "Plots not produced when plots==FALSE"
	}
	
	swcat("Auto-run JAGS complete.\n\n")
	if(killautocorr==FALSE) swcat("*PLEASE NOTE:  THIS SOFTWARE IS INTENDED FOR EDUCATIONAL PURPOSES ONLY*\n*YOU SHOULD ASSESS CONVERGENCE AND AUTOCORRELATION MANUALLY BEFORE RELYING ON RESULTS PROVIDED*\n\n")
	
	
	return(c(list(mcmc=final.mcmc, end.state=additional$end.state, req.samples=max(sample), samples.to.conv=updatesthrown, thin=thin, summary=fsummary, HPD=thpd, psrf=convergence, autocorr=autocorrelation, trace=plot1, density=plot2), otheroutputs))
}
	
autorun.JAGS <- autorun.jags
