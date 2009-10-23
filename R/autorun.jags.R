autorun.jags <- function(model=stop("No model supplied"), monitor = stop("No monitored variables supplied"), data=NA, n.chains=2, inits = replicate(n.chains, NA), startburnin = 5000, startsample = 10000, psrf.target = 1.05, normalise.mcmc = TRUE, raftery.options = list(), crash.retry=1, plots = TRUE, thin.sample = TRUE, jags = findjags(), silent.jags = FALSE, interactive=TRUE, max.time=Inf, adaptive=list(type="burnin", length=200)){

	
	if(thin.sample==TRUE) thin.sample <- startsample
	if(thin.sample==FALSE) thin.sample <- Inf
	
	if(startsample < 4000) stop("A startsample of 4000 or more iterations is required to complete the Raftery and Lewis's diagnostic")
	
	testmodel <- model
	testmonitor <- monitor
	
	pilot <- NA
	
	if(n.chains < 2) stop("Minimum of 2 chains should be used so that convergence can be assessed")
	
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
	
	cat("\nAuto-run JAGS",newlines,"Running a pilot chain...\n",sep="")
	pre.time <- Sys.time()
	
	
	pilot <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = inits, burnin=startburnin, sample=startsample, adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE)
	if(any(pilot=="Unable to load coda files")){
		cat("An error occured during the simulation\n\n")
		return(c("Error", "An error occured during the simulation"))
	}	
	
	if(any(is.na(pilot))){
		cat("An error occured during the simulation\n\n")
		return(c("Error", "An error occured during the simulation"))
	}
	if(class(pilot)=="character"){
		cat("An error occured during the simulation\n\n")
		return(c("Error", "An error occured during the simulation"))
	}
	
	if(any(names(pilot)=="crash.end")){
		repeat{
			time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
			if(time.taken > max.time | crash.retry==0){
				cat("The simulation exceeded the number of crashes allowed by crash.retry and so was aborted\n\n")
				return(c("Error", "The simulation was aborted due to crashes"))
			}
			cat("\nThe simulation crashed; retrying...",newlines,sep="")			
			crash.retry <- crash.retry - 1
			oldpilot <- pilot
			pilot <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = inits, burnin=startburnin, sample=startsample, adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE)
			
			if(!any(names(pilot)=="crash.end")) break
		}
	}
	
	time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
	cat("\n")
	
	
	
	final.mcmc <- pilot$mcmc
	
	cat("Calculating the Gelman-Rubin statistic....\n")
	suppressWarnings(success <- try(convergence <- safe.gelman.diag(normalise.mcmc(final.mcmc, normalise=normalise.mcmc, warn=FALSE), transform=FALSE, autoburnin=TRUE), silent=TRUE))
	if(class(success)=="try-error"){
		cat("An error occured while calculating the Gelman-Rubin statistic; aborting simulation.  Check that different chains have not been given the same starting values and random seeds.\n")
		return(c("Error", "An error occured while calculating the Gelman-Rubin statistic"))
	}
	suppressWarnings(success <- try(autocorr <- autocorr.diag(normalise.mcmc(final.mcmc, normalise=FALSE, warn=FALSE)), silent=TRUE))
	if(class(success)=="try-error"){
		cat("An error occured while calculating the autocorrelation; aborting simulation\n")
		return(c("Error", "An error occured while calculating the autocorrelation"))
	}
	
	options(show.error.messages = FALSE)
	suppressWarnings(fsummary <- summary(final.mcmc))
	options(show.error.messages = TRUE)
	
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
		}	
	}

	updatesdone <- startsample
	updatesthrown <- 0
	
	if(!is.na(param.conv)){
		if(unconverged > 0){
			cat("The Gelman-Rubin statistic was above ", psrf.target, " for ", unconverged, " parameter(s) after ", n.iters, " iterations (multi-variate psrf = ", as.numeric(round(convergence$mpsrf, digits=3)), ").  This may indicate poor convergence.\n", sep="")
			updatesthrown <- updatesthrown + startsample
			time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
			stop <- time.taken > max.time
			if(interactive) stop <- !ask("Extend the simulation to attempt to improve convergence?")
			if(stop){
				cat("Returning UNCONVERGED simulation results\n\n")
				return(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=NA, req.burnin=NA, pilot.summary=fsummary, samples.to.conv=NA, psrf=convergence, autocorr=autocorr))
			}else{
				finishconv <- FALSE
				cat("Extending the simulation to attempt to improve convergence...\n")
				thrownaway <- 0
			}
			
			pre.time <- Sys.time()-time.taken
			additional <- pilot
			
			while(finishconv==FALSE){
			
				time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
				availableupdates <- updatesdone / time.taken * (max.time - time.taken)
				neededupdates <- updatesdone / time.taken * 600
				neededupdates <- as.numeric(round(min(neededupdates, availableupdates, startsample)))
			
				if(neededupdates > 0){
					
					oldadditional <- additional
					additional <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits=pilot$end.state, burnin=burnadapt, sample=neededupdates, adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE)
					pilot <- additional
					
					if(any(pilot=="Unable to load coda files")){
						cat("\nThere was an error in the second simulation, possibly due to bad initial values or Random Number Seed values obtained from the first simulation.  You could try using fewer chains.")
					}
					
					if(any(additional=="Unable to load coda files")){
						cat("An error occured during the simulation\n\n")
						return(c("Error", "An error occured during the simulation"))
					}
					if(any(names(additional)=="crash.end")){
						repeat{
							time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
							if(time.taken > max.time | crash.retry==0){
								cat("The simulation exceeded the number of crashes allowed by crash.retry and so was aborted\n\n")
								return(c("Error", "The simulation was aborted due to crashes"))
							}
							cat("\nThe simulation crashed; retrying...",newlines,sep="")
							crash.retry <- crash.retry - 1
							oldadd <- additional
							additional <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = oldadditional$end.state, burnin=burnadapt, sample=(neededupdates), adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE)
							
							if(!any(names(additional)=="crash.end")) break
						}
					}
					
					final.mcmc <- window(combine.mcmc(list(final.mcmc, additional$mcmc)), start=(((niter(final.mcmc)+niter(additional$mcmc))-startsample)+1))
					
					cat("Calculating the Gelman-Rubin statistic....\n")
					thrownaway <- thrownaway+neededupdates
					suppressWarnings(success <- try(convergence <- safe.gelman.diag(normalise.mcmc(final.mcmc, normalise=normalise.mcmc, warn=FALSE), transform=FALSE, autoburnin=TRUE), silent=TRUE))
					if(class(success)=="try-error"){
						cat("An error occured while calculating the Gelman-Rubin statistic; aborting simulation.  Check that different chains have not been given the same starting values and random seeds.\n")
						return(c("Error", "An error occured while calculating the Gelman-Rubin statistic"))
					}
					suppressWarnings(success <- try(autocorr <- autocorr.diag(normalise.mcmc(final.mcmc, normalise=FALSE, warn=FALSE)), silent=TRUE))
					if(class(success)=="try-error"){
						cat("An error occured while calculating the autocorrelation; aborting simulation\n")
						return(c("Error", "An error occured while calculating the autocorrelation"))
					}
					
					options(show.error.messages = FALSE)
					suppressWarnings(fsummary <- summary(final.mcmc))
					options(show.error.messages = TRUE)
					
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
						}	
					}
#unconverged <- 1	
						
					if(unconverged > 0){
						cat("The Gelman-Rubin statistic was still above ", psrf.target, " for ", unconverged, " parameter(s) after ", updatesdone + neededupdates, " iterations (multi-variate psrf = ", as.numeric(round(convergence$mpsrf, digits=3)), ").\n", sep="")
						
						stop <- time.taken > max.time
						if(interactive) stop <- !ask("Extend the simulation to attempt to improve convergence?")
						if(stop){
							cat("Returning UNCONVERGED simulation results\n\n")
							return(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=NA, req.burnin=NA, pilot.summary=fsummary, samples.to.conv=NA, psrf=convergence, autocorr=autocorr))
						}else{
							cat("Extending the simulation to attempt to improve convergence...\n")
							updatesdone <- updatesdone + neededupdates
							updatesthrown <- updatesthrown + neededupdates
						}
						pre.time <- Sys.time()-time.taken
					}else{
						cat(paste("The Gelman-Rubin statistic is now below ", psrf.target, " for all parameters\n", sep=""))
						finishconv <- TRUE
					}
				}else{
					cat("Maximum time limit exceeded; chains still unconverged.  Try restarting the simulation using the end state values of the chains provided\n")
					cat("Returning UNCONVERGED simulation results\n\n")
					return(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=NA, req.burnin=NA, pilot.summary=fsummary, samples.to.conv=NA, psrf=convergence, autocorr=autocorr))
				}
			
			}
	
		}else{
			cat(paste("The Gelman-Rubin statistic is below ", psrf.target, " for all parameters\n", sep=""))
			updatesthrown <- 0
		}
	}else{
		return(c("Error", "The Gelman-Rubin statistic could not be calculated for these chains"))
	}
	
	
	
	cat("\nCalculating the necessary sample length based on the Raftery and Lewis's diagnostic...\n")
			
	success <- try({
	raftery.args$data <- normalise.mcmc(final.mcmc, normalise=FALSE, warn=FALSE)
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
	
	if(any(dependance > 5) & killautocorr==FALSE){
				cat("IMPORTANT:  The sample size(s) of monitored node(s) '", paste(dimnames(dependance)[[1]][apply(dependance, 1, function(x) if(any(x>5)) return(TRUE) else return(FALSE))], collapse="' & '"), "' have a high autocorrelation dependance in chain(s) ", paste(seq(1, n.chains)[apply(dependance, 2, function(x) if(any(x>5)) return(TRUE) else return(FALSE))], collapse= " & "), ".  Re-running the model with a different formulation or better initial values may help to reduce autocorrelation.\n", sep="")
	}
	
	
	moreupdates <- ceiling((max(sample+(max(0, burnin-startburnin)))-startsample)*1.1)
	
	if(moreupdates > 0){

		if((timestring(pre.time, Sys.time(), units="s", show.units=FALSE)+(timestring((time.taken*moreupdates/(startsample+startburnin)), units="s", show.units=FALSE))) < max.time){
			cat("The model will need to be run for a further ", moreupdates, " updates.  This will take approximately ", timestring((time.taken*moreupdates/(startsample+startburnin))), ".\n", sep="")
			if(interactive & (timestring((time.taken*moreupdates/(startsample+startburnin)), units="s", show.units=FALSE)>60)) if(!ask("Continue with the simulation?")){
				cat("Simulation aborted\n\n")
				return(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=max(sample), req.burnin=max(burnin), pilot.summary=fsummary, samples.to.conv=NA, psrf=convergence, autocorr=autocorr))
				
			}
		}else{
			cat("The model will need to be run for a further ", moreupdates, " updates.  This will take approximately ", timestring((time.taken*moreupdates/(startsample+startburnin))), ".\n", sep="")
			#cat("The model would need to be run for a further ", moreupdates, " updates.  This will take approximately ", timestring((time.taken*moreupdates/(startsample+startburnin))), ", taking the total simulation length to over ", timestring(max.time), ".\n", sep="")  NOW NO LONGER ADDING THIS TO TIME LIMIT
			#stop <- TRUE
			if(interactive & (timestring((time.taken*moreupdates/(startsample+startburnin)), units="s", show.units=FALSE)>60)) stop <- !ask("Continue with the simulation?")
			if(stop){
				cat("Simulation aborted\n\n")
				return(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=max(sample), req.burnin=max(burnin), pilot.summary=fsummary, samples.to.conv=NA, psrf=convergence, autocorr=autocorr))
			}
		}
		pre.time <- Sys.time()-time.taken
		cat("\n")
		additional <- run.jags(data=data, model=model, monitor=monitor,  n.chains=n.chains, inits=pilot$end.state, burnin=burnadapt, sample=moreupdates, adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE)
		if(any(additional=="Unable to load coda files")){
			cat("An error occured during the simulation\n\n")
			return(c("Error", "An error occured during the simulation"))
		}
		
		if(any(names(additional)=="crash.end")){
			repeat{
				time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
				if(time.taken > max.time | crash.retry==0){
					cat("The simulation exceeded the number of crashes allowed by crash.retry and so was aborted\n\n")
					return(c("Error", "The simulation was aborted due to crashes"))
				}
				cat("\nThe simulation crashed; retrying...",newlines,sep="")			
				crash.retry <- crash.retry - 1
				oldadd <- additional
				additional <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = pilot$end.state, burnin=burnadapt, sample=moreupdates, adapt=adapt, silent.jags=silent.jags, plots = FALSE, check.conv=FALSE)
				if(any(additional=="Unable to load coda files")){
					cat("An error occured during the simulation\n\n")
					return(c("Error", "An error occured during the simulation"))
				}
				if(!any(names(additional)=="crash.end")) break
			}
		}
		
		final.mcmc <- combine.mcmc(list(final.mcmc, additional$mcmc))
		
		
	}else{
		additional <- pilot # for end.state at the end, pilot is output of either first sim or extended convergence sim
	}	
	
	cat("Necessary sample length achieved\n")
	
	
	final.mcmc <- window(final.mcmc, start=max(burnin))
	n.iters <- niter(final.mcmc)
	
	suppressWarnings(success <- try(convergence <- safe.gelman.diag(normalise.mcmc(final.mcmc, normalise=normalise.mcmc, warn=FALSE), warn=TRUE, transform=FALSE, autoburnin=TRUE), silent=TRUE))
	if(class(success)=="try-error"){
		cat("An error occured while calculating the Gelman-Rubin statistic; aborting simulation.  Check that different chains have not been given the same starting values and random seeds.\n")
		return(c("Error", "An error occured while calculating the Gelman-Rubin statistic"))
	}
	suppressWarnings(success <- try(autocorrelation <- autocorr.diag(normalise.mcmc(final.mcmc, normalise=FALSE, warn=FALSE)), silent=TRUE))
	if(class(success)=="try-error"){
		cat("An error occured while calculating the autocorrelation; aborting simulation\n")
		return(c("Error", "An error occured while calculating the autocorrelation"))
	}
	
	
	options(show.error.messages = FALSE)
	suppressWarnings(fsummary <- summary(final.mcmc))
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
		}	
	}

	if(unconverged > 0 & !killautocorr){
		cat(paste("WARNING:  The Gelman-Rubin statistic for ", unconverged, " parameter(s) are above ", psrf.target, " for the final chains (multi-variate psrf = ", as.numeric(round(convergence$mpsrf, digits=3)), ").  The chains may have fallen out of convergence.  It is important to assess convergence manually before relying on inference from these chains.\n", sep=""))
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
			if(autocorrelated > 0){
				cat("IMPORTANT:  There was a high degree of autocorrelation for ", autocorrelated, " parameter(s)\n", sep="")
			}else{
				#cat("Convergence achieved for this run\n")
			}
		}else{
			cat("Autocorrelation could not be calculated for these chains\n")
		}
		unused <- normalise.mcmc(final.mcmc, normalise=FALSE, warn="warning")  # SO THAT WARNING OF NON STOCHASTICITY IS PRINTED
	}
	
	final.mcmc <- combine.mcmc(final.mcmc, return.samples=thin.sample)


	if(plots==TRUE){
		plot1 = plot2 = vector('list', length=length(varnames(final.mcmc)))
		names(plot1) = names(plot2) <- varnames(final.mcmc)
		thinned.mcmc <- combine.mcmc(list(final.mcmc), return.samples=1000)

		startdev <- dev.list()

		#a <- dev.new()
		#if(options("device")$device=="x11") x11()
	
		for(i in 1:length(varnames(final.mcmc))){
	
			plotdata <- thinned.mcmc[,c(i,i)]
			varnames(plotdata)[1] <- ""		
			plot1[[i]] <- xyplot(plotdata, layout=c(1,1), ylab="Value", xlab="Iteration", lattice.options=list(par.settings=plot.new()))
			class(plot1[[i]]) <- "plotindpages"
			plot2[[i]] <- densityplot(plotdata, layout=c(1,1), ylab="Density", xlab="Value")
			class(plot2[[i]]) <- "plotindpages"
	
		}
	
		if(!is.null(startdev)){
			for(i in dev.list()){
				if(!any(startdev==i)) dev.off(i)
			}
		}else{
			a <- dev.new()
			while(!is.null(dev.list())){
				dev.off()
			}
		}
	}else{
		plot1 = plot2 <- "Plots not produced when plots==FALSE"
	}
	
	cat("Auto-run JAGS complete.\n\n")
	if(killautocorr==FALSE) cat("*PLEASE NOTE:  THIS SOFTWARE IS INTENDED FOR EDUCATIONAL PURPOSES ONLY*\n*YOU SHOULD ASSESS CONVERGENCE AND AUTOCORRELATION MANUALLY BEFORE RELYING ON RESULTS PROVIDED*\n\n")
	
	
	return(list(mcmc=final.mcmc, end.state=additional$end.state, req.samples=max(sample), req.burnin=max(burnin), samples.to.conv=updatesthrown, summary=fsummary, psrf=convergence, autocorr=autocorrelation, trace=plot1, density=plot2))
}
	
autorun.JAGS <- autorun.jags
