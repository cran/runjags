autorun.jags <- function(model=stop("No model supplied"), monitor = stop("No monitored variables supplied"), data=NA, n.chains=2, inits = replicate(n.chains, NA), startburnin = 5000, startsample = 10000, psrf.target = 1.05, crash.retry=1, thin.sample = FALSE, jags = findjags(), silent.jags = FALSE, interactive=TRUE, max.time=Inf, adaptive=list(type="burnin", length=200)){

	
	if(thin.sample==TRUE) thin.sample <- startsample
	if(thin.sample==FALSE) thin.sample <- Inf
	
	if(startsample < 4000) stop("A startsample of 4000 or more iterations is required to complete the Raftery and Lewis's diagnostic")
		
	pilot <- NA
	
	if(n.chains < 2) stop("Minimum of 2 chains should be used so that convergence can be assessed")
	
	if(psrf.target <= 1) stop("psrf target must be greater than 1")
	
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
	
	
	cat("\nAuto-run JAGS\n\nRunning a pilot chain...\n")
	pre.time <- Sys.time()
	
	
	pilot <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = inits, burnin=startburnin, sample=startsample, adapt=adapt, silent.jags=silent.jags, check.conv=FALSE)
	if(any(pilot=="Unable to load coda files")) return(c("Error", "An error occured during the simulation"))
	
	
	if(any(is.na(pilot))) return(c("Error", "An error occured during the simulation"))
	if(class(pilot)=="character") return(c("Error", "An error occured during the simulation"))
	
	if(any(names(pilot)=="crash.end")){
		repeat{
			time.taken <- difftime(Sys.time(), pre.time, units="secs")
			if(time.taken > max.time | crash.retry==0) return(c("Error", "The simulation was aborted due to crashes"))

			cat("\nThe simulation crashed; retrying...\n\n")			
			crash.retry <- crash.retry - 1
			oldpilot <- pilot
			pilot <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = inits, burnin=startburnin, sample=startsample, adapt=adapt, silent.jags=silent.jags, check.conv=FALSE)
			if(!any(names(pilot)=="crash.end")) break
		}
	}
	
	time.taken <- difftime(Sys.time(), pre.time, units="secs")
	cat("\n")
	
	
	
	final.mcmc <- pilot$mcmc
	
	cat("Calculating the Gelman-Rubin statistic....\n")

	success <- try(convergence <- gelman.diag(final.mcmc))
	if(class(success)=="try-error") return(c("Error", "An error occured while calculating the Gelman-Rubin statistic"))
	success <- try(autocorr <- autocorr.diag(final.mcmc))
	if(class(success)=="try-error") return(c("Error", "An error occured while calculating the autocorrelation"))
	suppressWarnings(fsummary <- summary(final.mcmc))
	
	n.params <- ncol(autocorr)
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
			
			additional <- pilot
			
			while(finishconv==FALSE){
			
				time.taken <- timestring(pre.time, Sys.time(), units="secs", show.units=FALSE)
				availableupdates <- updatesdone / time.taken * (max.time - time.taken)
				neededupdates <- updatesdone / time.taken * 600
				neededupdates <- as.numeric(round(min(neededupdates, availableupdates, startsample)))
			
				if(neededupdates > 0){
					
					oldadditional <- additional
					additional <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits=pilot$end.state, burnin=burnadapt, sample=neededupdates, adapt=adapt, silent.jags=silent.jags, check.conv=FALSE)
					pilot <- additional
					
					if(any(additional=="Unable to load coda files")) return(c("Error", "An error occured during the simulation"))
					if(any(names(additional)=="crash.end")){
						repeat{
							time.taken <- difftime(Sys.time(), pre.time, units="secs")
							if(time.taken > max.time | crash.retry==0) return(c("Error", "The simulation was aborted due to crashes"))

							crash.retry <- crash.retry - 1
							oldadd <- additional
							additional <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = oldadditional$end.state, burnin=burnadapt, sample=(neededupdates), adapt=adapt, silent.jags=silent.jags, check.conv=FALSE)
							
							if(!any(names(additional)=="crash.end")) break
						}
					}
					
					final.mcmc <- window(combine.mcmc(list(final.mcmc, additional$mcmc)), start=neededupdates+1)
					
					thrownaway <- thrownaway+neededupdates
					success <- try(convergence <- gelman.diag(final.mcmc))
					if(class(success)=="try-error") return(c("Error", "An error occured while calculating the Gelman-Rubin statistic"))
					success <- try(autocorr <- autocorr.diag(final.mcmc))
					if(class(success)=="try-error") return(c("Error", "An error occured while calculating the autocorrelation"))
					suppressWarnings(fsummary <- summary(final.mcmc))
					
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
			
	success <- try(raftery <- raftery.diag(final.mcmc))
	
	#print(niter(final.mcmc))
	#print(raftery)
	
	if(class(success)=="try-error") return(c("Error", "An error occured while calculating the Raftery and Lewis's diagnostic"))
	if(raftery[[1]]$resmatrix[1]=="error") return(c("Error", "An error occured while calculating the Raftery and Lewis diagnostic"))
			
	
	# to correct for monitoring arrays:
	monitor <- dimnames(raftery[[1]]$resmatrix)[[1]]
	
	dependance = burnin = sample <- matrix(ncol=n.chains, nrow=length(monitor), dimnames=list(dimnames(raftery[[1]]$resmatrix)[[1]], 1:n.chains))
	
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
			cat("The model would need to be run for a further ", moreupdates, " updates.  This will take approximately ", timestring((time.taken*moreupdates/(startsample+startburnin))), ", taking the total simulation length to over ", timestring(max.time), ".\n", sep="")
			stop <- TRUE
			if(interactive & (timestring((time.taken*moreupdates/(startsample+startburnin)), units="s", show.units=FALSE)>60)) stop <- !ask("Continue with the simulation?")
			if(stop){
				cat("Simulation aborted\n\n")
				return(list(pilot.mcmc=final.mcmc, end.state=pilot$end.state, req.samples=max(sample), req.burnin=max(burnin), pilot.summary=fsummary, samples.to.conv=NA, psrf=convergence, autocorr=autocorr))
			}
			
		}
		cat("\n")
		additional <- run.jags(data=data, model=model, monitor=monitor,  n.chains=n.chains, inits=pilot$end.state, burnin=burnadapt, sample=moreupdates, adapt=adapt, silent.jags=silent.jags, check.conv=FALSE)
		if(any(additional=="Unable to load coda files")) return(c("Error", "An error occured during the simulation"))
		
		if(any(names(additional)=="crash.end")){
			repeat{
				time.taken <- difftime(Sys.time(), pre.time, units="secs")
				if(time.taken > max.time | crash.retry==0) return(c("Error", "The simulation was aborted due to crashes"))
				cat("\nThe simulation crashed; retrying...\n\n")			
				crash.retry <- crash.retry - 1
				oldadd <- additional
				additional <- run.jags(data=data, model=model, monitor=monitor, n.chains=n.chains, inits = pilot$end.state, burnin=burnadapt, sample=moreupdates, adapt=adapt, silent.jags=silent.jags, check.conv=FALSE)
				if(any(additional=="Unable to load coda files")) return(c("Error", "An error occured during the simulation"))
				if(!any(names(additional)=="crash.end")) break
			}
		}
	}else{
		additional <- pilot # for end.state at the end	
	}	
	
	cat("Necessary sample length achieved\n")
	
	final.mcmc <- combine.mcmc(list(pilot$mcmc, additional$mcmc))
	
	additional <- pilot
	
	
	final.mcmc <- window(final.mcmc, start=max(burnin))
	n.iters <- niter(final.mcmc)
	n.params <- nvar(final.mcmc)
	
	if(n.params==1) convergence$mpsrf <- NULL
	
	autocorrelated <- 0
	success <- try(autocorrelation <- autocorr.diag(final.mcmc))
	if(class(success)=="try-error") return(c("Error", "An error occured while calculating the autocorrelation"))
	
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
	}
	suppressWarnings(final.mcmc <- window(final.mcmc, thin=max(1, floor(n.iters/thin.sample))))

	cat("Auto-run JAGS complete.\n\n")
	if(killautocorr==FALSE) cat("*PLEASE NOTE:  THIS SOFTWARE IS INTENDED FOR EDUCATIONAL PURPOSES ONLY*\n*YOU SHOULD ASSESS CONVERGENCE AND AUTOCORRELATION MANUALLY BEFORE RELYING ON RESULTS PROVIDED*\n")
	
	fsummary <- suppressWarnings(summary(final.mcmc))
	
	return(list(mcmc=final.mcmc, end.state=additional$end.state, req.samples=max(sample), req.burnin=max(burnin), samples.to.conv=updatesthrown, summary=fsummary, psrf=convergence, autocorr=autocorrelation))
}
	
autorun.JAGS <- autorun.jags
