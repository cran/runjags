autoextend.jags <- function(runjags.object, add.monitor=character(0), drop.monitor=character(0), drop.chain=numeric(0), combine=length(c(add.monitor,drop.monitor,drop.chain))==0, startburnin = 0, startsample = 10000, psrf.target = 1.05, normalise.mcmc = TRUE, check.stochastic = TRUE, raftery.options = list(), crash.retry=1, summarise = TRUE, confidence=0.95, plots = summarise, thin.sample = FALSE, jags = findjags(), silent.jags = FALSE, interactive=FALSE, max.time=Inf, adaptive=list(type="burnin", length=200), thin = runjags.object$thin, tempdir=TRUE, jags.refresh=0.1, batch.jags=silent.jags, method=NA, method.options=NA){
	

	# We may be passed some unevaluated function arguments so evaluate everything here:
	argnames <- names(formals(autoextend.jags))
	for(i in 1:length(argnames)){
		assign(argnames[i], eval(get(argnames[i])))
	}

	checkvalidrunjagsobject(runjags.object)
	
	# Get unhelpful error messages otherwise:
	if(identical(NA, method)) method <- runjags.object$method
	if(identical(NA, method.options)) method.options <- runjags.object$method.options

	starttime <- Sys.time()
	
	# Not clear jags files to keep for autorun functions so disable:
	keep.jags.files <- FALSE
	directory <- "runjagsfiles"
	
	if(class(silent.jags)=="list"){
		sjl <- silent.jags
		killautocorr <- silent.jags$killautocorr
		silent.jags <- silent.jags$silent.jags
		if(identical(sjl,batch.jags)) batch.jags <- silent.jags
	}else{
		killautocorr <- FALSE
	}

	method <- getrunjagsmethod(method)
	
	jags.status <- testjags(jags, silent=TRUE)
	if(jags.status$JAGS.available==FALSE){
		if(jags.status$os=="windows"){
			# Try it again - sometimes this seems to clear it up:
			Sys.sleep(0.2)
			jags.status <- testjags(jags, silent=TRUE)
		}		
		
		jags <- jags.status$JAGS.path
		if(jags.status$JAGS.available==FALSE){			
			swcat("Unable to call JAGS using '", jags, "' - try specifying the path to the JAGS binary as the jags argument, or installing the rjags package.  Use the testjags() function for more detailed diagnostics.\n", sep="")
			stop("Unable to call JAGS", call.=FALSE)
		}
	}
	jags <- jags.status$JAGS.path
		
	if(!jags.status$JAGS.found && ! method%in%c("rjags","snow")){
		swcat("Unable to call JAGS using '", jags, "' - try specifying the path to the JAGS binary as the jags argument, or using the rjags method.  Use the testjags() function for more detailed diagnostics.\n", sep="")
		stop("Unable to call JAGS", call.=FALSE)
	}
	if(method=="rjags" && !require(rjags)){
		swcat("The rjags package was not found, either install the rjags package or use another method\n", sep="")
		stop("The rjags package was not found", call.=FALSE)
	}
	
		
	# Adjust monitors and chains:

	if(is.na(runjags.object$method) && combine){
		warning("Cannot combine old and new MCMC simulations when given an empty initialised runjags object - setting combine to FALSE")
		combine <- FALSE
		if(is.na(method)) method <- if('rjags' %in% .packages(TRUE)) 'rjags' else 'interruptible'
	}
	if(combine && runjags.object$thin != thin) stop("Cannot combine old and new MCMC simulations when changing the thin interval")
	if(length(c(add.monitor,drop.monitor,drop.chain))!=0 && combine) stop("Cannot combine old and new MCMC simulations when adding or removing monitors or chains")
	
	monitor <- runjags.object$monitor
	modules <- runjags.object$modules
	if(!all(is.character(modules))) stop("The vector provided for 'modules' must be a character vector naming one or more modules to be loaded in JAGS")
	factories <- runjags.object$factories
	if(!all(is.character(factories)) || !all(factories=="" | (grepl("(",factories,fixed=TRUE) & grepl(")",factories,fixed=TRUE)))) stop("The vector provided for 'factories' must be a character vector naming one or more factories to be loaded in JAGS with the following format:  <facname>(<factype>)")
	
	if(length(drop.monitor)>0){
		remove <- which(monitor %in% drop.monitor)
		if(length(remove)!=length(drop.monitor)) warning(paste("Specified variables ", paste(drop.monitor[!(drop.monitor %in% monitor)], collapse=", "), " were not being monitored so could not be dropped", sep=""))
		if(length(remove)>0) monitor <- monitor[-remove]
	}	
	
	if(length(add.monitor)>0){
		monitor <- c(monitor, add.monitor)
		if(any(tolower(monitor)=="dic")){
			monitor <- c(monitor, "pd", "deviance")
		}
		if(any(tolower(monitor)=="deviance") & (any(tolower(monitor)=="pd") | any(tolower(monitor)=="popt"))){
			monitor <- c(monitor, "dic")
		}
	
		monitor[monitor=="DIC"] <- "dic"
		monitor[monitor=="pD"] <- "pd"
		monitor[monitor=="pD.i"] <- "pd.i"
		monitor[monitor=="pOpt"] <- "popt"	
		monitor <- unique(monitor)
		monitor <- na.omit(monitor[monitor!=""])
	
		if(any(c("popt", "pd", "pd.i", "deviance", "dic") %in% monitor)){
			modules <- c(modules, "dic")
		}
		modules[modules=="DIC"] <- "dic"
		modules <- unique(modules)
		modules <- na.omit(modules[modules!=""])
	}
	
	if(any(c("popt", "pd.i") %in% monitor)){
		warning("Cannot monitor popt or pd.i with automatic run length functions - the specified monitor(s) have been removed")
		monitor <- monitor[monitor!="pd.i"]
		monitor <- monitor[monitor!="popt"]
	}
	
	if(any(monitor=="deviance") && as.character(runjags.object$data)==""){
		warning("Unable to monitor deviance with no data - removing the deviance monitor (and DIC/pd/popt/pd.i monitors if set)")
		monitor <- monitor[!monitor%in%c("deviance","pd","pd.i","popt","dic")]
	}

	if(any(grepl("lecuyer::RngStream",runjags.object$end.state))){
		modules <- c(modules, "lecuyer")
		modules <- unique(modules)		
	}
	
	if(length(drop.chain)>0){
		drop.chain <- unique(round(drop.chain))
		if(any(drop.chain>length(runjags.object$end.state)) | any(drop.chain < 1)) stop("Specified value(s) to drop.chain are invalid - please specify which chain(s) to drop by the chain number(s)")
		if(length(drop.chain)==length(runjags.object$end.state)) stop("Specified value(s) to drop.chains are invalid - it is not possible to drop all chains")
		inits <- runjags.object$end.state[-drop.chain]
	}else{
		inits <- runjags.object$end.state
	}
	if(length(grep('base::Mersenne-Twister', inits)>0) & as.numeric(jags.status$JAGS.version) < 2) warning('Using the RNG "base::Mersenne-Twister" (used by default for chain 4) may cause problems with restarting subsequent simulations using the end state of previous simulations due to a bug in JAGS version 1.x.  If you encounter the error "Invalid .RNG.state", please update JAGS to version 2.x and try again.  Or, you can change the random number generator by changing the .RNG.name to (for example) "base::Super-Duper" and remove the .RNG.state element of the list.')
		
	n.chains <- length(inits)
	if(n.chains < 2) stop("The number of chains must be 2 or more so that convergence can be assessed",call.=FALSE)
				
	monitor[monitor==""] <- NA
	if(class(monitor)!="character" | all(is.na(monitor))){
		stop("Monitored variable(s) must be provided in the form of a character vector")
	}
	monitor <- sort(monitor)
		
	if(length(confidence)!=1 || confidence < 0 || confidence > 1) stop("The value supplied for 'confidence' must be between 0 and 1", call.=FALSE)
		
	if(n.chains > 1 && all(runjags.object$end.state=="")){
		warning("No initial values were provided - using the same initial values for all chains", call.=FALSE)
	}else{
		if(n.chains > 1 && all(runjags.object$end.state==runjags.object$end.state[1])){
			warning("Identical initial values were provided for each chain - this is not recommended and may result in false convergence", call.=FALSE)
		}			
	}
	
	if(as.integer(thin)!=thin | thin < 1) stop("The value supplied for thin must be a positive integer")
	
	if(thin.sample==TRUE) thin.sample <- startsample
	if(thin.sample==FALSE) thin.sample <- Inf
	
	if(startsample < 4000) stop("A startsample of 4000 or more iterations (after thinning) is required to complete the Raftery and Lewis's diagnostic")
	
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
		if(adaptive$type!="burnin") warning("Adaptive type not recognised - choose one of 'burnin' or 'adapt'")
		burnadapt <- adaptive$length
		adapt <- 0
	}
	
	newlines <- if(silent.jags) "\n" else "\n\n"
	
	
	# Check to see if this is using an rjags method, and if it is get the method.options$rjags stuff set up:	
	if(method %in% c("background","bgparallel")) stop("The method specified to autorun.jags and autoextend.jags must run JAGS and wait for the results (ie the background method, and possibly other user specified methods, cannot be used)")
	if(method=="rjags"){
		if(keep.jags.files) stop("Unable to keep JAGS files when using the 'rjags' method")
		
		if(! 'rjags' %in% names(method.options)){
			method.options <- c(method.options, list(rjags=as.jags(runjags.object)))
		}
		checkcompiled <- try(coef(method.options$rjags),silent=TRUE)
		if(class(checkcompiled)=="try-error"){
			if(grepl("must be recompiled",checkcompiled)){
				if(silent.jags){
					o <- capture.output(method.options$rjags$recompile()) 
				}else{
					swcat("Compiling rjags model...\n")
				 	method.options$rjags$recompile()
				}
			}else{
				stop(paste("There was an error creating the rjags method for this JAGS model:  ", as.character(checkcompiled), sep=""))
			}
		}
	}else{
		method.options <- method.options[names(method.options)!="rjags"]	
	}
	# popt and pd.i are guaranteed not to be a problem for autorun functions
	
	swcat("\nAuto-run JAGS",newlines,sep="")
	
	if(startsample>runjags.object$sample | !combine){
		
		swcat("Running a pilot chain...\n")
		starttime <- Sys.time()
		
		if(combine){
			initialsample <- startsample - runjags.object$sample
			totalburnin <- runjags.object$burnin + startburnin
		}else{
			initialsample <- startsample
			totalburnin <- startburnin+runjags.object$burnin+runjags.object$sample
		}

		# Call function to run simulation and return MCMC list and pd, popt, pd.i objects:
		startinfo <- runjags.start(model=runjags.object$model, monitor=monitor, data=runjags.object$data, inits=inits, modules=modules, factories=runjags.object$factories, burnin = startburnin, sample = initialsample*thin, adapt=adapt, thin = thin, tempdir=tempdir, dirname=directory, method=method, method.options=method.options, internal.options=list(jags = jags, silent.jags = silent.jags, jags.refresh=jags.refresh, batch.jags=batch.jags))
		if(!startinfo$complete) stop("The method specified to autorun.jags and autoextend.jags must run JAGS and wait for the results (ie the background method, and possibly other user specified methods, cannot be used)")
		
		if(all(c("mcmc","end.state") %in% names(startinfo))){
			if(!"pd" %in% names(startinfo)) startinfo$pd <- NA
			if(!"pd.i" %in% names(startinfo)) startinfo$pd.i <- NA
			if(!"popt" %in% names(startinfo)) startinfo$popt <- NA
			additional <- list(mcmc=startinfo$mcmc, pd=startinfo$pd, popt=startinfo$popt, pd.i=startinfo$pd.i, end.state=startinfo$end.state)
		}else{
			additional <- runjags.readin(directory=startinfo$directory, copy=(keep.jags.files & tempdir), delete=!(keep.jags.files & !tempdir), silent.jags=silent.jags, target.iters=initialsample, n.chains=length(inits), monitor=monitor, method=method, method.options=method.options, suspended=FALSE)
		}
		
		if(niter(additional$mcmc) < initialsample){
			repeat{
				time.taken <- timestring(starttime, Sys.time(), units="secs", show.units=FALSE)
				if(time.taken > max.time | crash.retry==0){
					stop("The simulation exceeded the number of crashes allowed by crash.retry and so was aborted", call.=FALSE)
				}
				swcat("\nThe simulation crashed; retrying...",newlines,sep="")			
				crash.retry <- crash.retry - 1
				startinfo <- runjags.start(model=runjags.object$model, monitor=monitor, data=runjags.object$data, inits=inits, modules=modules, factories=runjags.object$factories, burnin = startburnin, sample = initialsample*thin, adapt=adapt, thin = thin, tempdir=tempdir, dirname=directory, method=method, method.options=method.options, internal.options=list(jags = jags, silent.jags = silent.jags, jags.refresh=jags.refresh, batch.jags=batch.jags))
				if(!startinfo$complete) stop("The method specified to autorun.jags and autoextend.jags must run JAGS and wait for the results (ie the background method, and possibly other user specified methods, cannot be used)")
				
				if(all(c("mcmc","end.state") %in% names(startinfo))){
					if(!"pd" %in% names(startinfo)) startinfo$pd <- NA
					if(!"pd.i" %in% names(startinfo)) startinfo$pd.i <- NA
					if(!"popt" %in% names(startinfo)) startinfo$popt <- NA
					additional <- list(mcmc=startinfo$mcmc, pd=startinfo$pd, popt=startinfo$popt, pd.i=startinfo$pd.i, end.state=startinfo$end.state)
				}else{
					additional <- runjags.readin(directory=startinfo$directory, copy=(keep.jags.files & tempdir), delete=!(keep.jags.files & !tempdir), silent.jags=silent.jags, target.iters=initialsample, n.chains=length(inits), monitor=monitor, method=method, method.options=method.options, suspended=FALSE)
				}
				
				
				if(niter(additional$mcmc) == initialsample) break
			}
		}
		
		# Combine runjags objects if necessary:
		if(combine){
			# Can't have pd.i and popt if combining so don't need to worry about them, but do need to combine pd if necessary:
			if(any(monitor=="pd")){
				pilot <- list(mcmc=combine.mcmc(list(runjags.object$mcmc, additional$mcmc), collapse.chains=FALSE), pd=combine.mcmc(list(runjags.object$pd, additional$pd), collapse.chains=FALSE), popt=NA, pd.i=NA, end.state=additional$end.state)
			}else{
				pilot <- list(mcmc=combine.mcmc(list(runjags.object$mcmc, additional$mcmc), collapse.chains=FALSE), pd=NA, popt=NA, pd.i=NA, end.state=runjags.object$end.state)				
			}
		}else{

			newmcmc <- additional$mcmc
			newthin <- thin(newmcmc)
			newiters <- (as.numeric(dimnames(newmcmc[[1]])[[1]])-(as.numeric(dimnames(newmcmc[[1]])[[1]])[1]-1))+startburnin+runjags.object$burnin+runjags.object$sample
			
			if(class(newmcmc)=="mcmc.list"){
				for(i in 1:length(newmcmc)){
					dimnames(newmcmc[[i]])[[1]] <- newiters
					newmcmc[[i]] <- mcmc(newmcmc[[i]], start=newiters[1], thin=newthin)
				}					
			}
			pilot <- list(mcmc=newmcmc, pd=additional$pd, popt=additional$popt, pd.i=additional$pd.i, end.state=additional$end.state)			

		}
		additional <- pilot
		rm(pilot)
		
		swcat("\n")
	
	}else{
		initialsample <- runjags.object$sample
		totalburnin <- runjags.object$burnin
		additional <- runjags.object
	}
	firsttimetaken = time.taken <- timestring(starttime, Sys.time(), units="secs", show.units=FALSE)

	last.end.state <- additional$end.state
	
	swcat("Calculating the Gelman-Rubin statistic for ", nvar(additional$mcmc), " variables....\n", sep="")
	
	suppressWarnings(success <- try(convergence <- safe.gelman.diag(normalise.mcmcfun(additional$mcmc, normalise=normalise.mcmc, warn=FALSE, check.stochastic = check.stochastic), transform=FALSE, autoburnin=FALSE), silent=TRUE))
	if(class(success)=="try-error"){
		stop("An error occured while calculating the Gelman-Rubin statistic.  Check that different chains have not been given the same starting values and random seeds.",call.=FALSE)
	}
	
	convergence <- c(convergence, psrf.target=psrf.target)
	class(convergence) <- "gelman.with.target"
	
	n.params <- nrow(convergence$psrf)
	n.iters <- niter(additional$mcmc)
	
	if(n.params==1) convergence$mpsrf <- convergence$psrf[1,1]
	unconverged <- 0

	for(j in 1:n.params){
		param.conv <- convergence$psrf[j, 1]
		if(!is.na(param.conv)){
			if(param.conv > psrf.target){
				unconverged <- unconverged + 1
			}
		}else{
			warning(paste("The Gelman-Rubin statistic for '", varnames(additional$mcmc)[j], "' could not be calculated", sep=""))
		}
	}

	updatesdone <- startsample
	updatesthrown <- runjags.object$burnin + startburnin
	
	if(unconverged > 0){
		if(class(convergence$mpsrf)!="numeric"){
			mpsrfstring <- " (Unable to calculate the multi-variate psrf)"
		}else{
			mpsrfstring <- paste(" (multi-variate psrf = ", round(convergence$mpsrf, digits=3), ")", sep="")
		}
			
		swcat("The Gelman-Rubin statistic was above ", psrf.target, " for ", unconverged, " parameter", if(unconverged>1) "s", " after ", n.iters, " iterations", mpsrfstring, ".  This may indicate poor convergence.\n", sep="")
		updatesthrown <- updatesthrown + startsample
		time.taken <- timestring(starttime, Sys.time(), units="secs", show.units=FALSE)
		stop <- time.taken > max.time
			
		if(interactive){
			time.taken <- timestring(starttime, Sys.time(), units="secs", show.units=FALSE)
			stop <- !ask("Extend the simulation to attempt to improve convergence?")
			starttime <- Sys.time()-time.taken
		}
		if(stop){
				
			swcat("Calculating autocorrelation and summary statistics...\n")
				
			summaries <- runjags.summaries(mcmclist=additional$mcmc, pd=additional$pd, popt=additional$popt, pd.i=additional$pd.i, monitor=monitor, plots = plots, psrf.target = psrf.target, normalise.mcmc = normalise.mcmc, check.stochastic = check.stochastic, confidence=confidence, silent=FALSE)
				
			# Use pre-calcuated convergence:
			summaries$psrf <- convergence
	
			combinedoutput <- c(list(mcmc=additional$mcmc, pd=additional$pd, popt=additional$popt, pd.i=additional$pd.i), summaries, list(end.state=last.end.state, burnin=updatesthrown, sample=niter(additional$mcmc), thin=thin), summaries, list(model=runjags.object$model, data=runjags.object$data, monitor=runjags.object$monitor, modules=modules, factories=runjags.object$factories, method=method, method.options=method.options, timetaken=(difftime(Sys.time(), starttime) + runjags.object$timetaken)))
			class(combinedoutput) <- 'runjags'
	
			swcat("Returning UNCONVERGED simulation results\n\n")
			return(combinedoutput)
		}else{
			finishconv <- FALSE
			swcat("Extending the simulation to attempt to improve convergence...\n")
			thrownaway <- 0
		}
						
		while(finishconv==FALSE){
			
			time.taken <- timestring(starttime, Sys.time(), units="secs", show.units=FALSE)
			availableupdates <- updatesdone / time.taken * (max.time - time.taken)
			neededupdates <- max(as.numeric(round(min(availableupdates, startsample))), 2)
			# if less updates are available than startsample, then some of the last run are used to make the total up to startsample using window() in about 35 lines time
				
			if(neededupdates > 0){
				
				totalburnin <- totalburnin + startsample*thin
					
				startinfo <- runjags.start(model=runjags.object$model, monitor=monitor, data=runjags.object$data, inits=last.end.state, modules=modules, factories=runjags.object$factories, burnin = burnadapt, sample = startsample*thin, adapt=adapt, thin = thin, tempdir=tempdir, dirname=directory, method=method, method.options=method.options, internal.options=list(jags = jags, silent.jags = silent.jags, jags.refresh=jags.refresh, batch.jags=batch.jags))
				if(!startinfo$complete) stop("The method specified to autorun.jags and autoextend.jags must run JAGS and wait for the results (ie the background method, and possibly other user specified methods, cannot be used)")

				if(all(c("mcmc","end.state") %in% names(startinfo))){
					if(!"pd" %in% names(startinfo)) startinfo$pd <- NA
					if(!"pd.i" %in% names(startinfo)) startinfo$pd.i <- NA
					if(!"popt" %in% names(startinfo)) startinfo$popt <- NA
					additional <- list(mcmc=startinfo$mcmc, pd=startinfo$pd, popt=startinfo$popt, pd.i=startinfo$pd.i, end.state=startinfo$end.state)
				}else{
					additional <- runjags.readin(directory=startinfo$directory, copy=(keep.jags.files & tempdir), delete=!(keep.jags.files & !tempdir), silent.jags=silent.jags, target.iters=initialsample, n.chains=length(inits), monitor=monitor, method=method, method.options=method.options, suspended=FALSE)
				}
				
		
				if(niter(additional$mcmc) < initialsample){
					repeat{
						time.taken <- timestring(starttime, Sys.time(), units="secs", show.units=FALSE)
						if(time.taken > max.time | crash.retry==0){
							stop("The simulation exceeded the number of crashes allowed by crash.retry and so was aborted", call.=FALSE)
						}
						swcat("\nThe simulation crashed; retrying...",newlines,sep="")			
						crash.retry <- crash.retry - 1
						startinfo <- runjags.start(model=runjags.object$model, monitor=monitor, data=runjags.object$data, inits=last.end.state, modules=modules, factories=runjags.object$factories, burnin = burnadapt, sample = startsample*thin, adapt=adapt, thin = thin, tempdir=tempdir, dirname=directory, method=method, method.options=method.options, internal.options=list(jags = jags, silent.jags = silent.jags, jags.refresh=jags.refresh, batch.jags=batch.jags))
						if(!startinfo$complete) stop("The method specified to autorun.jags and autoextend.jags must run JAGS and wait for the results (ie the background method, and possibly other user specified methods, cannot be used)")
						if(all(c("mcmc","end.state") %in% names(startinfo))){
							if(!"pd" %in% names(startinfo)) startinfo$pd <- NA
							if(!"pd.i" %in% names(startinfo)) startinfo$pd.i <- NA
							if(!"popt" %in% names(startinfo)) startinfo$popt <- NA
							additional <- list(mcmc=startinfo$mcmc, pd=startinfo$pd, popt=startinfo$popt, pd.i=startinfo$pd.i, end.state=startinfo$end.state)
						}else{
							additional <- runjags.readin(directory=startinfo$directory, copy=(keep.jags.files & tempdir), delete=!(keep.jags.files & !tempdir), silent.jags=silent.jags, target.iters=initialsample, n.chains=length(inits), monitor=monitor, method=method, method.options=method.options, suspended=FALSE)
						}
						
				
						if(niter(additional$mcmc) == startsample) break
					}
				}
				
				last.end.state <- additional$end.state

				swcat("Calculating the Gelman-Rubin statistic for ", nvar(additional$mcmc), " variables....\n", sep="")
				thrownaway <- thrownaway+neededupdates
				suppressWarnings(success <- try(convergence <- safe.gelman.diag(normalise.mcmcfun(additional$mcmc, normalise=normalise.mcmc, warn=FALSE, check.stochastic = check.stochastic), transform=FALSE, autoburnin=FALSE), silent=TRUE))
				if(class(success)=="try-error"){
					stop("An error occured while calculating the Gelman-Rubin statistic.  Check that different chains have not been given the same starting values and random seeds.", call.=FALSE)
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
						warning(paste("The Gelman-Rubin statistic for '", varnames(additional$mcmc)[j], "' could not be calculated", sep=""))
					}	
				}
										
				if(class(convergence$mpsrf)!="numeric"){
					mpsrfstring <- " (Unable to calculate the multi-variate psrf)"
				}else{
					mpsrfstring <- paste(" (multi-variate psrf = ", round(convergence$mpsrf, digits=3), ")", sep="")
				}
					
				if(unconverged > 0){
					swcat("The Gelman-Rubin statistic was still above ", psrf.target, " for ", unconverged, " parameter", if(unconverged>1) "s", " after ", updatesdone + neededupdates, " iterations", mpsrfstring, ".\n", sep="")
						
					stop <- time.taken > max.time
					if(interactive){
						time.taken <- timestring(starttime, Sys.time(), units="secs", show.units=FALSE)
						stop <- !ask("Extend the simulation to attempt to improve convergence?")
						starttime <- Sys.time()-time.taken
					}
					if(stop){
							
						swcat("Calculating autocorrelation and summary statistics...\n")
				
						summaries <- runjags.summaries(mcmclist=additional$mcmc, pd=additional$pd, popt=additional$popt, pd.i=additional$pd.i, monitor=monitor, plots = plots, psrf.target = psrf.target, normalise.mcmc = normalise.mcmc, check.stochastic = check.stochastic, confidence=confidence, silent=FALSE)
				
						# Use pre-calcuated convergence:
						summaries$psrf <- convergence
	
						combinedoutput <- c(list(mcmc=additional$mcmc, pd=additional$pd, popt=additional$popt, pd.i=additional$pd.i), summaries, list(end.state=last.end.state, burnin=updatesthrown, sample=niter(additional$mcmc), thin=thin), summaries, list(model=runjags.object$model, data=runjags.object$data, monitor=runjags.object$monitor, modules=modules, factories=runjags.object$factories, method=method, method.options=method.options, timetaken=(difftime(Sys.time(), starttime) + runjags.object$timetaken)))
						class(combinedoutput) <- 'runjags'
	
						swcat("Returning UNCONVERGED simulation results\n\n")
						return(combinedoutput)
							
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
				
				summaries <- runjags.summaries(mcmclist=additional$mcmc, pd=additional$pd, popt=additional$popt, pd.i=additional$pd.i, monitor=monitor, plots = plots, psrf.target = psrf.target, normalise.mcmc = normalise.mcmc, check.stochastic = check.stochastic, confidence=confidence, silent=FALSE)
				
				# Use pre-calcuated convergence:
				summaries$psrf <- convergence
	
				combinedoutput <- c(list(mcmc=additional$mcmc, pd=additional$pd, popt=additional$popt, pd.i=additional$pd.i), summaries, list(end.state=last.end.state, burnin=updatesthrown, sample=niter(additional$mcmc), thin=thin), summaries, list(model=runjags.object$model, data=runjags.object$data, monitor=runjags.object$monitor, modules=modules, factories=runjags.object$factories, method=method, method.options=method.options, timetaken=(difftime(Sys.time(), starttime) + runjags.object$timetaken)))
				class(combinedoutput) <- 'runjags'
	
				swcat("Returning UNCONVERGED simulation results\n\n")
				return(combinedoutput)
			}
			
		}
	
	}else{
		swcat(paste("The Gelman-Rubin statistic is below ", psrf.target, " for all parameters\n", sep=""))
	}	
	
	
	swcat("\nCalculating the necessary sample length based on the Raftery and Lewis's diagnostic...\n")
			
	success <- try({
	raftery.args$data <- normalise.mcmcfun(additional$mcmc, normalise=FALSE, warn=FALSE, check.stochastic = check.stochastic)
	class(raftery.args) <- "list"
	raftery <- do.call("raftery.diag", raftery.args)
	})
	
	if(class(success)=="try-error") stop("An error occured while calculating the Raftery and Lewis's diagnostic",call.=FALSE)
	if(raftery[[1]]$resmatrix[1]=="error") stop("Error", "An error occured while calculating the Raftery and Lewis diagnostic",call.=FALSE)
	
	# to correct for monitoring arrays and non-stochastic nodes:
	newmonitor <- dimnames(raftery[[1]]$resmatrix)[[1]]
	
	dependance = burnin = sample <- matrix(ncol=n.chains, nrow=length(newmonitor), dimnames=list(dimnames(raftery[[1]]$resmatrix)[[1]], 1:n.chains))
	
	for(i in 1:n.chains){	
		dependance[,i] <- raftery[[i]]$resmatrix[,"I"]
		burnin[,i] <- raftery[[i]]$resmatrix[,"M"]
		sample[,i] <- raftery[[i]]$resmatrix[,"N"]
	}
	
	dependancethreshold <- 3
	
#	if(any(dependance > dependancethreshold) & killautocorr==FALSE){
#		swcat("IMPORTANT:  The sample size of monitored node(s) '", paste(dimnames(dependance)[[1]][apply(dependance, 1, function(x) if(any(x>dependancethreshold)) return(TRUE) else return(FALSE))], collapse="' & '"), "' have a high autocorrelation dependance in chain(s) ", paste(seq(1, n.chains)[apply(dependance, 2, function(x) if(any(x>dependancethreshold)) return(TRUE) else return(FALSE))], collapse= " & "), ".  Re-running the model with a different formulation or better initial values may help to reduce autocorrelation.\n", sep="")
#	}

	moreupdates <- max(((max(sample) - (niter(additional$mcmc)*n.chains)) / n.chains), 0)
	if(moreupdates == 1) moreupdates <- 2	
	moreupdates <- ceiling(moreupdates/thin)*thin
	
	if(moreupdates > 0){
			
		firstpart <- additional
			
		success <- try({
			testmatrix <- matrix(nrow=(max(sample)/thin)*n.chains, ncol=length(newmonitor)+as.numeric(any(monitor=="deviance")))
			if(any(monitor=="pd")) testmatrix2 <- matrix(nrow=(max(sample)/thin), ncol=1) else testmatrix4 <- NA
			rm(testmatrix)
			if(any(monitor=="pd")) rm(testmatrix2)
		})
		if(class(success)=="try-error"){
			stop(paste("The model needs to be run for a further ", moreupdates, " iterations.  This would create a vector too large to be read into R.  Try re-parameterising the model to reduce autocorrelation, using the thin option to reduce autocorrelation, or monitoring less variables.  The simulation can be restarted using the chain end states provided", sep=""), call.=FALSE)
		}
					
		swcat("The model will need to be run for a further ", moreupdates, " updates.  This will take approximately ", timestring((firsttimetaken*moreupdates/(startsample+startburnin))), ".\n", sep="")
		if(interactive & (timestring((time.taken*moreupdates/(startsample+startburnin)), units="s", show.units=FALSE)>60)){
			time.taken <- timestring(starttime, Sys.time(), units="secs", show.units=FALSE)
			if(!ask("Continue with the simulation?")){
					
				swcat("Calculating autocorrelation and summary statistics...\n")
				
				summaries <- runjags.summaries(mcmclist=additional$mcmc, pd=additional$pd, popt=additional$popt, pd.i=additional$pd.i, monitor=monitor, plots = plots, psrf.target = psrf.target, normalise.mcmc = normalise.mcmc, check.stochastic = check.stochastic, confidence=confidence, silent=FALSE)
				
				# Use pre-calcuated convergence:
				summaries$psrf <- convergence
	
				combinedoutput <- c(list(mcmc=additional$mcmc, pd=additional$pd, popt=additional$popt, pd.i=additional$pd.i), summaries, list(end.state=last.end.state, burnin=updatesthrown, sample=niter(additional$mcmc), thin=thin), summaries, list(model=runjags.object$model, data=runjags.object$data, monitor=runjags.object$monitor, modules=modules, factories=runjags.object$factories, method=method, method.options=method.options, timetaken=(difftime(Sys.time(), starttime) + runjags.object$timetaken)))
				class(combinedoutput) <- 'runjags'
	
				swcat("Simulation aborted\n\n")
				return(combinedoutput)
					
			}
			starttime <- Sys.time()-time.taken
		}
		
		swcat("\n")
			
		startinfo <- runjags.start(model=runjags.object$model, monitor=monitor, data=runjags.object$data, inits=last.end.state, modules=modules, factories=runjags.object$factories, burnin = burnadapt, sample = moreupdates*thin, adapt=adapt, thin = thin, tempdir=tempdir, dirname=directory, method=method, method.options=method.options, internal.options=list(jags = jags, silent.jags = silent.jags, jags.refresh=jags.refresh, batch.jags=batch.jags))
		if(!startinfo$complete) stop("The method specified to autorun.jags and autoextend.jags must run JAGS and wait for the results (ie the background method, and possibly other user specified methods, cannot be used)")

		if(all(c("mcmc","end.state") %in% names(startinfo))){
			if(!"pd" %in% names(startinfo)) startinfo$pd <- NA
			if(!"pd.i" %in% names(startinfo)) startinfo$pd.i <- NA
			if(!"popt" %in% names(startinfo)) startinfo$popt <- NA
			additional <- list(mcmc=startinfo$mcmc, pd=startinfo$pd, popt=startinfo$popt, pd.i=startinfo$pd.i, end.state=startinfo$end.state)
		}else{
			additional <- runjags.readin(directory=startinfo$directory, copy=(keep.jags.files & tempdir), delete=!(keep.jags.files & !tempdir), silent.jags=silent.jags, target.iters=moreupdates, n.chains=length(inits), monitor=monitor, method=method, method.options=method.options, suspended=FALSE)
		}
		
		
		if(niter(additional$mcmc) < moreupdates){
			repeat{
				time.taken <- timestring(starttime, Sys.time(), units="secs", show.units=FALSE)
				if(time.taken > max.time | crash.retry==0){
					stop("The simulation exceeded the number of crashes allowed by crash.retry and so was aborted", call.=FALSE)
				}
				swcat("\nThe simulation crashed; retrying...",newlines,sep="")			
				crash.retry <- crash.retry - 1
				startinfo <- runjags.start(model=runjags.object$model, monitor=monitor, data=runjags.object$data, inits=last.end.state, modules=modules, factories=runjags.object$factories, burnin = burnadapt, sample = moreupdates*thin, adapt=adapt, thin = thin, tempdir=tempdir, dirname=directory, method=method, method.options=method.options, internal.options=list(jags = jags, silent.jags = silent.jags, jags.refresh=jags.refresh, batch.jags=batch.jags))
				if(!startinfo$complete) stop("The method specified to autorun.jags and autoextend.jags must run JAGS and wait for the results (ie the background method, and possibly other user specified methods, cannot be used)")

				if(all(c("mcmc","end.state") %in% names(startinfo))){
					if(!"pd" %in% names(startinfo)) startinfo$pd <- NA
					if(!"pd.i" %in% names(startinfo)) startinfo$pd.i <- NA
					if(!"popt" %in% names(startinfo)) startinfo$popt <- NA
					additional <- list(mcmc=startinfo$mcmc, pd=startinfo$pd, popt=startinfo$popt, pd.i=startinfo$pd.i, end.state=startinfo$end.state)
				}else{
					additional <- runjags.readin(directory=startinfo$directory, copy=(keep.jags.files & tempdir), delete=!(keep.jags.files & !tempdir), silent.jags=silent.jags, target.iters=moreupdates, n.chains=length(inits), monitor=monitor, method=method, method.options=method.options, suspended=FALSE)
				}
				
				
				if(niter(additional$mcmc) == moreupdates) break
			}
		}
		
		last.end.state <- additional$end.state
		
		if(any(monitor=="pd")){
			combinedoutput <- list(mcmc=combine.mcmc(list(firstpart$mcmc, additional$mcmc), collapse.chains=FALSE, return.samples=thin.sample), pd=combine.mcmc(list(firstpart$pd, additional$pd), collapse.chains=FALSE, return.samples=thin.sample), popt=NA, pd.i=NA)
		}else{
			combinedoutput <- list(mcmc=combine.mcmc(list(firstpart$mcmc, additional$mcmc), collapse.chains=FALSE, return.samples=thin.sample), pd=NA, popt=NA, pd.i=NA)				
		}
			
	}else{
		if(any(monitor=="pd")){
			combinedoutput <- list(mcmc=combine.mcmc(additional$mcmc, collapse.chains=FALSE, return.samples=thin.sample), pd=combine.mcmc(additional$pd, collapse.chains=FALSE, return.samples=thin.sample), popt=NA, pd.i=NA)
		}else{
			combinedoutput <- list(mcmc=combine.mcmc(additional$mcmc, collapse.chains=FALSE, return.samples=thin.sample), pd=NA, popt=NA, pd.i=NA)				
		}
	}	
	
		
	burnin <- totalburnin
	sample <- niter(combinedoutput$mcmc)
	iternames <- seq((burnin+1), (burnin+(sample*thin))-(thin-1), length.out=sample)
	currentdn <- dimnames(combinedoutput$mcmc[[1]]) 
	for(i in 1:length(combinedoutput$mcmc)){
		stopifnot(dim(combinedoutput$mcmc[[i]])[1]==sample)
		dimnames(combinedoutput$mcmc[[i]]) <- list(iternames, currentdn[[2]])
	}
	if(class(combinedoutput$pd)=="mcmc" && !is.na(combinedoutput$pd)){
		dimnames(combinedoutput$pd) <- list(iternames, dimnames(combinedoutput$pd)[[2]])			
	}	
	
	
	swcat("Necessary sample length achieved\n")
	
	if(summarise){
		summaries <- runjags.summaries(mcmclist=combinedoutput$mcmc, pd=combinedoutput$pd, popt=combinedoutput$popt, pd.i=combinedoutput$pd.i, monitor=monitor, plots = plots, psrf.target = psrf.target, normalise.mcmc = normalise.mcmc, check.stochastic = check.stochastic, confidence=confidence, silent=TRUE)
	}else{
		if(any(monitor=="dic")) warning("Cannot calculate DIC automatically when summarise=FALSE")
		if(plots) warning("Cannot produce plots automatically when summarise=FALSE")
		message <- "Summary statistics not produced when summarise=FALSE"	
		summaries <- list(summary=message, HPD=message, hpd=message, mcse=message, psrf=message, autocorr=message, crosscorr=message, stochastic=message, dic=message, trace=message, density=message)
	}
	
	class(last.end.state) <- 'runjags.inits'
	combinedoutput <- c(combinedoutput, list(end.state=last.end.state, burnin=totalburnin, sample=niter(combinedoutput$mcmc), thin=thin), summaries, list(model=runjags.object$model, data=runjags.object$data, monitor=monitor, modules=modules, factories=runjags.object$factories, method=method, method.options=method.options, timetaken=(difftime(Sys.time(), starttime) + runjags.object$timetaken)))
	class(combinedoutput) <- 'runjags'
	
	swcat("Auto-run JAGS complete.\n\n")
	
	return(combinedoutput)
	
}

autoextend.JAGS <- autoextend.jags
