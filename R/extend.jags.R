extend.jags <- function(runjags.object, add.monitor=character(0), drop.monitor=character(0), drop.chain=numeric(0), combine=length(c(add.monitor,drop.monitor,drop.chain))==0, burnin = 0, sample = 10000, adapt=max(200-burnin, 0), jags = findjags(), silent.jags = FALSE, summarise = TRUE, confidence=0.95, plots = summarise, psrf.target = 1.05, normalise.mcmc = TRUE, check.stochastic = TRUE, thin = runjags.object$thin, keep.jags.files = FALSE, tempdir=TRUE, jags.refresh=0.1, batch.jags=silent.jags, method=NA, method.options=NA){
	
	# We may be passed some unevaluated function arguments from parent functions using getargs so evaluate everything here:
	argnames <- names(formals(extend.jags))
	for(i in 1:length(argnames)){
		assign(argnames[i], eval(get(argnames[i])))
	}
		
	checkvalidrunjagsobject(runjags.object)
	
	# Get unhelpful error messages otherwise:
	if(identical(NA, method)) method <- runjags.object$method
	if(identical(NA, method.options)) method.options <- runjags.object$method.options
	
	if(length(keep.jags.files)!=1) stop("keep.jags.files must be either TRUE, FALSE or a character string specifying a folder to save results to")
	if(class(tempdir)!="logical") stop("tempdir must be either TRUE or FALSE")
	if(class(keep.jags.files)=="character"){
		tempdir <- FALSE
		directory <- keep.jags.files
		keep.jags.files <- TRUE
	}else{
		directory <- NA
	}
	
	starttime <- Sys.time()
		
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
		
	n.chains <- length(runjags.object$end.state)	
	
	if(sample < 2 && sample!=0) stop(paste("Unable to run the simulation for ", sample, " update(s) - the value supplied for sample must be 2 or more", sep=""))
	if(((sample + combine*runjags.object$sample) < 100) && (summarise || plots)){
		warning("Cannot produce meaningful summary statistics or plots with less than 100 samples - setting both to FALSE")
		summarise <- FALSE
		plots <- FALSE
	} 
	
	if(length(confidence)!=1 || confidence < 0 || confidence > 1) stop("The value supplied for 'confidence' must be between 0 and 1", call.=FALSE)
		
	if(n.chains > 1 && all(runjags.object$end.state=="")){
		warning("No initial values were provided - using the same initial values for all chains", call.=FALSE)
	}else{
		if(n.chains > 1 && all(runjags.object$end.state==runjags.object$end.state[1])){
			warning("Identical initial values were provided for each chain - this is not recommended and may result in false convergence", call.=FALSE)
		}			
	}
	
	if(length(drop.monitor)>0){
		remove <- which(monitor %in% drop.monitor)
		if(length(remove)!=length(drop.monitor)) warning(paste("Specified variables ", paste(drop.monitor[!(drop.monitor %in% monitor)], collapse=", "), " were not being monitored so could not be dropped", sep=""))
		if(length(remove)>0) monitor <- monitor[-remove]
	}	
	
	if(length(add.monitor)>0){
		monitor <- c(monitor, add.monitor)
		if(any(tolower(monitor)=="dic")){
			monitor <- c(monitor, "popt", "pd", "deviance")
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
	
	if(any(c("popt", "pd.i") %in% monitor) & combine) stop("Cannot combine old and new MCMC simulations when monitoring popt or pd.i")
	
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
		
	n.chains <- length(inits)
	if(n.chains<1) stop("Number of chains must be greater than 0")
	
	if(any(monitor=="deviance") && as.character(runjags.object$data)==""){
		warning("Unable to monitor deviance with no data - removing the deviance monitor (and DIC/pd/popt/pd.i monitors if set)")
		monitor <- monitor[!monitor%in%c("deviance","pd","pd.i","popt","dic")]
	}
		
	monitor[monitor==""] <- NA
	if(class(monitor)!="character" | all(is.na(monitor))){
		stop("Monitored variable(s) must be provided in the form of a character vector")
	}
	monitor <- sort(monitor)
		
	if(as.integer(thin)!=thin | thin < 1) stop("The value supplied for thin must be a positive integer")
	
	sample <- sample
	burnin <- burnin
	
	
	# Check to see if this is using an rjags method, and if it is get the method.options$rjags stuff set up:	
	method <- getrunjagsmethod(method)	
	if(keep.jags.files && method=="rjags"){
		warning("Unable to keep JAGS files when using the 'rjags' method - switching to the 'interruptible' method")
		method <- "interruptible"
	}
	if((method=="rjags")){
		
		# Module loading MUST be done before model compilation:
		for(m in modules){
			if(m!=""){
				if(m=="runjags"){
					success <- try(load.module.runjags())
				}else{
					success <- try(load.module(m))
				}
			
				if(class(success)=="try-error") stop(paste("Failed to load the module '", m, "'",sep=""))
			}
		}		
		
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
		if(any(c("popt", "pd.i") %in% monitor)){
			stop("Cannot monitor popt or pd.i with the rjags method.  Try using the interuptible or simple method instead, or removing the pd.i and popt monitors.")
		}					
	}else{
		method.options <- method.options[names(method.options)!="rjags"]	
	}	
	
	# Wrapper to catch sample=0, in which case we are probably just using this as a hack to calculate sumamry statistics:
	if(!(sample==0 & combine)){
		# Call function to run simulation and return MCMC list and pd, popt, pd.i objects:
		startinfo <- runjags.start(model=runjags.object$model, monitor=monitor, data=runjags.object$data, inits=inits, modules=modules, factories=runjags.object$factories, burnin = burnin, sample = sample*thin, adapt=adapt, thin = thin, tempdir=tempdir, dirname=directory, method=method, method.options=method.options, internal.options=list(jags = jags, silent.jags = silent.jags, jags.refresh=jags.refresh, batch.jags=batch.jags))
		
		# Return info for a background / xgrid etc job:
		if(!startinfo$complete){
			# Make a background.runjags.object object and save it - to be used by user in case of emergencies
			
			background.runjags.object <- runjags.object
			if(!combine) background.runjags.object$mcmc <- NA
			background.runjags.object$pd <- NA
			background.runjags.object$popt <- NA
			background.runjags.object$pd.i <- NA
			background.runjags.object$end.state <- NA
			background.runjags.object$thin <- thin
			background.runjags.object$HPD <- NA
			background.runjags.object$hpd <- NA
			background.runjags.object$mcse <- NA
			background.runjags.object$psrf <- NA
			background.runjags.object$autocorr <- NA
			background.runjags.object$crosscorr <- NA
			background.runjags.object$stochastic <- NA
			background.runjags.object$dic <- NA
			background.runjags.object$trace <- NA
			background.runjags.object$density <- NA
			background.runjags.object$thin <- NA
			
			background.runjags.object$monitor <- monitor
			background.runjags.object$method <- method
			background.runjags.object$method.options <- method.options

			background.runjags.object$combine <- combine
			background.runjags.object$summarise <- summarise
			background.runjags.object$plots <- plots
			background.runjags.object$psrf.target <- psrf.target
			background.runjags.object$normalise.mcmc <- normalise.mcmc
			background.runjags.object$check.stochastic <- check.stochastic
			background.runjags.object$silent.jags <- silent.jags
			background.runjags.object$oldburnin <- runjags.object$burnin
			background.runjags.object$oldsample <- runjags.object$sample
			background.runjags.object$burnin <- burnin
			background.runjags.object$sample <- sample
			background.runjags.object$thin <- thin
			background.runjags.object$oldthin <- runjags.object$thin
			background.runjags.object$keep.jags.files <- keep.jags.files
			background.runjags.object$tempdir <- tempdir
			background.runjags.object$inits <- inits
			background.runjags.object$modules <- modules
			background.runjags.object$confidence <- confidence
			
			background.runjags.object$startedon <- starttime
			
			background.runjags.object <- c(background.runjags.object, startinfo)
			
			class(background.runjags.object) <- "runjags.bginfo"
			
			# Return an object of class suspendedrunjags with all these options inside it, and the part of the old runjags object that we still need (eg for joining etc) also inside along with startinfo itself
			
			save(background.runjags.object, file=file.path(startinfo$directory,"jagsinfo.Rsave"))
			return(background.runjags.object)
		}
		
		if(all(c("mcmc","end.state") %in% names(startinfo))){
			if(!"pd" %in% names(startinfo)){
				startinfo$pd <- NA
			}
			if(any(monitor=="pd") && is.na(startinfo$pd)){
				warning("The pD was not returned as expected; removing the pD (and DIC) monitor")
				monitor <- monitor[! monitor %in% c("pd","dic")]
			}
			if(!"pd.i" %in% names(startinfo)){
				startinfo$pd.i <- NA
			}
			if(any(monitor=="pd.i") && is.na(startinfo$pd.i)){
				warning("The pD.i was not returned as expected; removing the pD.i monitor")
				monitor <- monitor[! monitor %in% c("pd.i")]
			}
			if(!"popt" %in% names(startinfo)){
				startinfo$popt <- NA
			}
			if(any(monitor=="popt") && is.na(startinfo$popt)){
				warning("The pOpt was not returned as expected; removing the pOpt monitor")
				monitor <- monitor[! monitor %in% c("popt")]
			}
			newoutput <- list(mcmc=startinfo$mcmc, pd=startinfo$pd, popt=startinfo$popt, pd.i=startinfo$pd.i, end.state=startinfo$end.state)
		}else{
			newoutput <- runjags.readin(directory=startinfo$directory, copy=(keep.jags.files & tempdir), delete=!(keep.jags.files & !tempdir), silent.jags=silent.jags, target.iters=sample, n.chains=length(inits), monitor=monitor, method=method, method.options=method.options, suspended=FALSE)
		}
		
		end.state <- newoutput$end.state
		burnin <- runjags.object$burnin+(thin*runjags.object$sample)+burnin
		
		iternames <- seq((burnin+1), (burnin+(sample*thin))-(thin-1), length.out=sample)
		currentdn <- dimnames(newoutput$mcmc[[1]]) 
		for(i in 1:length(newoutput$mcmc)){
			stopifnot(dim(newoutput$mcmc[[i]])[1]==sample)
			newoutput$mcmc[[i]] <- mcmc(newoutput$mcmc[[i]], start=burnin+1, thin=thin)
			dimnames(newoutput$mcmc[[i]]) <- list(iternames, currentdn[[2]])
		}
		if(class(newoutput$pd)=="mcmc" && !is.na(newoutput$pd)){
			dimnames(newoutput$pd) <- list(iternames, dimnames(newoutput$pd)[[2]])
			newoutput$pd <- mcmc(newoutput$pd, start=burnin+1, thin=thin)		
		}	
		
		# Combine runjags objects if necessary:
		if(combine){
			# Can't have pd.i and popt if combining so don't need to worry about them, but do need to combine pd if necessary:
			if(any(monitor=="pd")){
				combinedoutput <- list(mcmc=combine.mcmc(list(runjags.object$mcmc, newoutput$mcmc), collapse.chains=FALSE), pd=combine.mcmc(list(runjags.object$pd, newoutput$pd), collapse.chains=FALSE), popt=NA, pd.i=NA)
			}else{
				combinedoutput <- list(mcmc=combine.mcmc(list(runjags.object$mcmc, newoutput$mcmc), collapse.chains=FALSE), pd=NA, popt=NA, pd.i=NA)				
			}
		}else{
			combinedoutput <- list(mcmc=newoutput$mcmc, pd=newoutput$pd, popt=newoutput$popt, pd.i=newoutput$pd.i)			
		}
		
		if(combine) burnin <- runjags.object$burnin
		
		# Save some RAM:
		rm(newoutput)
	}else{
		combinedoutput <- list(mcmc=runjags.object$mcmc, pd=runjags.object$pd, popt=runjags.object$popt, pd.i=runjags.object$pd.i)			
		end.state <- runjags.object$end.state
	}
	
	# Call function to calculate summary statistics and plots etc:
	if(summarise){
		summaries <- runjags.summaries(mcmclist=combinedoutput$mcmc, pd=combinedoutput$pd, popt=combinedoutput$popt, pd.i=combinedoutput$pd.i, monitor=monitor, plots = plots, psrf.target = psrf.target, normalise.mcmc = normalise.mcmc, check.stochastic = check.stochastic, confidence=confidence, silent=FALSE)
	}else{
		if(any(monitor=="dic")) warning("Cannot calculate DIC automatically when summarise=FALSE")
		if(plots) warning("Cannot produce plots automatically when summarise=FALSE")
		message <- "Summary statistics not produced when summarise=FALSE"	
		summaries <- list(summary=message, HPD=message, hpd=message, mcse=message, psrf=message, autocorr=message, crosscorr=message, stochastic=message, dic=message, trace=message, density=message)
	}
	
	class(end.state) <- 'runjags.inits'
	
	combinedoutput <- c(combinedoutput, list(end.state=end.state, burnin=burnin, sample=niter(combinedoutput$mcmc), thin=thin), summaries, list(model=runjags.object$model, data=runjags.object$data, monitor=monitor, modules=modules, factories=runjags.object$factories, method=method, method.options=method.options, timetaken=(difftime(Sys.time(), starttime) + runjags.object$timetaken)))
	class(combinedoutput) <- 'runjags'
	
	swcat("Finished running the simulation\n")
	
	return(combinedoutput)
}


results.jags <- function(background.runjags.object){
	
	if(class(background.runjags.object)!="runjags.bginfo") stop("An object produced by a background runjags method must be supplied (see the manual page for more details)")
	if(background.runjags.object$method=="xgrid"){
		warning("Re-routing function call to xgrid.results.jags to retrieve xgrid job")
		return(xgrid.results.jags(background.runjags.object))
	}
	
	if(!file.exists(background.runjags.object$directory)) stop("The JAGS files for the model supplied were not found - they may have been deleted")
	
	# Make name easier to type:
	output=runjags.object <- background.runjags.object
	
	# A copy of the background.runjags.object in case it's needed by the user:
	# load("jagsinfo.Rsave")
	

	newoutput <- runjags.readin(directory=output$directory, copy=(output$keep.jags.files & output$tempdir), delete=!(output$keep.jags.files & !output$tempdir), silent.jags=output$silent.jags, target.iters=output$sample, n.chains=length(output$inits), monitor=output$monitor, method=output$method, method.options=output$method.options, suspended=TRUE)
		

	end.state <- newoutput$end.state
	class(end.state) <- 'runjags.inits'
	
	burnin <- runjags.object$oldburnin+(background.runjags.object$oldthin*background.runjags.object$oldsample)+runjags.object$burnin
	sample <- background.runjags.object$sample
	thin <- background.runjags.object$thin

	iternames <- seq((burnin+1), (burnin+(sample*thin))-(thin-1), length.out=sample)
	currentdn <- dimnames(newoutput$mcmc[[1]]) 
	if(!is.null(currentdn[[1]])){
		stopifnot(currentdn[[1]]==iternames)
	}
	for(i in 1:length(newoutput$mcmc)){
		stopifnot(dim(newoutput$mcmc[[i]])[1]==sample)
		dimnames(newoutput$mcmc[[i]]) <- list(iternames, currentdn[[2]])
	}
	if(class(newoutput$pd)=="mcmc" && !is.na(newoutput$pd)){
		dimnames(newoutput$pd) <- list(iternames, dimnames(newoutput$pd)[[2]])			
	}	

	if(runjags.object$combine) burnin <- runjags.object$oldburnin

	# Combine runjags objects if necessary:
	if(runjags.object$combine){
		# Can't have pd.i and popt if combining so don't need to worry about them, but do need to combine pd if necessary:
		if(any(runjags.object$monitor=="pd")){
			combinedoutput <- list(mcmc=combine.mcmc(list(runjags.object$mcmc, newoutput$mcmc), collapse.chains=FALSE), pd=combine.mcmc(list(runjags.object$pd, newoutput$pd), collapse.chains=FALSE), popt=NA, pd.i=NA)
		}else{
			combinedoutput <- list(mcmc=combine.mcmc(list(runjags.object$mcmc, newoutput$mcmc), collapse.chains=FALSE), pd=NA, popt=NA, pd.i=NA)				
		}
	}else{
		combinedoutput <- list(mcmc=newoutput$mcmc, pd=newoutput$pd, popt=newoutput$popt, pd.i=newoutput$pd.i)			

	}
	# Save some RAM:
	rm(newoutput)
	
	# Call function to calculate summary statistics and plots etc:
	if(runjags.object$summarise){
		summaries <- runjags.summaries(mcmclist=combinedoutput$mcmc, pd=combinedoutput$pd, popt=combinedoutput$popt, pd.i=combinedoutput$pd.i, monitor=runjags.object$monitor, plots = runjags.object$plots, psrf.target = runjags.object$psrf.target, normalise.mcmc = runjags.object$normalise.mcmc, check.stochastic = runjags.object$check.stochastic, confidence=runjags.object$confidence, silent=FALSE)
	}else{
		if(any(runjags.object$monitor=="dic")) warning("Cannot calculate DIC automatically when summarise=FALSE", call.=FALSE)
		if(runjags.object$plots) warning("Cannot produce plots automatically when summarise=FALSE", call.=FALSE)
		message <- "Summary statistics not produced when summarise=FALSE"	
		summaries <- list(summary=message, HPD=message, hpd=message, mcse=message, psrf=message, autocorr=message, crosscorr=message, stochastic=message, dic=message, trace=message, density=message)
	}
	
	combinedoutput <- c(combinedoutput, list(end.state=end.state, burnin=burnin, sample=niter(combinedoutput$mcmc), thin=runjags.object$thin), summaries, list(model=runjags.object$model, data=runjags.object$data, monitor=runjags.object$monitor, modules=runjags.object$modules, factories=runjags.object$factories, method=runjags.object$method, method.options=runjags.object$method.options, timetaken=(difftime(Sys.time(), background.runjags.object$startedon) + background.runjags.object$timetaken)))
	class(combinedoutput) <- 'runjags'
	
	swcat("Finished running the simulation\n")
	
	return(combinedoutput)

}

results.JAGS <- results.jags
extend.JAGS <- extend.jags

