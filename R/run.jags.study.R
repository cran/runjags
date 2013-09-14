run.jags.study <- function(simulations, model=NULL, datafunction=NULL, targets=list(), confidence=0.95, record.chains=FALSE, runjags.options=list(), cat.progress=FALSE, test=TRUE, parallel.method=parLapply, ...){
	
	# ... is passed either to autorun.jags (add.monitor not allowed, combine not allowed, maybe others) or to parallel.method function
	
	# Reset failed study:
	failedjags$study <- "No failed study available!"
	
	# Only allow simple or parallel method for autorun.jags - can't start/stop anyway
	
	if(!is.numeric(simulations) || length(simulations)!=1 || simulations!=as.integer(simulations) || simulations<1) stop("The value for simulations must be a single positive integer")
	
	# Force evaluation:
	parallel.method <- parallel.method
	
	# First expand and check the passed arguments match to something in autorun.jags:
	runjags.options <- getargs(c('autorun.jags'), runjags.options, returnall=FALSE)	
	
	# Now remove any passed data list or initlist etc:
	if(any(names(runjags.options)=="datalist")) warning("A datalist supplied in the runjags.options list was ignored")
	runjags.options <- runjags.options[names(runjags.options)!="datalist"]
	
	if(any(names(runjags.options)=="silent.jags")) warning("The silent.jags option supplied in the runjags.options list was ignored - suppression of JAGS output is always disabled for the run.jags.study function")
	runjags.options$silent.jags <- TRUE
	runjags.options$batch.jags <- TRUE
	
	if(!any(names(runjags.options)=="method")){
		runjags.options$method <- expression(if('rjags' %in% .packages(TRUE)) 'rjags' else 'interruptible')
	}else{
		runjags.options$method <- getrunjagsmethod(runjags.options$method)
	}
	
	if(!any(names(runjags.options)=="plots")){
		runjags.options$plots <- FALSE
		if(!any(names(runjags.options)=="summarise")){
			runjags.options$summarise <- FALSE
		}		
	}else{
		runjags.options$summarise <- runjags.options$plots
	}
	
	if(runjags.options$plots && !runjags.options$summarise){
		warning("Cannot produce plots automatically when summarise=FALSE; setting summarise=TRUE")
		runjags.options$summarise <- TRUE
	}
	
	if(!is.null(model)){
		if(any(names(runjags.options)=="model")) warning("A model supplied in the runjags.options list was ignored")
		runjags.options$model <- model
	}else{
		model <- runjags.options$model
	}
	if(any(names(runjags.options)=="datalist")){
		warning("The datalist supplied will be ignored - use the datafunction argument to run.jags.study to provide datasets for each iteration instead")
	}
	# The special 'NaN' argument suppresses the warning about data in the model being ignored:
	runjags.options$datalist <- NaN
	
	# Get the data auto-specified in the model so we can copy and possibly over-write it:
	modelsetup <- setup.jagsfile(model,n.chains=1,call.setup=FALSE,failincomplete=FALSE,method=runjags.options$method)
	modeldata <- modelsetup$data
	if(is.na(modeldata)) modeldata <- list() else modeldata <- list.format(modeldata)
	
	st <- Sys.time()
	swcat(paste("\nStarting a JAGS study at", format(st, format="%H:%M"), "\n"))
	
	# Now check that the datafunction gives us some data and if it does include it with the setup call
	if(!is.null(datafunction)){
		if(!is.function(datafunction) || length(formals(datafunction))!=1){
			stop("The datafunction argument provided must be a function with a single argument representing the simulation number")
		}		

		data <- vector('list',length=simulations)
		alreadywarned <- FALSE
		for(i in 1:simulations){
			thedata <- datafunction(i)
			if(class(thedata)=="character") thedata <- list.format(thedata)
			if(class(thedata)!="list") stop("The data function must return either a named list or a character string representing the data for that iteration")
			
			tdata <- modeldata
			# Check all names are unique and if not over-write tdata (specified in the model) with thedata (specified in the function) with a warning:
			if(any(names(tdata) %in% names(thedata))){
				
				duplicated <- names(tdata)[which(names(tdata) %in% names(thedata))]				
				if(!alreadywarned) warning(paste("The following data variable", if(length(duplicated)>1) "s were" else " was", " specified both in the model and the data function: ", paste(duplicated,collapse=", "), " (the data function values were used)", sep=""))
				alreadywarned <- TRUE
				
				for(d in duplicated) tdata[d] <- thedata[d]
					
				notduplicated <- names(thedata)[!names(thedata) %in% duplicated]
				tdata <- c(tdata, thedata[notduplicated])
				
			}else{
				tdata <- c(tdata, thedata)
			}

			data[[i]] <- dump.format(tdata)
			class(data[[i]]) <- "runjags.data"
			valid <- checkvalidforjags(data[[i]])	
			if(!valid$valid) stop(paste("The following problem was identified in the data for simulation ", i, ":  ", valid$probstring, sep=""))				
			
		}
		dc <- sample(1:simulations, 1)

		runjags.options$data <- as.character(data[[dc]])
		if(test) swcat("Testing the model and data for simulation ", dc, "...\n",sep="") else swcat("Checking the supplied model definition (and data for simulation ", dc, ")...\n",sep="")		
	}else{
		data <- NULL
		if(test) swcat("Testing the model...\n",sep="") else swcat("Checking the supplied model definition...\n",sep="")		
	}
	
	# Add targets to monitored variables:
	if(identical(targets,list())) stop("A named list of variables and their values must be provided to the 'target' option")
	if(class(targets)!="list" || length(targets)==0 || any(names(targets)=="")) stop("The targets variable must be a named list of variable(s) to assess the model's performance on")
	
	# If we don't have any monitored variables make the targets monitors, otherwise add them later so we don't over-write the monitors in the model block:
	addtargets <- TRUE
	if(length(c(modelsetup$monitor,runjags.options$monitor))==0){
		runjags.options$monitor <- names(targets)
		addtargets <- FALSE
	}
	
	# Now get the full argument list for autorun.jags with these modifications above:
	runjags.options <- getargs(c('autorun.jags'), runjags.options, returnall=TRUE)	
	
	args <- runjags.options[which(names(runjags.options) %in% names(formals(setup.jagsfile)))]
	obj <- do.call("setup.jagsfile", args=args)
	
	if(any(obj$modules=="runjags") && runjags.options$method!="rjags") stop("")
	
	if(any(c("popt", "pd.i") %in% obj$monitor)){
		warning("Cannot monitor popt or pd.i with automatic run length functions - the specified monitor(s) have been removed")
		obj$monitor <- obj$monitor[obj$monitor!="pd.i"]
		obj$monitor <- obj$monitor[obj$monitor!="popt"]
	}
	if(!runjags.options$summarise && any(obj$monitor=="dic")){
		runjags.options$summarise <- TRUE
	}
	if(addtargets) runjags.options$add.monitor <- names(targets)
	
	if(test){
		testoptions <- runjags.options[which(names(runjags.options) %in% names(formals(extend.jags)))]
		testoptions <- c(testoptions, list(runjags.object=obj))
		testoptions$adapt <- 1000
		testoptions$sample <- 1000
		testoptions$combine <- FALSE
		testr <- try(do.call("extend.jags", args=testoptions))
		if(class(testr)=="try-error") stop("The test model returned an error - the simulation study was aborted")
		swcat("The model runs OK\n")
	}else{
		swcat("The model compiles OK\n")
	}
	
	# Slight hack - we're not calling actually run.jags so add combine=FALSE to the argument list:
	runjags.options <- runjags.options[which(names(runjags.options) %in% names(formals(autoextend.jags)))]
	runjags.options <- c(runjags.options, list(runjags.object=obj, combine=FALSE))
	
	if(is.null(data)){
		DATAS <- lapply(1:simulations, function(x) return(obj$data))
	}else{
		DATAS <- data	
	}
	X <- 1:simulations
	FUN <- function(x, DATAS=DATAS, runjags.options=runjags.options){
		
		runjags.options$method <- eval(runjags.options$method)
		# Detect common problems:
		if(!require(runjags) || package_version(installed.packages()['runjags','Version'])<1) stop(paste("The runjags package (version >=1.0.0) is not installed on the cluster node '", Sys.info()['nodename'], "'", sep="")) 
		if(runjags.options$method=='rjags' && !require(rjags)) stop(paste("The rjags package is not installed on the cluster node '", Sys.info()['nodename'], "' - try specifying runjags.options=list(method='simple')", sep="")) 
		if(numeric_version(installed.packages()['runjags','Version']) < 1) stop("The runjags package (version >=1.0.0) must be installed on each cluster node")
		if(runjags.options$method=='rjags'){
			for(module in runjags.options$modules){
				if(module=="runjags"){
					success <- try(load.module.runjags())
				}else{
					success <- try(load.module(module))				
				}
				if(class(success)=="try-error") stop(paste("The required module '", module, "' is not installed on the cluster node '", Sys.info()['nodename'], "'", sep=""))
			}
		}else{
			if(any(runjags.options$modules=="runjags")) stop("The runjags module is only available using the rjags method; to use the functions provided with other methods install (and specify using the module argument) the 'runjagsmodule' standalone module")
			
		}
		
		thedata <- DATAS[[x]]
		runjags.options$runjags.object$data <- thedata
		
		output <- capture.output({
			result <- try(do.call("autoextend.jags", args=runjags.options))
			})
		
		cat("Finished running simulation ", x, " of ", length(DATAS), "\n", sep="")
		
		return(result)
		
	}
	
	if(simulations>1 && all(sapply(2:simulations,function(x) return(identical(DATAS[[1]],DATAS[[x]]))))) warning("The data provided is the same for each simulation")
	
	bigfun <- any(names(formals(parallel.method))=="FUN")
	if(!bigfun && !any(names(formals(parallel.method))=="fun")) stop("The provided parallel apply function does not take the argument 'fun' or 'FUN'")
	if(!any(names(formals(parallel.method))=="X")) stop("The provided parallel apply function does not take the argument 'X'")
	if(any(names(formals(parallel.method)) %in% c("DATAS", "runjags.options"))) stop("The arguments 'DATAS' and 'runjags.options' are used internally and can't be passed to the provided parallel apply function")
	
	swcat("Calling autorun.jags for ", simulations, " simulations...\n",sep="")
	
	success <- try({
	# Now set up the cluster:
	if(identical(parallel.method, parLapply) && !any(names(list(...))=="cl")){
			if(cat.progress) warning("Unable to display the progress of simulations when using parLapply")
			ncores <- suppressWarnings(detectCores())
			if(is.na(ncores)){
				ncores <- 2
				warning("Unable to detect the available number of cores on your machine - using 2 cores as a default")
			}
			cl <- makeCluster(ncores)
			on.exit(stopCluster(cl))
			output <- capture.output({
				results <- parallel.method(cl=cl, X=1:simulations, if(bigfun) FUN=FUN else fun=FUN, DATAS=DATAS, runjags.options=runjags.options, ...)
			})
	}else{
		if(cat.progress){
			results <- parallel.method(X=1:simulations, if(bigfun) FUN=FUN else fun=FUN, DATAS=DATAS, runjags.options=runjags.options, ...)		
		}else{
			output <- capture.output({			
				results <- parallel.method(X=1:simulations, if(bigfun) FUN=FUN else fun=FUN, DATAS=DATAS, runjags.options=runjags.options, ...)						
			})
		}
	}
	})
	
	if(class(success)=="try-error"){
		stop("An unexpected error occured - ensure that the model runs using run.jags on the first dataset, and try using lapply as the parallel.method to debug")
	}
	
	errors <- sapply(results, class)=="try-error"
	if(all(errors)){
		failedjags$study <- results
		if(test) stop("All simulations returned an error (see failedjags$study) - ensure that the model runs using run.jags on the first dataset, and try using lapply as the parallel.method to debug") else stop("All simulations returned an error (see failedjags$study) - ensure that the model runs using run.jags on the first dataset")
	}
	if(any(errors)) warning(paste("An error was returned from ", sum(errors), " out of ", length(errors), " simulations:  the errors will be returned along with the runjags objects"))
	
	swcat("Finished running the simulations\n")

	if(simulations != length(results)) stop("A different number of results was returned to that specified by simulations - ensure that the model runs using run.jags on the first dataset, and try using lapply as the parallel.method to debug")
	
	# Farm this out into another function so we can easily have a retrieve.jags.study function one day - targets will have to be added to the runjags.study.suspended object:
	retval <- summarise.jags.study(results=results[!errors], targets=targets, confidence=confidence)
	
	retval <- c(retval, list(simulations=simulations, model=obj$model, targets=targets, monitor=unique(c(obj$monitor,names(targets))), datafunction=datafunction, data=DATAS, crashed=errors))
	if(record.chains) retval <- c(retval, list(runjags=results))
	
	class(retval) <- "runjags.study"
	
	et <- Sys.time()
	swcat("Finished summarising results\n")
	
	swcat(paste("Finished JAGS study at ", format(et, format="%H:%M"), " (total time taken:  ", timestring(st, et), ")\n\n", sep=""))
	
	return(retval)
	
}


summarise.jags.study <- function(results, targets, confidence){
	
	if(!length(confidence)>0 || any(confidence <= 0) || any(confidence > 1)) stop("The confidence variable must be a numeric vector between 0 and 1")
	
	# Targets is a named list of monitor variables (which are automatically added to monitor) and the true value (if an array magically catch and rename to var[1] var[2] etc) which is compared to MCMC output - return distribution of medians and confidence% LCI/UCI and % within confidence limits - confidence may be a vector

	fullvars <- targets[order(names(targets))]
	fullvars <- getjagsnames(fullvars)
	
	useindex <- which(varnames(results[[1]]$mcmc) %in% names(fullvars))
	usevars <- fullvars[names(fullvars) %in% varnames(results[[1]]$mcmc)]

	if(length(usevars)==0) stop(paste("None of the target variable(s) '", paste(names(fullvars),collapse="','"), "' were found in the JAGS output ('", paste(varnames(results[[1]]$mcmc),collapse="','"),"')",sep=""))

	if(length(usevars)!=length(fullvars)) warning(paste("One or more of the target variable(s) '", paste(names(fullvars),collapse="','"), "' were not found in the JAGS output ('", paste(varnames(results[[1]]$mcmc),collapse="','"),"')",sep=""))
	
	usevars <- usevars[match(varnames(results[[1]]$mcmc),names(usevars))]
	if(!(all(names(usevars)==varnames(results[[1]]$mcmc)[useindex]))){
		stop(paste("Sorry - something has gone wrong with indexing the variables; I was expecting to see ", paste(names(usevars),collapse=","), " but saw ", paste(varnames(results[[1]]$mcmc)[useindex],collapse=","), " - please submit a bug report!", sep=""))
	}
	
	simulations <- length(results)
	sumtable <- vapply(results,function(result){
		
		result <- combine.mcmc(result$mcmc,collapse.chains=TRUE)[,useindex,drop=FALSE]
		
		rtable <- matrix(nrow=length(useindex), ncol=4+(length(confidence)*4),dimnames=list(names(usevars), c("Target","Median","Mean",apply(expand.grid(c("Lower","Upper","Range","Within"),confidence*100,"%CI"),1,paste,collapse=""),"AutoCorr(Lag10)")))

		stochastic <- apply(result,2,var)!=0
		if(!any(stochastic)) stop("One or more target variables is non-stochastic")
		
		rtable[,1] <- usevars
		
		rtable[stochastic,2] <- apply(result[,stochastic,drop=FALSE],2,median)
		rtable[stochastic,3] <- apply(result[,stochastic,drop=FALSE],2,mean)
		for(c in 1:length(confidence)){
			h <- as.matrix(HPDinterval(result[,stochastic,drop=FALSE],prob=confidence[c]))
			r <- h[,2]-h[,1]
			w <- (usevars[stochastic] >= h[,1] & usevars[stochastic] <= h[,2])
			rtable[stochastic,((4*(c-1)) : ((4*c)-1))+4] <- c(h,r,w)
		}
		rtable[stochastic,ncol(rtable)] <- safe.autocorr.diag(result[,stochastic,drop=FALSE],lags=10)
		
		rtable[!stochastic,2:ncol(rtable)] <- NA
		
		return(rtable)
		
	}, matrix(0,nrow=length(useindex), ncol=4+(length(confidence)*4)))
	
	values <- apply(sumtable[,2,,drop=FALSE],1,function(x) return(sum(!is.na(x))))
	meanable <- values>1
	if(any(meanable)){
		msum <- cbind(apply(sumtable[meanable,,,drop=FALSE],c(1,2),mean,na.rm=TRUE), Simulations=values[meanable])
	}else{
		msum <- NA	
	}
	indable <- values==1
	if(any(indable)){
		isum <- t(apply(sumtable[indable,,,drop=FALSE],1,function(x) return(x[,which(!is.na(x[2,]))])))
	}else{
		isum <- NA	
	}
	
#	sumsum[,((4*(length(confidence)-1))+6)] <- sumsum[,((4*(length(confidence)-1))+6)]*100
	
	timetaken <- sapply(results, function(x) return(x$timetaken))
	sample <- sapply(results, function(x) return(x$sample * x$thin))
	burnin <- sapply(results, function(x) return(x$burnin * x$thin))
	
	return(list(means=msum, singles=isum, individual=sumtable, timetaken=timetaken, sample=sample, burnin=burnin))

}

run.JAGS.study <- run.jags.study