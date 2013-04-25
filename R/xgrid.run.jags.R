xgrid.run.jags <- function(model, max.threads=Inf, JAGSversion=">=2.0.0", email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA, queueforserver=FALSE, hostnode=NA, forcehost=FALSE, ramrequired=10, jobname=NA, cleanup=TRUE, showprofiles=FALSE, jagspath='/usr/local/bin/jags', mgridpath=system.file("xgrid", "mgrid.sh", package="runjags"), hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"), password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), ...){
	
	if(max.threads < 1) stop("The maximum number of threads must be greater than 0")
		
	passed <- c(list(...), model=model)
	if(any(names(passed)=="method") || any(names(passed)=="method.options" )) stop("Cannot specify 'method' or 'method.options' arguments to xgrid functions")
	
	# Translate monitor.pd/popt/pd.i/deviance (legacy code):
	monitor.deviance <- any(names(passed)=="monitor.deviance") && passed$monitor.deviance
	monitor.pd <- any(names(passed)=="monitor.pd") && passed$monitor.pd
	monitor.popt <- any(names(passed)=="monitor.popt") && passed$monitor.popt
	monitor.pd.i <- any(names(passed)=="monitor.pd.i") && passed$monitor.pd.i
	if(any(names(passed)=="check.conv")){
		warning("Use of the 'check.conv' argument is deprecated - use 'summarise' to achieve some of the same function", call.=FALSE)
		if(!any(names(passed)=="summarise")) passed <- c(passed, list(summarise=passed$check.conv))
	}

	if(any(names(passed)=="monitor")){
		passed$monitor <- c(passed$monitor, if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")
	}else{
		if(any(c(monitor.deviance,monitor.pd,monitor.popt,monitor.pd.i))){
			passed <- c(passed, list(c(if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")))
			warning("Use of the 'monitor.deviance', 'monitor.pd', 'monitor.popt' and 'monitor.pdi' arguments is deprecated - add the special variables 'dic', 'deviance', 'pd', 'popt' or 'pd.i' to the 'monitor' argument instead", call.=FALSE)
		}
	}
	
	# Get default argument values from specified functions for anything not given to this function:
 	arglist <- getargs(c('run.jags'), passed)	
	
 	args <- arglist[which(names(arglist) %in% names(formals(setup.jagsfile)))]
	args$method <- if(any(.packages()=="rjags")) "rjags" else "simple"
 	obj <- do.call("setup.jagsfile", args=args)


	arglist <- arglist[names(arglist)!="method" & names(arglist)!="method.options"]
	
	# Get xgrid method options:
	method.options <- setup.xgrid(separate=max.threads>1,JAGSversion=JAGSversion, Rversion="", packages=list(), artfun=function() writeLines("1"), email=email, profiling=profiling, cpuarch=cpuarch, minosversion=minosversion, queueforserver=queueforserver, hostnode=hostnode, forcehost=forcehost, ramrequired=ramrequired, jobname=jobname, cleanup=cleanup, showprofiles=showprofiles, jagspath=jagspath, Rpath='/usr/bin/R', Rbuild='64', max.filesize="1GB", mgridpath=mgridpath, hostname=hostname, password=password, submitandstop=FALSE, jagsrun=TRUE)
	method.options$max.threads <- max.threads
	method.options <- method.options[c("jobname", "command", "customart", "jagspath", "submitandstop", "max.threads", "mgridpath", "hostname", "password")]
	
	if(max.threads>1 && any(obj$monitor %in% c("dic", "pd", "popt", "pd.i"))){
		warning("Setting max.threads to 1 to allow calculation of dic/pd/popt/pd.i")
		max.threads <- 1
	}
	method.options$max.threads <- max.threads
	
	# Slight hack - we're not calling actually run.jags so add combine=FALSE to the argument list:
	arglist <- c(arglist, list(runjags.object=obj, combine=FALSE, method="xgrid", method.options=method.options)) 
	
	if(!any(names(arglist)=="burnin")) arglist$burnin <- 5000
	
 	args <- arglist[which(names(arglist) %in% names(formals(extend.jags)))]
	
 	res <- do.call("extend.jags", args=args)
	
	return(res)
}


xgrid.autorun.jags <- function(model, max.threads=Inf, JAGSversion=">=2.0.0", email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA, queueforserver=FALSE, hostnode=NA, forcehost=FALSE, ramrequired=10, jobname=NA, cleanup=TRUE, showprofiles=FALSE, jagspath='/usr/local/bin/jags', mgridpath=system.file("xgrid", "mgrid.sh", package="runjags"), hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"), password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), ...){
	
	if(max.threads < 1) stop("The maximum number of threads must be greater than 0")
		
	passed <- c(list(...), model=model)
	if(any(names(passed)=="method") || any(names(passed)=="method.options" )) stop("Cannot specify 'method' or 'method.options' arguments to xgrid functions")
	
	# Translate monitor.pd/popt/pd.i/deviance (legacy code):
	monitor.deviance <- any(names(passed)=="monitor.deviance") && passed$monitor.deviance
	monitor.pd <- any(names(passed)=="monitor.pd") && passed$monitor.pd
	monitor.popt <- any(names(passed)=="monitor.popt") && passed$monitor.popt
	monitor.pd.i <- any(names(passed)=="monitor.pd.i") && passed$monitor.pd.i
	
	if(any(names(passed)=="monitor")){
		passed$monitor <- c(passed$monitor, if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")
	}else{
		if(any(c(monitor.deviance,monitor.pd,monitor.popt,monitor.pd.i))){
			passed <- c(passed, list(c(if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")))
			warning("Use of the 'monitor.deviance', 'monitor.pd', 'monitor.popt' and 'monitor.pdi' arguments is deprecated - add the special variables 'dic', 'deviance', 'pd', 'popt' or 'pd.i' to the 'monitor' argument instead", call.=FALSE)
		}
	}
	
	# Get default argument values from specified functions for anything not given to this function:
 	arglist <- getargs(c('autorun.jags'), passed)	
	
 	args <- arglist[which(names(arglist) %in% names(formals(setup.jagsfile)))]
	args$method <- if(any(.packages()=="rjags")) "rjags" else "simple"
 	obj <- do.call("setup.jagsfile", args=args)
	
	arglist <- arglist[names(arglist)!="method" & names(arglist)!="method.options"]
	
	# Get xgrid method options:
	method.options <- setup.xgrid(separate=max.threads>1,JAGSversion=JAGSversion, Rversion="", packages=list(), artfun=function() writeLines("1"), email=email, profiling=profiling, cpuarch=cpuarch, minosversion=minosversion, queueforserver=queueforserver, hostnode=hostnode, forcehost=forcehost, ramrequired=ramrequired, jobname=jobname, cleanup=cleanup, showprofiles=showprofiles, jagspath=jagspath, Rpath='/usr/bin/R', Rbuild='64', max.filesize="1GB", mgridpath=mgridpath, hostname=hostname, password=password, submitandstop=FALSE, jagsrun=TRUE)
	method.options$max.threads <- max.threads
	method.options <- method.options[c("jobname", "command", "customart", "jagspath", "submitandstop", "max.threads", "mgridpath", "hostname", "password")]
	
	if(max.threads>1 && any(obj$monitor %in% c("dic", "pd", "popt", "pd.i"))){
		warning("Setting max.threads to 1 to allow calculation of dic/pd/popt/pd.i")
		max.threads <- 1
	}
	method.options$max.threads <- max.threads
	
	# Slight hack - we're not calling actually run.jags so add combine=FALSE to the argument list:
	arglist <- c(arglist, list(runjags.object=obj, combine=FALSE, method="xgrid", method.options=method.options))
	if(!any(names(arglist)=="startburnin")) arglist$startburnin <- 5000
	
 	args <- arglist[which(names(arglist) %in% names(formals(autoextend.jags)))]
	
 	res <- do.call("autoextend.jags", args=args)
	
	return(res)
}

xgrid.extend.jags <- function(runjags.object, max.threads=Inf, JAGSversion=">=2.0.0", email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA, queueforserver=FALSE, hostnode=NA, forcehost=FALSE, ramrequired=10, jobname=NA, cleanup=TRUE, showprofiles=FALSE, jagspath='/usr/local/bin/jags', mgridpath=system.file("xgrid", "mgrid.sh", package="runjags"), hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"), password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), ...){
	
	checkvalidrunjagsobject(runjags.object)

	if(max.threads < 1) stop("The maximum number of threads must be greater than 0")
		
	# We are only passing straight through to extend.jags so don't really need all this but may as well  keep it in:
	
	passed <- c(list(...), list(x=runjags.object))
	if(any(names(passed)=="method") || any(names(passed)=="method.options" )) stop("Cannot specify 'method' or 'method.options' arguments to xgrid functions")
	
	# Translate monitor.pd/popt/pd.i/deviance (legacy code):
	monitor.deviance <- any(names(passed)=="monitor.deviance") && passed$monitor.deviance
	monitor.pd <- any(names(passed)=="monitor.pd") && passed$monitor.pd
	monitor.popt <- any(names(passed)=="monitor.popt") && passed$monitor.popt
	monitor.pd.i <- any(names(passed)=="monitor.pd.i") && passed$monitor.pd.i
	if(any(names(passed)=="check.conv")){
		warning("Use of the 'check.conv' argument is deprecated - use 'summarise' to achieve some of the same function", call.=FALSE)
		if(!any(names(passed)=="summarise")) passed <- c(passed, list(summarise=passed$check.conv))
	}
	
	if(any(names(passed)=="monitor")){
		passed$monitor <- c(passed$monitor, if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")
	}else{
		if(any(c(monitor.deviance,monitor.pd,monitor.popt,monitor.pd.i))){
			passed <- c(passed, list(c(if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")))
			warning("Use of the 'monitor.deviance', 'monitor.pd', 'monitor.popt' and 'monitor.pdi' arguments is deprecated - add the special variables 'dic', 'deviance', 'pd', 'popt' or 'pd.i' to the 'monitor' argument instead", call.=FALSE)
		}
	}
	
	# Get default argument values from specified functions for anything not given to this function:
 	arglist <- getargs(c('extend.jags'), passed)	
 	arglist <- arglist[names(arglist)!="method" & names(arglist)!="method.options"]
	
	# Get xgrid method options:
	method.options <- setup.xgrid(separate=max.threads>1,JAGSversion=JAGSversion, Rversion="", packages=list(), artfun=function() writeLines("1"), email=email, profiling=profiling, cpuarch=cpuarch, minosversion=minosversion, queueforserver=queueforserver, hostnode=hostnode, forcehost=forcehost, ramrequired=ramrequired, jobname=jobname, cleanup=cleanup, showprofiles=showprofiles, jagspath=jagspath, Rpath='/usr/bin/R', Rbuild='64', max.filesize="1GB", mgridpath=mgridpath, hostname=hostname, password=password, submitandstop=FALSE, jagsrun=TRUE)
	method.options$max.threads <- max.threads
	method.options <- method.options[c("jobname", "command", "customart", "jagspath", "submitandstop", "max.threads", "mgridpath", "hostname", "password")]
	
	if(max.threads>1 && any(runjags.object$monitor %in% c("dic", "pd", "popt", "pd.i"))){
		warning("Setting max.threads to 1 to allow calculation of dic/pd/popt/pd.i")
		max.threads <- 1
	}
	method.options$max.threads <- max.threads
	
	arglist <- c(arglist, list(method="xgrid", method.options=method.options))
	args <- arglist[which(names(arglist) %in% names(formals(extend.jags)))]
	
 	res <- do.call("extend.jags", args=args)
	
	return(res)
}

xgrid.autoextend.jags <- function(runjags.object, max.threads=Inf, JAGSversion=">=2.0.0", email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA, queueforserver=FALSE, hostnode=NA, forcehost=FALSE, ramrequired=10, jobname=NA, cleanup=TRUE, showprofiles=FALSE, jagspath='/usr/local/bin/jags', mgridpath=system.file("xgrid", "mgrid.sh", package="runjags"), hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"), password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), ...){
	
	checkvalidrunjagsobject(runjags.object)

	if(max.threads < 1) stop("The maximum number of threads must be greater than 0")
		
	# We are only passing straight through to autoextend.jags so don't really need all this but may as well keep it in:

	passed <- c(list(...), list(x=runjags.object))
	if(any(names(passed)=="method") || any(names(passed)=="method.options" )) stop("Cannot specify 'method' or 'method.options' arguments to xgrid functions")
	
	# Translate monitor.pd/popt/pd.i/deviance (legacy code):
	monitor.deviance <- any(names(passed)=="monitor.deviance") && passed$monitor.deviance
	monitor.pd <- any(names(passed)=="monitor.pd") && passed$monitor.pd
	monitor.popt <- any(names(passed)=="monitor.popt") && passed$monitor.popt
	monitor.pd.i <- any(names(passed)=="monitor.pd.i") && passed$monitor.pd.i
	
	if(any(names(passed)=="monitor")){
		passed$monitor <- c(passed$monitor, if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")
	}else{
		if(any(c(monitor.deviance,monitor.pd,monitor.popt,monitor.pd.i))){
			passed <- c(passed, list(c(if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")))
			warning("Use of the 'monitor.deviance', 'monitor.pd', 'monitor.popt' and 'monitor.pdi' arguments is deprecated - add the special variables 'dic', 'deviance', 'pd', 'popt' or 'pd.i' to the 'monitor' argument instead", call.=FALSE)
		}
	}
	
	# Get default argument values from specified functions for anything not given to this function:
 	arglist <- getargs(c('autoextend.jags'), passed)	
	
	arglist <- arglist[names(arglist)!="method" & names(arglist)!="method.options"]
	
	# Get xgrid method options:
	method.options <- setup.xgrid(separate=max.threads>1,JAGSversion=JAGSversion, Rversion="", packages=list(), artfun=function() writeLines("1"), email=email, profiling=profiling, cpuarch=cpuarch, minosversion=minosversion, queueforserver=queueforserver, hostnode=hostnode, forcehost=forcehost, ramrequired=ramrequired, jobname=jobname, cleanup=cleanup, showprofiles=showprofiles, jagspath=jagspath, Rpath='/usr/bin/R', Rbuild='64', max.filesize="1GB", mgridpath=mgridpath, hostname=hostname, password=password, submitandstop=FALSE, jagsrun=TRUE)
	method.options$max.threads <- max.threads
	method.options <- method.options[c("jobname", "command", "customart", "jagspath", "submitandstop", "max.threads", "mgridpath", "hostname", "password")]
	
	if(max.threads>1 && any(runjags.object$monitor %in% c("dic", "pd", "popt", "pd.i"))){
		warning("Setting max.threads to 1 to allow calculation of dic/pd/popt/pd.i")
		max.threads <- 1
	}
	method.options$max.threads <- max.threads
	
	arglist <- c(arglist, list(method="xgrid", method.options=method.options))
 	args <- arglist[which(names(arglist) %in% names(formals(autoextend.jags)))]
	
 	res <- do.call("autoextend.jags", args=args)
	
	return(res)
}

xgrid.submit.jags <- function(model, max.threads=Inf, JAGSversion=">=2.0.0", email=NA, profiling=TRUE, cpuarch=NA, minosversion=NA, queueforserver=FALSE, hostnode=NA, forcehost=FALSE, ramrequired=10, jobname=NA, jagspath='/usr/local/bin/jags', mgridpath=system.file("xgrid", "mgrid.sh", package="runjags"), hostname=Sys.getenv("XGRID_CONTROLLER_HOSTNAME"), password=Sys.getenv("XGRID_CONTROLLER_PASSWORD"), ...){
	
	if(max.threads < 1) stop("The maximum number of threads must be greater than 0")
		
	passed <- c(list(...), model=model)
	if(any(names(passed)=="method") || any(names(passed)=="method.options" )) stop("Cannot specify 'method' or 'method.options' arguments to xgrid functions")
	if(any(names(passed)=="tempdir")) stop("Cannot specify 'tempdir' argument to xgrid.submit functions")
	if(any(names(passed)=="keep.jags.files")) stop("Cannot specify 'keep.jags.files' argument to xgrid.submit functions (this argument is supplied to xgrid.results.jags function instead)")
	
	# Translate monitor.pd/popt/pd.i/deviance (legacy code):
	monitor.deviance <- any(names(passed)=="monitor.deviance") && passed$monitor.deviance
	monitor.pd <- any(names(passed)=="monitor.pd") && passed$monitor.pd
	monitor.popt <- any(names(passed)=="monitor.popt") && passed$monitor.popt
	monitor.pd.i <- any(names(passed)=="monitor.pd.i") && passed$monitor.pd.i
	if(any(names(passed)=="check.conv")){
		warning("Use of the 'check.conv' argument is deprecated - use 'summarise' to achieve some of the same function", call.=FALSE)
		if(!any(names(passed)=="summarise")) passed <- c(passed, list(summarise=passed$check.conv))
	}
	
	if(any(names(passed)=="monitor")){
		passed$monitor <- c(passed$monitor, if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")
	}else{
		if(any(c(monitor.deviance,monitor.pd,monitor.popt,monitor.pd.i))){
			passed <- c(passed, list(c(if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")))
			warning("Use of the 'monitor.deviance', 'monitor.pd', 'monitor.popt' and 'monitor.pdi' arguments is deprecated - add the special variables 'dic', 'deviance', 'pd', 'popt' or 'pd.i' to the 'monitor' argument instead", call.=FALSE)
		}
	}
	
	# Get default argument values from specified functions for anything not given to this function:
 	arglist <- getargs(c('run.jags'), passed)	
	
 	args <- arglist[which(names(arglist) %in% names(formals(setup.jagsfile)))]
	args$method <- if(any(.packages()=="rjags")) "rjags" else "simple"
 	obj <- do.call("setup.jagsfile", args=args)
	
	arglist <- arglist[names(arglist)!="method" & names(arglist)!="method.options" & names(arglist)!="tempdir" & names(arglist)!="keep.jags.files"]
	
	# Get xgrid method options:
	method.options <- setup.xgrid(separate=max.threads>1,JAGSversion=JAGSversion, Rversion="", packages=list(), artfun=function() writeLines("1"), email=email, profiling=profiling, cpuarch=cpuarch, minosversion=minosversion, queueforserver=queueforserver, hostnode=hostnode, forcehost=forcehost, ramrequired=ramrequired, jobname=jobname, cleanup=FALSE, showprofiles=FALSE, jagspath=jagspath, Rpath='/usr/bin/R', Rbuild='64', max.filesize="1GB", mgridpath=mgridpath, hostname=hostname, password=password, submitandstop=TRUE, jagsrun=TRUE)
	method.options$max.threads <- max.threads
	method.options <- method.options[c("jobname", "command", "customart", "jagspath", "submitandstop", "max.threads", "mgridpath", "hostname", "password")]
	
	if(max.threads>1 && any(obj$monitor %in% c("dic", "pd", "popt", "pd.i"))){
		warning("Setting max.threads to 1 to allow calculation of dic/pd/popt/pd.i")
		max.threads <- 1
	}
	method.options$max.threads <- max.threads
	
	
	# Slight hack - we're not calling actually run.jags so add combine=FALSE to the argument list:
	arglist <- c(arglist, list(runjags.object=obj, combine=FALSE, method="xgrid", method.options=method.options, keep.jags.files=TRUE, tempdir=FALSE))
	
 	args <- arglist[which(names(arglist) %in% names(formals(extend.jags)))]
	if(!any(names(arglist)=="burnin")) arglist$burnin <- 5000
	
 	res <- do.call("extend.jags", args=args)
	
	return(res)
}


xgrid.results.jags <- function(background.runjags.object, wait=TRUE, cleanup=TRUE){
	
	if(class(background.runjags.object)!="runjags.bginfo") stop("An object produced by a background runjags method must be supplied (see the manual page for more details)")
	if(background.runjags.object$method!="xgrid"){
		stop("This JAGS process was not started using an xgrid method")
	}
	
	jobinfo <- list(directory=background.runjags.object$directory, jobname=background.runjags.object$jobname, jobid=background.runjags.object$jobid)
	
	swcat("Retrieving xgrid JAGS results...\n\n")
	
	# First go to xgrid.retrieve which checks the jobname exists and gets files back from xgrid (silent is always FALSE as if silent.jags was specified, then jags output is binned):
	output <- xgrid.retrieve(jobinfo=jobinfo, wait=wait, silent=FALSE, cleanup=cleanup, partialretrieve=FALSE, jags=TRUE)

	if(!output$done & wait) stop("There was an error running the model")
	if(!output$done & !wait) stop("The model has not yet finished running")
	
	save.directory <- getwd()
	on.exit(setwd(save.directory))
		
	# tempdir is always FALSE, keep.jags.files can't be specified to xgrid.submit functions:
	tempdir <- FALSE
	keep.jags.files <- !cleanup

	# Then copy/paste from extend.jags:
		
	newoutput <- runjags.readin(directory=background.runjags.object$directory, copy=(keep.jags.files & tempdir), delete=!(keep.jags.files & !tempdir), silent.jags=background.runjags.object$silent.jags, target.iters=background.runjags.object$sample, n.chains=length(background.runjags.object$inits), monitor=background.runjags.object$monitor, method=background.runjags.object$method, method.options=background.runjags.object$method.options, suspended=TRUE)
		
	runjags.object <- background.runjags.object
	
	end.state <- newoutput$end.state
	class(end.state) <- 'runjags.inits'
	
	if(runjags.object$combine){
		burnin <- runjags.object$oldburnin
	}else{
		burnin <- runjags.object$oldburnin+background.runjags.object$oldsample+runjags.object$burnin
	}

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
		summaries <- runjags.summaries(mcmclist=combinedoutput$mcmc, pd=combinedoutput$pd, popt=combinedoutput$popt, pd.i=combinedoutput$pd.i, monitor=runjags.object$monitor, plots = runjags.object$plots, psrf.target = runjags.object$psrf.target, normalise.mcmc = runjags.object$normalise.mcmc, check.stochastic = runjags.object$check.stochastic, silent=FALSE)
	}else{
		if(any(runjags.object$monitor=="dic")) warning("Cannot calculate DIC automatically when summarise=FALSE", call.=FALSE)
		if(runjags.object$plots) warning("Cannot produce plots automatically when summarise=FALSE", call.=FALSE)
		message <- "Summary statistics not produced when summarise=FALSE"	
		summaries <- list(summary=message, HPD=message, hpd=message, mcse=message, psrf=message, autocorr=message, crosscorr=message, stochastic=message, dic=message, trace=message, density=message)
	}
	
	combinedoutput <- c(combinedoutput, list(end.state=end.state, burnin=burnin, sample=niter(combinedoutput$mcmc), thin=runjags.object$thin), summaries, list(model=runjags.object$model, data=runjags.object$data, monitor=runjags.object$monitor, modules=runjags.object$modules, factories=runjags.object$factories, method=runjags.object$method, method.options=runjags.object$method.options))
	class(combinedoutput) <- 'runjags'
	
	swcat("Finished running the simulation\n")
	
	combinedoutput$timetaken <- difftime(Sys.time(), background.runjags.object$startedon) + background.runjags.object$timetaken
	
	return(combinedoutput)

}


xgrid.run.JAGS <- xgrid.run.jags
xgrid.autorun.JAGS <- xgrid.autorun.jags
xgrid.extend.JAGS <- xgrid.extend.jags
xgrid.autoextend.JAGS <- xgrid.autoextend.jags
xgrid.submit.JAGS <- xgrid.submit.jags

xgrid.results.JAGS <- xgrid.results.jags


# Deprecated (but no warnings as they are just copied):
xgrid.run.jagsfile <- xgrid.run.jags
xgrid.autorun.jagsfile <- xgrid.autorun.jags
xgrid.submit.jagsfile <- xgrid.submit.jags
xgrid.run.JAGSfile <- xgrid.run.jagsfile
xgrid.autorun.JAGSfile <- xgrid.autorun.jagsfile
xgrid.submit.JAGSfile <- xgrid.submit.jagsfile
