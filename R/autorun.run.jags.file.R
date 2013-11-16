run.jags <- function(model, monitor = NA, data=NA, n.chains=NA, inits = NA, burnin = 5000, sample = 10000, adapt=max(200-burnin, 0), datalist=NA, initlist=NA, jags = runjags.getOption('jagspath'), silent.jags = FALSE, summarise = TRUE, confidence=0.95, plots = summarise, psrf.target = 1.05, normalise.mcmc = TRUE, check.stochastic = TRUE, modules=runjags.getOption('modules'), factories=runjags.getOption('factories'), thin = 1, monitor.deviance = FALSE, monitor.pd = FALSE, monitor.pd.i = FALSE, monitor.popt = FALSE, check.conv = summarise, keep.jags.files = FALSE, tempdir=runjags.getOption('tempdir'), jags.refresh=0.1, batch.jags=silent.jags, method=runjags.getOption('method'), method.options=list()){
	
	# Translate monitor.pd/popt/pd.i/deviance/check.conv (legacy code):
	monitor <- c(monitor, if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")
	if(summarise!=check.conv){
		warning("Use of the 'check.conv' argument is deprecated - use 'summarise' to achieve some of the same function", call.=FALSE)
		summarise <- check.conv
	}
	if(any(c(monitor.deviance, monitor.pd, monitor.popt, monitor.pd.i))) warning("Use of the 'monitor.deviance', 'monitor.pd', 'monitor.popt' and 'monitor.pdi' arguments is deprecated - add the special variables 'dic', 'deviance', 'pd', 'popt' or 'pd.i' to the 'monitor' argument instead", call.=FALSE)
		
	method <- getrunjagsmethod(method)
	obj <- setup.jagsfile(model=model, datalist=datalist, initlist=initlist, n.chains=n.chains, data=data, inits=inits, monitor=monitor, modules=modules, factories=factories, jags=jags, call.setup=TRUE, method=method)
	
	res <- extend.jags(runjags.object=obj, add.monitor=character(0), drop.monitor=character(0), drop.chain=numeric(0), combine=FALSE, burnin = burnin, sample = sample, adapt=adapt, jags = jags, silent.jags = silent.jags, summarise = summarise, confidence = confidence, plots = plots, psrf.target = psrf.target, normalise.mcmc = normalise.mcmc, check.stochastic = check.stochastic, thin = thin, keep.jags.files = keep.jags.files, tempdir=tempdir, method=method, method.options=method.options, jags.refresh=jags.refresh, batch.jags=batch.jags)
	
	# FOR BAYESCOUNT - remove when bayescount version 1 released
	if(n.chains==2 && class(data)=="character" && burnin==1000 && sample==1000 && identical(names(list.format(data)), c("N","R","Count"))){
		res$req.samples <- res$sample
		res$samples.to.conv <- res$burnin
		class(res) <- "list"
	}
	######
	
	return(res)
	
}


autorun.jags <- function(model, monitor = NA, data=NA, n.chains=NA, inits = NA, startburnin = 5000, startsample = 10000, datalist=NA, initlist=NA, psrf.target = 1.05, normalise.mcmc = TRUE, check.stochastic = TRUE, modules=runjags.getOption('modules'), factories=runjags.getOption('factories'), raftery.options = list(), crash.retry=1, summarise = TRUE, confidence=0.95, plots = summarise, thin.sample = FALSE, jags = runjags.getOption('jagspath'), silent.jags = FALSE, interactive=FALSE, max.time=Inf, adaptive=list(type="burnin", length=200), thin = 1, monitor.deviance = FALSE, monitor.pd = FALSE, tempdir=runjags.getOption('tempdir'), jags.refresh=0.1, batch.jags=silent.jags, method=runjags.getOption('method'), method.options=list()){

	# Translate monitor.pd and deviance (legacy code):
	monitor <- c(monitor, if(monitor.deviance) "deviance", if(monitor.pd) "pd")
	if(any(c(monitor.deviance, monitor.pd))) warning("Use of the 'monitor.deviance' and 'monitor.pd' arguments is deprecated - add the special variables 'dic', 'deviance' or 'pd' to the 'monitor' argument instead", call.=FALSE)
		
	method <- getrunjagsmethod(method)
	obj <- setup.jagsfile(model=model, datalist=datalist, initlist=initlist, n.chains=n.chains, data=data, inits=inits, monitor=monitor, modules=modules, factories=factories, jags=jags, call.setup=TRUE, method=method)
	
	res <- autoextend.jags(runjags.object=obj, add.monitor=character(0), drop.monitor=character(0), drop.chain=numeric(0), combine=FALSE, startburnin=startburnin, startsample=startsample, psrf.target=psrf.target, normalise.mcmc=normalise.mcmc, check.stochastic=check.stochastic, raftery.options=raftery.options, crash.retry=crash.retry, summarise=summarise, confidence = confidence, plots=plots, thin.sample=thin.sample, jags=jags, silent.jags = silent.jags, interactive=interactive, max.time=max.time, adaptive=adaptive, thin = thin, tempdir=tempdir, method=method, method.options=method.options, jags.refresh=jags.refresh, batch.jags=batch.jags)
	
	# FOR BAYESCOUNT - remove when bayescount version 1 released
	if(is.list(silent.jags)){
		res$req.samples <- res$sample
		res$samples.to.conv <- res$burnin
		class(res) <- "list"
	}
	######
	
	return(res)
}
	

run.jagsfile <- function(path, datalist = NA, initlist = NA, n.chains=NA, data=NA, model=NA, inits=NA, monitor=NA, modules=runjags.getOption('modules'), factories=runjags.getOption('factories'), jags = runjags.getOption('jagspath'), ...){
	
	warning("Use of the run.jagsfile function is deprecated - use run.jags directly with the 'model' argument replacing the 'path' argument")
	
	monitor.deviance=monitor.pd=monitor.popt=monitor.pd.i <- FALSE
	passed <- list(...)
	if(any(names(passed)=="monitor.deviance")) monitor.deviance <- TRUE
	if(any(names(passed)=="monitor.pd")) monitor.pd <- TRUE
	if(any(names(passed)=="monitor.popt")) monitor.popt <- TRUE
	if(any(names(passed)=="monitor.pd.i")) monitor.pd.i <- TRUE
			
	if(any(names(passed)=="check.conv")) warning("Use of the 'check.conv' argument is deprecated - use 'summarise' to achieve some of the same function", call.=FALSE)
	
	# Translate monitor.pd/popt/pd.i/deviance/check.conv (legacy code):
	monitor <- c(monitor, if(monitor.deviance) "deviance", if(monitor.pd) "pd", if(monitor.popt) "popt", if(monitor.pd.i) "pd.i")

	if(any(c(monitor.deviance, monitor.pd, monitor.popt, monitor.pd.i))) warning("Use of the 'monitor.deviance', 'monitor.pd', 'monitor.popt' and 'monitor.pdi' arguments is deprecated - add the special variables 'dic', 'deviance', 'pd', 'popt' or 'pd.i' to the 'monitor' argument instead", call.=FALSE)
		
	if(any(names(passed)=="method")) method <- getrunjagsmethod(passed$method) else method <- if('rjags' %in% .packages()) 'rjags' else 'interruptible'
	obj <- setup.jagsfile(model=path, datalist=datalist, initlist=initlist, n.chains=n.chains, data=data, inits=inits, monitor=monitor, modules=modules, factories=factories, jags=jags, call.setup=TRUE, method=method)

	if(!any(names(passed)=="burnin")) passed$burnin <- 5000
	if(!any(names(passed)=="method")){
		passed$method <- if(Sys.info()['user']=='nobody'){ 'simple' }else{ 'interruptible'} 
	}
	if(!any(names(passed)=="method.options")) passed$method.options <- list()
	passed <- c(passed, list(runjags.object=obj, add.monitor=character(0), drop.monitor=character(0), drop.chain=numeric(0), combine=FALSE, jags=jags))
		
	res <- do.call("extend.jags",args=passed)
	return(res)

}

autorun.jagsfile <- function(path, datalist = NA, initlist = NA, n.chains=NA, data=NA, model=NA, inits=NA, monitor=NA, modules=runjags.getOption('modules'), factories=runjags.getOption('factories'), jags = runjags.getOption('jagspath'), ...){
	
	warning("Use of the autorun.jagsfile function is deprecated - use autorun.jags directly with the 'model' argument replacing the 'path' argument")
	
	monitor.deviance=monitor.pd=monitor.popt=monitor.pd.i <- FALSE
	passed <- list(...)
	if(any(names(passed)=="monitor.deviance")) monitor.deviance <- TRUE
	if(any(names(passed)=="monitor.pd")) monitor.pd <- TRUE
	if(any(names(passed)=="monitor.popt")) monitor.popt <- TRUE
	if(any(names(passed)=="monitor.pd.i")) monitor.pd.i <- TRUE
	
	# Translate monitor.pd and deviance (legacy code):
	monitor <- c(monitor, if(monitor.deviance) "deviance", if(monitor.pd) "pd")
	if(any(c(monitor.deviance, monitor.pd))) warning("Use of the 'monitor.deviance' and 'monitor.pd' arguments is deprecated - add the special variables 'dic', 'deviance' or 'pd' to the 'monitor' argument instead", call.=FALSE)
		
	if(any(names(passed)=="method")) method <- getrunjagsmethod(passed$method) else method <- if('rjags' %in% .packages()) 'rjags' else 'interruptible'
	method <- getrunjagsmethod(method)
	obj <- setup.jagsfile(model=path, datalist=datalist, initlist=initlist, n.chains=n.chains, data=data, inits=inits, monitor=monitor, modules=modules, factories=factories, jags=jags, call.setup=TRUE, method=method)
	
	if(!any(names(passed)=="startburnin")) passed$startburnin <- 5000
	if(!any(names(passed)=="method")){
		passed$method <- if(Sys.info()['user']=='nobody'){ 'simple' }else{ 'interruptible'} 
	}
	if(!any(names(passed)=="method.options")) passed$method.options <- list()
	passed <- c(passed, list(runjags.object=obj, add.monitor=character(0), drop.monitor=character(0), drop.chain=numeric(0), combine=FALSE, jags=jags))
		
	res <- do.call("autoextend.jags",args=passed)
	
	return(res)
		

}

run.JAGS <- run.jags
autorun.JAGS <- autorun.jags
run.JAGSfile <- run.jagsfile
autorun.JAGSfile <- autorun.jagsfile
