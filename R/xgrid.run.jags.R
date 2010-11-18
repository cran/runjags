xgrid.run.jags <- function(wait.interval="10 min", xgrid.method='simple',  jagspath='/usr/local/bin/jags', jobname=NA, cleanup=TRUE, sub.app=if(!file.exists(Sys.which('mgrid'))) 'xgrid -job submit -in "$indir"' else 'mgrid -t $ntasks -i "$indir"', sub.options="", sub.command=paste(sub.app, sub.options, '"$cmd"', sep=' '), ...){
	
	# **If these defaults are changed then remember to change the corresponding defaults in run.jags**
	return(run.jags(method=list(method='xgrid', wait.interval=wait.interval, command=sub.command, xgrid.method=xgrid.method, jagspath=jagspath, cleanup=cleanup, jobname=jobname, submitandstop=FALSE), ...))
	
	
}

xgrid.autorun.jags <- function(wait.interval="10 min", xgrid.method='simple',  jagspath='/usr/local/bin/jags', jobname=NA, cleanup=TRUE, sub.app=if(!file.exists(Sys.which('mgrid'))) 'xgrid -job submit -in "$indir"' else 'mgrid -t $ntasks -i "$indir"', sub.options="", sub.command=paste(sub.app, sub.options, '"$cmd"', sep=' '), ...){
	
	return(autorun.jags(method=list(method='xgrid', wait.interval=wait.interval, command=sub.command, xgrid.method=xgrid.method, jagspath=jagspath, cleanup=cleanup, jobname=jobname, submitandstop=FALSE), ...))

}

xgrid.run.jagsfile <- function(wait.interval="10 min", xgrid.method='simple',  jagspath='/usr/local/bin/jags', jobname=NA, cleanup=TRUE, sub.app=if(!file.exists(Sys.which('mgrid'))) 'xgrid -job submit -in "$indir"' else 'mgrid -t $ntasks -i "$indir"', sub.options="", sub.command=paste(sub.app, sub.options, '"$cmd"', sep=' '), ...){
	
	passthrough <- list(...)
	if(is.null(passthrough$inits)) inits <- "fromxgrid" else inits <- NA
	if(is.null(passthrough$data)) data <- "fromxgrid" else data <- NA

	return(run.jagsfile(inits=inits, data=data, method=list(method='xgrid', wait.interval=wait.interval, command=sub.command, xgrid.method=xgrid.method, jagspath=jagspath, cleanup=cleanup, jobname=jobname, submitandstop=FALSE), ...))
}

xgrid.autorun.jagsfile <- function(wait.interval="10 min", xgrid.method='simple',  jagspath='/usr/local/bin/jags', jobname=NA, cleanup=TRUE, sub.app=if(!file.exists(Sys.which('mgrid'))) 'xgrid -job submit -in "$indir"' else 'mgrid -t $ntasks -i "$indir"', sub.options="", sub.command=paste(sub.app, sub.options, '"$cmd"', sep=' '), ...){

	passthrough <- list(...)
	if(is.null(passthrough$inits)) inits <- "fromxgrid" else inits <- NA
	if(is.null(passthrough$data)) data <- "fromxgrid" else data <- NA
	
	return(run.jagsfile(inits=inits, data=data, autorun=TRUE, method=list(method='xgrid', wait.interval=wait.interval, command=sub.command, xgrid.method=xgrid.method, jagspath=jagspath, cleanup=cleanup, jobname=jobname, submitandstop=FALSE), ...))
}


xgrid.submit.jags <- function(xgrid.method='simple', jagspath='/usr/local/bin/jags', jobname=NA, sub.app=if(!file.exists(Sys.which('mgrid'))) 'xgrid -job submit -in "$indir"' else 'mgrid -t $ntasks -i "$indir"', sub.options="", sub.command=paste(sub.app, sub.options, '"$cmd"', sep=' '), ...){
	
	# **If these defaults are changed then remember to change the corresponding defaults in run.jags**
	return(run.jags(method=list(method='xgrid', command=sub.command, xgrid.method=xgrid.method, jagspath=jagspath,  jobname=jobname, submitandstop=TRUE), ...))
	
	
}

xgrid.submit.jagsfile <- function(xgrid.method='simple', jagspath='/usr/local/bin/jags', jobname=NA, sub.app=if(!file.exists(Sys.which('mgrid'))) 'xgrid -job submit -in "$indir"' else 'mgrid -t $ntasks -i "$indir"', sub.options="", sub.command=paste(sub.app, sub.options, '"$cmd"', sep=' '), ...){
	
	passthrough <- list(...)
	if(is.null(passthrough$inits)) inits <- "fromxgrid" else inits <- NA
	if(is.null(passthrough$data)) data <- "fromxgrid" else data <- NA

	return(run.jagsfile(inits=inits, data=data, method=list(method='xgrid', command=sub.command, xgrid.method=xgrid.method, jagspath=jagspath, jobname=jobname, submitandstop=TRUE), ...))
}

xgrid.results.jags <- function(jobname, cleanup=TRUE, ...){
	
	if(is.list(jobname)) jobname <- jobname$jobname
	#if(partial.retrieve) xm <- 'xgrid.retrieve.part' else xm <- 'xgrid.retrieve'
	xm <- 'xgrid.retrieve'
	return(xgrid.run.jags(xgrid.method=xm, jobname=jobname, cleanup=cleanup, ...))

}


xgrid.run.JAGS <- xgrid.run.jags
xgrid.autorun.JAGS <- xgrid.autorun.jags
xgrid.run.JAGSfile <- xgrid.run.jagsfile
xgrid.autorun.JAGSfile <- xgrid.autorun.jagsfile
xgrid.submit.JAGS <- xgrid.submit.jags
xgrid.submit.JAGSfile <- xgrid.submit.jagsfile
xgrid.results.JAGS <- xgrid.results.jags
