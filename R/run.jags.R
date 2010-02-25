run.jags <- function(model=stop("No model supplied"), monitor = stop("No monitored variables supplied"), data=NA,  n.chains=2, inits = replicate(n.chains, NA), burnin = 5000*thin, sample = 10000*thin, adapt=if(burnin<200) 100 else 0, jags = findjags(), silent.jags = FALSE, check.conv = TRUE, plots = TRUE, psrf.target = 1.05, normalise.mcmc = TRUE, check.stochastic = TRUE, modules=c(""), thin = 1, monitor.deviance = FALSE, monitor.pd = FALSE, monitor.popt = FALSE, keep.jags.files = FALSE, tempdir=TRUE, method=if(.Platform$OS.type=='unix' & .Platform$GUI!="AQUA") 'interruptible' else 'simple'){

if(class(method)=='list'){
	xgrid.options <- method
	method <- xgrid.options$method
	
	# Ensure that we have the defaults for everything in case the list is supplied manually:	
	if(!any(names(xgrid.options)=="xgrid.method")) xgrid.options$xgrid.method <- "simple"
	if(!any(names(xgrid.options)=="wait.interval")) xgrid.options$wait.interval <- "10 min"
	if(!any(names(xgrid.options)=="command")) xgrid.options$command <- if(Sys.which('mgrid')=="") 'xgrid -job submit -in $indir $cmd' else 'mgrid -i $indir -t $ntasks $cmd'
	if(!any(names(xgrid.options)=="jagspath")) xgrid.options$jagspath <- '/usr/local/bin/jags'
	if(!any(names(xgrid.options)=="cleanup")) xgrid.options$cleanup <- TRUE
	if(!any(names(xgrid.options)=="separatetasks")) xgrid.options$separatetasks <- FALSE
	if(!any(names(xgrid.options)=="separatejobs")) xgrid.options$separatejobs <- FALSE
	if(!any(names(xgrid.options)=="submitandstop")) xgrid.options$submitandstop <- FALSE
	if(!any(names(xgrid.options)=="jobname")) xgrid.options$jobname <- NA
	
	jobname <- xgrid.options$jobname
	if(is.na(jobname)){
		jobname <- paste('xgridrunJAGS.', Sys.info()['user'], sep='')
	}
	
	if(xgrid.options$xgrid.method=='xgrid.retrieve') method <- 'xgrid.retrieve'
	xgrid.options$separatetasks <- xgrid.options$xgrid.method=='separatetasks'
	xgrid.options$separatejobs <- xgrid.options$xgrid.method=='separatejobs'
	
	xgrid.options$command <- gsub('"', '\"', xgrid.options$command)
	xgrid.options$command <- gsub('\\"', '\"', xgrid.options$command)
	xgrid.options$command <- gsub("'", "\'", xgrid.options$command)
	xgrid.options$command <- gsub("\\'", "\'", xgrid.options$command)
	
	if(!any(xgrid.options$xgrid.method==c('simple', 'separatejobs', 'separatetasks', 'xgrid.retrieve'))) stop(paste('Unrecognised xgrid.method - "', xgrid.options$xgrid.method, '"', sep=''))
	if(xgrid.options$xgrid.method=='separatetasks'&xgrid.options$command=='xgrid -job submit -in $indir $cmd') stop('The basic xgrid command cannot be used with the separatetasks method - either install mgrid (this can be found in the "inst" folder in the runjags package source), specify a command to another script file to generate and submit a plist file, or use the simple or separatejobs method')
	
	if(xgrid.options$submitandstop & tempdir){
		#warning('Cannot use a temporary directory with asynchronous submission')
		tempdir <- FALSE
	}
			
	if(class(xgrid.options$wait.interval)=="numeric" | class(xgrid.options$wait.interval)=="integer"){
		max.time <- xgrid.options$wait.interval *60
	}else{
		if(class(xgrid.options$wait.interval)!="character") stop("wait.interval must be either a numeric or character value")
		str.time <- strsplit(xgrid.options$wait.interval, "")[[1]]

		time.unit <- suppressWarnings(str.time[is.na(as.numeric(str.time)!=str.time)])
		time.unit <- tolower(time.unit[time.unit!=" "][1])
		max.time <- suppressWarnings(as.numeric(paste(na.omit(str.time[as.numeric(str.time)==str.time]) ,collapse="")))
	
		max.time <- max.time * switch(time.unit, d=24*60*60, w=24*60*60*7, h=60*60, m=60, s=1, NA)
		if(is.na(max.time)) stop("Unrecognised unit of xgrid wait interval time -'", time.unit, "'")
		xgrid.options$wait.interval <- max.time
	}
}

# Needed to stop unrelated code messing up:
if(method!='xgrid' & method!='xgrid.retrieve'){
	xgrid.options <- list(separatetasks=FALSE, separatejobs=FALSE, submitandstop=FALSE)
}


if(!any(method==c('xgrid', 'xgrid.retrieve', 'interruptible', 'simple', 'parallel'))) stop(paste("Unsupported method = '", method, "'", sep=""))

updates <- sample

if(.Platform$OS.type=='windows' & method!='simple'){
	warning('Only the simple method is available on Windows')
	method <- 'simple'
}
if(.Platform$GUI=='AQUA' & method=='interruptible') warning("The JAGS session may not be interruptible using the console version of R - use the terminal version instead for this feature")

if(method=='xgrid' | method=='xgrid.retrieve'){
	# Check xgrid is available
	if(.Platform$OS.type=='windows'){
		warning('The xgrid method is not available.  Only the simple method is available on Windows')
		method <- 'simple'
	}else{
		xgridavail <- system('xgrid 2>&1', intern=TRUE)
		if(length(xgridavail)==1){
			stop('xgrid is not available on this machine')
		}
		
		xgridlist <- system('xgrid -job list 2>&1', intern=TRUE)
		if(paste(xgridlist, collapse='\n')==paste(xgridavail, collapse='\n')){
			if(.Platform$GUI=='AQUA') stop('Testing basic xgrid functionality produced an error.  System environmental variables are not accessible from the GUI version of R - either ensure that you have specified the controller hostname and password in the xgrid command or use the console version of R instead') else stop('Testing basic xgrid functionality produced an error.  Ensure that you have either specified the controller hostname and password in the xgrid command or set up envirnomental variables for this.')
		}
	}
}

if(method=='xgrid.retrieve'){
	if(!file.exists(jobname)) stop("The supplied jobname does not exist in the working directory")
	save.directory <- getwd()
	on.exit(setwd(save.directory))
	setwd(jobname)
	load('workingobj.Rsave')
	method <- 'xgrid.retrieve'
}else{

	if(any(c(monitor.pd, monitor.popt)) & (n.chains < 2 | method=='parallel' | (method=='xgrid' & xgrid.options$separatetasks==TRUE))){
		warning("The pD and popt cannot be assessed with only 1 chain or when using separate or parallel chains")
		monitor.pd <- FALSE
		monitor.popt <- FALSE
	}

	if(any(c(monitor.deviance, monitor.pd, monitor.popt))) modules <- c(modules, "dic")
	modules <- unique(modules)
	modules <- na.omit(modules[modules!=""])

	if(as.integer(thin)!=thin | thin < 1) stop("The value supplied for thin must be a positive integer")

	real.runs <- as.integer(updates)
	ini.runs <- as.integer(burnin)
	adapt.runs <- as.integer(adapt)

	if(!require(lattice)){
		stop("The required library 'lattice' is not installed")
	}
	if(!require(coda)){
		stop("The required library 'coda' is not installed")
	}

	test <- testjags(jags, silent=TRUE)
	if(test[[2]][1]==FALSE & method!='xgrid'){
		cat("Unable to call JAGS using '", jags, "' - try specifying the path to the JAGS binary as the jags argument\n", sep="")
		stop("Unable to call JAGS")
	}
	
	
	if(class(inits)=='list'){
		if(length(inits)>1){
			for(i in 1:length(inits)) if(class(inits[[i]])=='list') inits[[i]] <- dump.format(inits[[i]])
		}else{
			inits <- dump.format(inits)
		}		
	}
	
	inits <- unlist(inits)
	if(all(is.na(inits))) inits <- replicate(n.chains, NA)

	if(length(inits)==1){
		inits <- replicate(n.chains, inits)
		if(n.chains!=1) warning("Only one set of initial values was provided.  The same initial values will be used across all chains (this is not recommended)")
	}

	if(!is.na(n.chains)){
		if(length(inits) != n.chains){
			temp <- inits
			inits <- character(n.chains)
		
			suppressWarnings(inits[] <- temp)
			warning("The number of chains specified did not match the number of initial value strings supplied.  Some initial value strings will be recycled or ignored")
		}
	}

	n.chains <- length(inits)
	
	if(length(grep('base::Mersenne-Twister', inits)>0) & as.numeric(test$JAGS.version[1]) < 2) warning('Using the RNG "base::Mersenne-Twister" (used by default for chain 4) may cause problems with restarting subsequent simulations using the end state of previous simulations due to a bug in JAGS version 1.x.  If you encounter the error "Invalid .RNG.state", please update JAGS to version 2.x and try again.  Or, you can change the random number generator by changing the .RNG.name to (for example) "base::Super-Duper" and remove the .RNG.state element of the list.')
		
	if(xgrid.options$separatejobs & length(xgrid.options$command)!=1){
		if(length(xgrid.options$command)!=n.chains) stop("The vector of commands supplied for the xgrid call did not match the number of chains specified")
	}
	if(xgrid.options$separatejobs & length(xgrid.options$command)==1){
		xgrid.options$command <- replicate(n.chains, xgrid.options$command)
	}


	if(class(model)!="character" | length(model)!=1){
		stop("The model must be provided in the form of a single character string")
	}

	if(class(data)=="list") suppressWarnings(try(data <- dump.format(data), silent=TRUE))

	suppressWarnings(if(is.na(data)) class(data) <- "character")

	if(class(data)!="character" | length(data)!=1){
		stop("The data must be provided in R dump format (see dump.format())")
	}

	if(all(!is.na(inits)) & any(class(inits)!="character")){
		stop("Initial values must be provided as a character vector in the R dump format (see dump.format()), with length equal to the number of chains required")
	}

	chains <- length(inits)

	if(chains<1) stop("Number of chains must be greater than 0")

	monitor[monitor==""] <- NA
	if(class(monitor)!="character" | all(is.na(monitor))){
		stop("Monitored variable(s) must be provided in the form of a character vector")
	}

	if(check.conv==TRUE & chains==1){
		warning("Convergence cannot be assessed with only 1 chain")
	}

	modelstring <- paste(model, "\n", sep="")
	monitorcollapse <- paste(">, thin(", thin, ")\nmonitor set <", sep="")
	monitors <- paste("monitor set <", paste(monitor, collapse=monitorcollapse), ">, thin(", thin, ")\n", sep="")
	n.params <- length(monitor)
	params.names <- monitor
	
	initstring <- paste(inits, "\n", sep="")
	
	datastring <- paste(data, "\n", sep="")
	
	#  Write model file

	total.updates <- n.params * updates

	save.directory <- getwd()
	on.exit(setwd(save.directory))

	if(tempdir){
		temp.directory <- tempfile('runjagsdir')
		dir.create(temp.directory)
	}else{
		jobname <- new_unique(jobname, touch=TRUE, type='folder')
		temp.directory <- if(method=='xgrid' & xgrid.options$submitandstop) jobname else new_unique('runjagsfiles', touch=TRUE, type='folder')
		if((temp.directory=="Directory not writable")==TRUE){
			cat("Directory not writable\n")
			return(c("Error", "Write permissions"))
		}
		temp.directory <- paste(getwd(), '/', temp.directory, sep='')
	}

	setwd(temp.directory)


	#output <- file("model.txt", 'w')
	cat(modelstring, file="model.txt",sep="")  
	#close(output)


	## Write data and script and init files

	#output <- file("data.txt", 'w')
	cat(datastring, file="data.txt",sep="")  
	#close(output)


	# New stuff to allow parallelisation:

	parallelmethod <- FALSE
	if(method=='parallel') parallelmethod <- TRUE
	if(method=='xgrid' & xgrid.options$separatetasks) parallelmethod <- TRUE
	if(method=='xgrid' & xgrid.options$separatejobs) parallelmethod <- TRUE

	if(parallelmethod) nsims <- chains else nsims <- 1

	if(nsims!=1){
		if(monitor.deviance | monitor.popt | monitor.pd){
			warning("Unaable to set deviance, popt or pd monitors with parallelised chains")
			monitor.deviance=monitor.popt=monitor.pd <- FALSE
		}
		if(.Platform$OS.type=='windows'){
			warning("Parallelising chains is not supported in Windows")
			parallelise <- FALSE
			nsims <- 1
		}
	}

	for(s in 1:nsims){

	scriptstring <- ""

	if(xgrid.options$separatetasks) pathfix <- paste('sim.', s, '/', sep='') else pathfix <- ''

	if(any(modules!="")){
		for(i in 1:length(modules)){
			scriptstring <- paste(scriptstring, "load <", modules[i], ">\n", sep="")
		}
	}

	if(method=='parallel') scriptstring <- paste(scriptstring, "model in <\"../model.txt\">\n", sep="") else scriptstring <- paste(scriptstring, "model in <\"model.txt\">\n", sep="")
	if(!is.na(data)){
		if(method=='parallel') scriptstring <- paste(scriptstring, "data in <\"../data.txt\">\n", sep="") else scriptstring <- paste(scriptstring, "data in <\"data.txt\">\n", sep="")
	}

	if(nsims==1){
	scriptstring <- paste(scriptstring, "compile, nchains(<", as.integer(chains), ">)\n", sep="")
	for(i in 1:chains){
		if(!is.na(inits[i])) scriptstring <- paste(scriptstring, "parameters in <\"", pathfix, "inits", i, ".txt\">, chain(<", i, ">)\n", sep="")
	}
	}else{
		scriptstring <- paste(scriptstring, "compile, nchains(<1>)\n", sep="")
		if(!is.na(inits[s])) scriptstring <- paste(scriptstring, "parameters in <\"", pathfix, "inits", s, ".txt\">, chain(<1>)\n", sep="")
	}
	scriptstring <- paste(scriptstring, "initialize\n", sep="")
	if(adapt > 0){
		scriptstring <- paste(scriptstring, "adapt <", adapt.runs, ">\n", sep="")
	}
	if(burnin > 0 | adapt <= 0){
		scriptstring <- paste(scriptstring, "update <", ini.runs, ">\n", sep="")
	}
	scriptstring <- paste(scriptstring, monitors, if(monitor.deviance) paste("monitor, type(deviance) thin(", thin, ")\n", sep=""), if(monitor.pd) paste("monitor, type(pD) thin(", thin, ")\n", sep=""), if(monitor.popt) paste("monitor, type(popt) thin(", thin, ")\n", sep=""), "update <", real.runs, ">
	coda <*>, stem(", pathfix, "CODA)\n", sep="")
	if(nsims==1){
	for(i in 1:chains){
	scriptstring <- paste(scriptstring, "parameters to <\"", pathfix, "out", i, ".Rdump\">, chain(<", i, ">)\n", sep="")
	}
	}else{
		scriptstring <- paste(scriptstring, "parameters to <\"", pathfix, "out", s, ".Rdump\">, chain(<1>)\n", sep="")
	}
	if(monitor.deviance) scriptstring <- paste(scriptstring, "monitors to <\"", pathfix, "deviance.Rdump\">, type(deviance)\n", sep="")
	if(monitor.pd) scriptstring <- paste(scriptstring, "monitors to <\"", pathfix, "pd.Rdump\">, type(pD)\n", sep="")
	if(monitor.popt) scriptstring <- paste(scriptstring, "monitors to <\"", pathfix, "popt.Rdump\">, type(popt)\n", sep="")

	scriptstring <- paste(scriptstring, "exit\n", sep="")

	if(nsims==1) output <- file("script.cmd", 'w') else output <- file(paste("script", s, ".cmd", sep=""), 'w')
	cat(scriptstring, file=output,sep="")  
	close(output)
	}

	for(i in 1:chains){
		#output <- file(paste("inits", i, ".txt", sep=""), 'w')
		cat(initstring[i], file=paste("inits", i, ".txt", sep=""),sep="")
		#close(output)
	}

	if(nsims!=1){
		for(s in 1:nsims){

			sim <- paste("sim", s, sep=".")
			#system(paste('mkdir ', sim, sep=''))
			dir.create(sim)
		
			file.copy(from=c(paste('script', s, '.cmd', sep=''), paste('inits', s, '.txt', sep='')), to=sim)
			thed <- getwd()
			setwd(sim)
			file.rename(from=paste('script', s, '.cmd', sep=''), to='script.cmd')
		
			#filecon <- file('scriptlauncher.sh', 'w')
			cat('#!/bin/sh

			', jags, ' < script.cmd > jagsoutput.txt 2>&1 &

			echo $! > jagspid.txt
			', sep='', file='scriptlauncher.sh')
			#close(filecon)

			Sys.chmod('scriptlauncher.sh')
		
			setwd(thed)
		
			unlink(paste('script', s, '.cmd', sep=''))
			unlink(paste('inits', s, '.txt', sep=''))
		}
	
	}

	cat("Calling the simulation... (this may take some time)\n")

	jags.status <- testjags(jags, silent=TRUE)

	jags <- jags.status$JAGS.path

	# Catches when R is killed (or hits an error) and terminates the JAGS process (UNIX only):

	dontfinish <- FALSE
	gottoend <- FALSE
	interrupt <- FALSE
	xgrid.waiting <- FALSE
}

tryCatch({

if(method=='simple'){

if (jags.status$os == "windows"){
	if(jags.status$popen.support == TRUE){
		success <- system(paste(jags, " script.cmd", sep = ""), intern=silent.jags, wait=TRUE, ignore.stderr = !silent.jags, show.output.on.console = !silent.jags)
	}else{
		success <- system(paste(jags, " script.cmd", sep = ""), ignore.stderr = !silent.jags, wait=TRUE, show.output.on.console = !silent.jags)
	}
}else{
	if(silent.jags == FALSE && jags.status$popen.support==TRUE){
		success <- system(paste(jags, "< script.cmd", sep=""), ignore.stderr = FALSE)
	}
	if(silent.jags == TRUE && jags.status$popen.support==TRUE){
		success <- system(paste(jags, "< script.cmd", sep=""), intern=TRUE, ignore.stderr = TRUE)
	}
	if(silent.jags == FALSE && jags.status$popen.support==FALSE){
		success <- system(paste(jags, "< script.cmd", sep=""), ignore.stderr = FALSE)
	}
	if(silent.jags == TRUE && jags.status$popen.support==FALSE){
		success <- system(paste(jags, "< script.cmd > /dev/null", sep=""), ignore.stderr = TRUE)
	}
}

}

if(method=='interruptible'){

	filecon <- file('scriptlauncher.sh', 'w')
	cat('#!/bin/sh
	
	', jags, ' < script.cmd > jagsoutput.txt 2>&1 &
	
	echo $! > jagspid.txt
	exit 0
	', sep='', file=filecon)
	close(filecon)

	Sys.chmod('scriptlauncher.sh')
	
	success <- system('./scriptlauncher.sh', wait=TRUE, intern=FALSE)	

	Sys.sleep(1)

	suppressWarnings(output <- readLines('jagspid.txt'))
	pid <- output[1]
	if(as.numeric(pid)!=as.integer(pid) | pid[s]=="") stop("A problem occured when reading the output of a started process")
	
	output <- tailf('jagsoutput.txt', start=1, min.static=2, stop.when=function(){
		
		pscall <- pipe(paste('ps ', pid, sep=''))
		psout <- readLines(pscall)
		close(pscall)
		return(length(psout)==1)
		}, print=!silent.jags, return=TRUE)
	
	if(output$interrupt==TRUE){
		interrupt <- TRUE
		system(paste('kill ', pid, sep=''), ignore.stderr=TRUE)
		stop("Process interrupted by user")
	}
}

if(method=='parallel'){
	pid <- character(nsims)
	for(s in 1:nsims){
		success <- system(paste('cd sim.', s, '; ./scriptlauncher.sh', sep=''), wait=TRUE, intern=FALSE)
		Sys.sleep(1)
		suppressWarnings(output <- readLines(paste('sim.', s, '/jagspid.txt', sep='')))
		pid[s] <- output[1]
		if(as.numeric(pid[s])!=as.integer(pid[s]) | pid[s]=="") stop("A problem occured when reading the output of a started process")		
	}
	
	if(!silent.jags) cat("Following the progress of chain 1 (the program will wait for all chains to finish before continuing):\n")
	output <- tailf('sim.1/jagsoutput.txt', start=1, min.static=2, stop.when=function(){
		
		psout <- numeric(nsims)
		for(s in 1:nsims){
			pscall <- pipe(paste('ps ', pid[s], sep=''))
			psout[s] <- length(readLines(pscall))
			close(pscall)
		}
		return(all(psout==1))
		}, print=!silent.jags, return=TRUE)
	
	if(output$interrupt==TRUE){
		for(s in 1:nsims){
			system(paste('kill ', pid[s], sep=''), ignore.stderr=TRUE)
		}
		interrupt=TRUE
		stop("Process interrupted by user")
	}
	
	for(s in 1:nsims){
		sim <- paste("sim", s, sep=".")
		thed <- getwd()
		setwd(sim)
		file.rename(from='CODAchain1.txt', to=paste('CODAchain', s, '.txt', sep=''))
		file.copy(from=paste('CODAchain', s, '.txt', sep=''), to='..')
		if(s==1) file.copy(from='CODAindex.txt', to='..')
		file.copy(from=paste('out', s, '.Rdump', sep=''), to='..')
		setwd(thed)
	}
}

if(method=='xgrid'){
	
	if(xgrid.options$separatejobs){
		# Wanted to symlink to the data and model files, but I don't think we can so will have to copy them instead:
				
		for(s in 1:nsims){
			file.copy(from='model.txt', to=paste('sim.', s, '/model.txt', sep=''))
			file.copy(from='data.txt', to=paste('sim.', s, '/data.txt', sep=''))
		}
		unlink('model.txt')
		unlink('data.txt')
		
		jobnames <- paste(jobname, 1:nsims, sep='.chain.')
	}
	
	#filecon <- file('jagslauncher.sh', 'w')

# cd doesn't work on some nodes, so the working directory is left where it is now and paths adjusted accordingly in the script
#	', if(xgrid.options$separatetasks) 'cd sim.$1', '
	cat('#!/bin/sh


pid=$$

echo "" > ', if(xgrid.options$separatetasks) 'sim.$1/', 'jagsout.txt
echo "" > ', if(xgrid.options$separatetasks) 'sim.$1/', 'jagserror.txt
', if(xgrid.options$separatetasks) '( ( echo "Chain "$1":" 2>&1 1>&3 | tee -a sim.$1/jagserror.txt) 3>&1 1>&2) 2>&1 | tee -a sim.$1/jagsout.txt', '

( ( (', xgrid.options$jagspath, ' < ', if(xgrid.options$separatetasks) 'sim.$1/', 'script.cmd; echo $? > .retstat.$pid) 2>&1 1>&3 | tee -a ', if(xgrid.options$separatetasks) 'sim.$1/', 'jagserror.txt) 3>&1 1>&2) 2>&1 | tee -a ', if(xgrid.options$separatetasks) 'sim.$1/', 'jagsout.txt

', if(xgrid.options$separatetasks) '( ( echo "" 2>&1 1>&3 | tee -a sim.$1/jagserror.txt) 3>&1 1>&2) 2>&1 | tee -a sim.$1/jagsout.txt', '

# This makes sure the process has finished before continuing:
wait

returnstat=`cat < .retstat.$$`
rm .retstat.$$

exit $returnstat
	', sep='', file=jobname)
	#close(filecon)
	
	Sys.chmod(jobname)
		
	# Should always be simple - ditched parallel in favour of multipletasks:
	#if(xgrid.options$method=='simple'){

# Again; no longer have ntasks:
#ntasks="', nsims, '"
		
	if(xgrid.options$separatejobs){
		
		for(s in 1:nsims) file.copy(from=jobname, to=paste('sim.', s, '/', jobnames[s], sep=''))
		jobnum <- integer(nsims)
		
		for(s in 1:nsims){
			
			cat('#!/bin/sh
cd ', paste('sim.', s, sep=''), '
cmd="', jobnames[s], '"
ntasks=1
job=', s, '
indir="', paste(temp.directory, "/sim.", s, sep=""), '"
', xgrid.options$command[s], '
			', sep='', file='starter.sh')
		#}

			Sys.chmod('starter.sh')

			success <- system('./starter.sh 2>&1 | tee starteroutput.txt', intern=FALSE)

			if(file.exists(paste(temp.directory, "/sim.", s, '/jobid.txt', sep=""))) tjobnum <- paste(readLines(paste(temp.directory, "/sim.", s, '/jobid.txt', sep="")), collapse='\n') else tjobnum <- paste(readLines('starteroutput.txt'), collapse='\n')

			jobnum[s] <- gsub('[^[:digit:]]', '', paste(tjobnum, collapse=''))

			xgrid.waiting <- TRUE
			Sys.sleep(2)
			xgrid.waiting <- FALSE
			cat("Job ", s, " of ", nsims, " submitted to xgrid\n", sep="")
			
			
		}
		
	}else{
		
		cat('#!/bin/sh
	
cmd="', jobname, '"
ntasks=', nsims, '
job=1
indir="', temp.directory, '"
', xgrid.options$command, '
		', sep='', file='starter.sh')
	#}

		Sys.chmod('starter.sh')
		
		success <- system('./starter.sh 2>&1 | tee starteroutput.txt', intern=FALSE)
						
		if(file.exists('jobid.txt')) jobnum <- paste(readLines('jobid.txt'), collapse='\n') else jobnum <- paste(readLines('starteroutput.txt'), collapse='\n')
		
		jobnum <- gsub('[^[:digit:]]', '', paste(jobnum, collapse=''))

		xgrid.waiting <- TRUE
		Sys.sleep(2)
		xgrid.waiting <- FALSE

		cat("Job submitted to xgrid\n")
		
		
	}
	
	if(xgrid.options$submitandstop){
		cat(jobnum, file='jobid.txt', sep='\n') 
		save(list=ls(), file='workingobj.Rsave')
		gottoend <- TRUE
		return(list(jobname=jobname, jobid=jobnum))
	}else{
		method <- 'xgrid.run'
	}
}

if(method=='xgrid.retrieve' | method=='xgrid.run'){
	success <- xgrid.retrieve(jobnum, wait=(method=='xgrid.run'), wait.interval=xgrid.options$wait.interval, silent=silent.jags, cleanup=xgrid.options$cleanup, directory=temp.directory, jags=TRUE)
	
	if(xgrid.options$separatejobs){
		for(s in 1:nsims){
			sim <- paste("sim", s, sep=".")
			thed <- getwd()
			setwd(sim)
			file.rename(from='CODAchain1.txt', to=paste('CODAchain', s, '.txt', sep=''))
			file.copy(from=paste('CODAchain', s, '.txt', sep=''), to='..')
			if(s==1) file.copy(from='CODAindex.txt', to='..')
			file.copy(from=paste('out', s, '.Rdump', sep=''), to='..')
			setwd(thed)
		}
	}else{
		
		if(xgrid.options$separatetasks){
			for(s in 1:nsims){
				sim <- paste("sim", s, sep=".")
				thed <- getwd()
				setwd(sim)
				file.rename(from='CODAchain1.txt', to=paste('CODAchain', s, '.txt', sep=''))
				file.copy(from=paste('CODAchain', s, '.txt', sep=''), to='..')
				if(s==1) file.copy(from='CODAindex.txt', to='..')
				file.copy(from=paste('out', s, '.Rdump', sep=''), to='..')
				setwd(thed)
			}
		}
	}
	
}

if (file.exists("CODAindex.txt") == FALSE){
  	if (file.exists("JAGS.out") == TRUE){
 		cat("You are using a version of JAGS prior to 0.99.0, which is no longer supported.  Please update JAGS and try again\n")
   		stop("JAGS version not supported")
   	}else{
   		cat("ERROR:  The coda files were not found\n")
   		suppressWarnings(try(cat(success, "\n", sep=""), silent=TRUE))
   		
		dontfinish <- TRUE
		setwd(save.directory)
		if(keep.jags.files & tempdir){
			new.directory <- new_unique('runjagsfiles', touch=TRUE, type='folder')
			if((new.directory=="Directory not writable")==TRUE){
				warning("JAGS files could not be copied to the working directory as it is not writable")
			}else{
				file.copy(from=paste(temp.directory, list.files(temp.directory), sep=.Platform$file.sep), to=new.directory, recursive=TRUE)
			}
		}
		if(tempdir) unlink(temp.directory, recursive = TRUE)
		results <- c("Unable to load coda files")
		return(results)
   	}
}


if((.Platform$GUI == "AQUA" || .Platform$OS.type == "windows") && silent.jags==FALSE){
	flush.console()
	for(i in 1:15) {
		Sys.sleep(0.1)
		cat("")
	}
	flush.console()
}

if(!silent.jags) cat("\n")


cat("Simulation complete.  Reading coda files...\n")

suppressWarnings(inputsuccess <- try(input.data <- read.openbugs(quiet=TRUE), silent=TRUE))

if((class(inputsuccess)=="try-error")){
	filename <- paste("jags.dump", 1, ".R", sep="")
	suppressWarnings(try(inputsuccess <- try(tempinput <- readLines(filename)), silent=TRUE))
	if(class(inputsuccess)=="try-error"){
		
		dontfinish <- TRUE
		
		setwd(save.directory)
		if(keep.jags.files & tempdir){
			new.directory <- new_unique('runjagsfiles', touch=TRUE, type='folder')
			if((new.directory=="Directory not writable")==TRUE){
				warning("JAGS files could not be copied to the working directory as it is not writable")
			}else{
				file.copy(from=paste(temp.directory, list.files(temp.directory), sep=.Platform$file.sep), to=new.directory, recursive=TRUE)
			}
			
		}
		if(tempdir) unlink(temp.directory, recursive = TRUE)
		
		cat("Unable to load coda files or output of a crashed model.  The model may not have compiled correctly, or the monitored nodes may not exist.  Also check that the initial values or prior distributions given are valid and do not conflict with the data.\n")
		results <- c("Unable to load coda files")
		return(results)
	}else{
		cat("The model crashed during the burnin period.  Ensure that the syntax is correct and that appropriate prior distributions and starting values have been given.\n")
		achieved <- 0
		input.data <- "The model crashed during the burnin period"
	}
}else{
	cat("Coda files loaded successfully\n")
	achieved <- niter(input.data)
}	

otheroutputs <- vector("list")

suppressWarnings({
if(monitor.deviance){
	deviance <- try(dget("deviance.Rdump"), silent=TRUE)
	if(class(deviance)=="try-error"){
		warning("There was an error reading the individual parameter deviance")
		deviance <- "There was an error reading the individual parameter deviance"
	}else{
		for(i in 1:length(deviance)){
			dimnames(deviance[[i]]) <- list(1:nrow(deviance[[i]]), paste("chain_", 1:chains, sep=""))
		}
	}
	otheroutputs <- c(otheroutputs, deviance=list(deviance))
}
if(monitor.pd){
	pd <- try(dget("pd.Rdump"), silent=TRUE)
	if(class(pd)=="try-error"){
		warning("There was an error reading the pD")
		pd <- "There was an error reading the pD"
	}else{
	#	for(i in 1:length(deviance)){
	#		dimnames(deviance[[i]]) <- list(1:nrow(deviance[[i]]), paste("chain_", 1:chains, sep=""))
	#	}
	}
	otheroutputs <- c(otheroutputs, pd=list(pd))
}
if(monitor.popt){
	popt <- try(dget("popt.Rdump"), silent=TRUE)
	if(class(popt)=="try-error"){
		warning("There was an error reading the popt")
		popt <- "There was an error reading the popt"
	}else{
	#	for(i in 1:length(deviance)){
	#		dimnames(deviance[[i]]) <- list(1:nrow(deviance[[i]]), paste("chain_", 1:chains, sep=""))
	#	}
	}
	otheroutputs <- c(otheroutputs, popt=list(popt))
}

})

# stuff removed from here for unequal chain lengths

achieved <- achieved * thin
	
if(achieved!=updates){
	crashed <- TRUE
	if(achieved!=0) cat("Simulation crashed at ", achieved, " iterations\n", sep="")
	if(silent.jags == TRUE && jags.status$popen.support==TRUE){
		if(class(success)=="character") cat(paste("The following model trace output may reveal the cause of the crash:\n\n", paste(success, collapse="\n"), "\n\n", sep=""))
	}
	
	crash.end <- character(length=chains)
	for(i in 1:chains){
		filename <- paste("jags.dump", i, ".R", sep="")
		suppressWarnings(inputsuccess <- try(tempinput <- readLines(filename)))
		if(class(inputsuccess)=="try-error"){
			cat("Error reading crash point of chain ", i, ".\n", sep="")
			crash.end[i] <- NA
		}else{
			crash.end[i] <- ""
			for(j in 1:length(tempinput)){
				crash.end[i] <- paste(crash.end[i], tempinput[j], "\n", sep="")
			}
		}
	}
}else{
	crashed <- FALSE
	input.end <- character(length=chains)
	for(i in 1:chains){
		filename <- paste("out", i, ".Rdump", sep="")
		suppressWarnings(inputsuccess <- try(tempinput <- readLines(filename)))
		if(class(inputsuccess)=="try-error"){
			cat("Error reading end point of chain ", i, ".\n", sep="")
			input.end[i] <- NA
		}else{
			input.end[i] <- ""
			for(j in 1:length(tempinput)){
				input.end[i] <- paste(input.end[i], tempinput[j], "\n", sep="")
			}
		}
	}
}

gottoend <- TRUE
# End part of top level tryCatch - ensures that the temp.directory is removed and (unix only) kills JAGS process(es) if they are still up.
}, finally={
	
	if(!dontfinish){
	setwd(save.directory)
	if(keep.jags.files & tempdir){
		new.directory <- new_unique('runjagsfiles', touch=TRUE, type='folder')
		if((new.directory=="Directory not writable")==TRUE){
			warning("JAGS files could not be copied to the working directory as it is not writable")
		}else{
			dir.create(new.directory)
			file.copy(from=paste(temp.directory, list.files(temp.directory), sep=.Platform$file.sep), to=new.directory, recursive=TRUE)
		}
	}
	if(tempdir) unlink(temp.directory, recursive = TRUE)
	
	if(!gottoend){
		if(interrupt){
			results <- c("Process interrupted by user")
			return(results)
		}else{
			if(xgrid.waiting) cat("The execution was halted while waiting for the xgrid job to finish - the job has not been deleted\n")
		}
	}
	}
})


if(any(is.na(unlist(input.data)))){
	
	nastring <- ""
	
	varnames <- dimnames(input.data[[1]])[[2]]
	
	for(i in 1:chains){
		
		for(j in 1:nvar(input.data)){
			
			if(any(is.na(input.data[[i]][,j]))){
				
				nastring <- paste(nastring, if(nastring!="") ", ", varnames[j], " (chain ", i, ")", sep="")
				
			}
			
		}
		
	}
	
	cat(paste("\nOne or more of the values for the monitored variable(s) '", nastring, "' was invalid (missing data).  Ensure that the model syntax is correct and that appropriate starting values have been given.", sep=""))
	results <- c("Unable to load coda files")
	return(results)
}

## Convert dumpstring to a named list to return initial values:

if(crashed) inits <- unlist(crash.end) else inits <- unlist(input.end)
inits.out <- vector('list', length=n.chains)

for(i in 1:n.chains){
	str <- inits[i]
	str <- gsub("<-", "=", str)
	str <- gsub("`", "", str)
	str <- gsub("= \n", "=", str)
	str <- gsub("\n", ",", str)
	if(strsplit(str, split="")[[1]][length(strsplit(str, split="")[[1]])] == ",") str <- paste(strsplit(str, split="")[[1]][1:(length(strsplit(str, split="")[[1]])-1)], collapse="")
	inits.out[[i]] <- eval(parse(text=paste('list(', str, ')'))) 
}

names(inits.out) <- paste('Chain.', 1:n.chains, sep='')


if(plots==TRUE & achieved!=0){
	success <- try({
	final.mcmc <- input.data
	plot1 = plot2 = vector('list', length=length(varnames(final.mcmc)))
	names(plot1) = names(plot2) <- varnames(final.mcmc)
	thinned.mcmc <- combine.mcmc(list(final.mcmc), collapse.chains=FALSE, return.samples=1000)

	#startdev <- dev.list()

	#a <- dev.new()
	#if(options("device")$device=="x11") x11()

	for(i in 1:length(varnames(final.mcmc))){

		plotdata <- thinned.mcmc[,c(i,i)] # xyplot throws an error if there is only 1 variable, so double the plot...
		varnames(plotdata)[2] <- 'dummy' # Different name prevents warning about duplicate factors
		plot1[[i]] <- xyplot(plotdata, layout=c(1,1), ylab="Value", xlab="Iteration")
		class(plot1[[i]]) <- "plotindpages"
		plot2[[i]] <- densityplot(plotdata, layout=c(1,1), ylab="Density", xlab="Value")
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
	#	try(a <- dev.off(), silent=TRUE)
	#}
	
	})
	if(class(success)=="try-error"){
		plot1 = plot2 <- "An unexpected error occured while attempting to plot graphs"
		warning("An unexpected error occured while attempting to plot graphs")
	}
}else{
	plot1 = plot2 <- "Plots not produced when plots==FALSE"
}


if(check.conv==TRUE & achieved!=0){
	success <- try({
		
	
	if(chains > 1){
		convergence <- safe.gelman.diag(normalise.mcmc(input.data, normalise = normalise.mcmc, warn=TRUE, check.stochastic = check.stochastic), transform=FALSE, autoburnin=TRUE)
		
		convergence <- c(convergence, psrf.target=psrf.target)
		class(convergence) <- "gelman.with.target"
		
		n.params <- nrow(convergence$psrf)

	}else{	
		convergence <- "Convergence cannot be assessed using only 1 chain"
		unconverged <- 0
		param.conv <- 1
		n.params <- 1
	}
	autocorrelation <- autocorr.diag(normalise.mcmc(input.data, normalise = FALSE, warn = FALSE, check.stochastic = check.stochastic))

	autocorrelated <- 0
	unconverged <- 0
	
	for(j in 1:n.params){
		if(chains > 1){
			param.conv <- convergence$psrf[j, 1]
			if(!is.na(param.conv)){
				if(param.conv > 1.05){
					unconverged <- unconverged + 1
				}
			}
		}
	}
	

	for(j in 1:n.params){
		param.autocorr <- autocorrelation[3,j]
		if(!is.na(param.autocorr)){
			if(param.autocorr > 0.1){
				autocorrelated <- autocorrelated + 1
			}
		}
	
	}
	
	if(n.chains > 1){
		if(class(convergence$mpsrf)!="numeric"){
			mpsrfstring <- " (Unable to calculate the multi-variate psrf)"
		}else{
			mpsrfstring <- paste(" (multi-variate psrf = ", round(convergence$mpsrf, digits=3), ")", sep="")
		}
	}
	
	if(!is.na(param.conv)){
		if(unconverged > 0){
			if(n.params==1) cat("Convergence failed for this run after ", updates, " iterations (psrf = ", round(convergence$psrf[1,1], digits=3), ")\n", sep="") else cat("Convergence failed for this run for ", unconverged, " parameter(s) after ", updates, " iterations", mpsrfstring, "\n", sep="")
		}else{
			if(n.chains > 1) cat("The Gelman-Rubin statistic is below 1.05 for all parameters\n")
			if(n.chains==0) cat("The Gelman-Rubin statistic could not be calculated for these chains\n")
		}
	}else{
		cat("The Gelman-Rubin statistic could not be calculated for these chains\n")
	}
	
	if(!is.na(param.autocorr)){
		if(autocorrelated > 0){
			cat("IMPORTANT:  There was a high degree of autocorrelation for ", autocorrelated, " parameter(s)\n", sep="")
		}else{
			#cat("Convergence achieved for this run\n")
		}
	}else{
		cat("Autocorrelation could not be calculated for these chains\n")
	}
	
	}, silent=FALSE)
	if(class(success)=="try-error"){
		cat("An error occured when assessing convergence\n")
		convergence <- "An error occured when assessing convergence"
		autocorrelation <- "An error occured when assessing convergence and autocorrelation"
	}

	options(show.error.messages = FALSE)
	suppressWarnings(tsummary <- summary(combine.mcmc(input.data, collapse.chains=FALSE)))
	options(show.error.messages = TRUE)	
	
	if(crashed==TRUE){
		return(c(list(mcmc=input.data, crash.end=inits.out, burnin=burnin+adapt, sample=achieved, thin=thin,  summary=tsummary, psrf=convergence, autocorr=autocorrelation, trace=plot1, density=plot2), otheroutputs))
	}else{
		return(c(list(mcmc=input.data, end.state=inits.out, burnin=burnin+adapt, sample=sample, thin=thin, summary=tsummary, psrf=convergence, autocorr=autocorrelation, trace=plot1, density=plot2), otheroutputs))
	}
	
}else{

	if(crashed==TRUE){
		return(c(list(mcmc=input.data, crash.end=inits.out, burnin=burnin+adapt, sample=achieved, thin=thin, summary="Summary not produced when check.conv==FALSE", psrf="Potential scale reductionf factors not produced when check.conv==FALSE", autocorr="Autocorrelation statistics not produced when check.conv==FALSE", trace=plot1, density=plot2), otheroutputs))
	}else{
		return(c(list(mcmc=input.data, end.state=inits.out, burnin=burnin+adapt, sample=sample, thin=thin, summary="Summary not produced when check.conv==FALSE", psrf="Potential scale reductionf factors not produced when check.conv==FALSE", autocorr="Autocorrelation statistics not produced when check.conv==FALSE", trace=plot1, density=plot2), otheroutputs))
	}

}
}

run.JAGS <- run.jags