xgrid.run <- function(f=function(iteration){}, niters, object.list=list(), file.list=character(0), threads=min(niters,100), arguments=as.list(1:niters), jobname=NA, wait.interval="10 min", xgrid.method=if(threads==1) 'simple' else if(!file.exists(Sys.which('mgrid'))) 'separatejobs' else 'separatetasks',  Rpath='/usr/bin/R', Rbuild='64', cleanup=TRUE, submitandstop=FALSE, tempdir=!submitandstop, keep.files=FALSE, show.output=TRUE, max.filesize="1GB", sub.app=if(!file.exists(Sys.which('mgrid'))) 'xgrid -job submit -in "$indir"' else 'mgrid -t $ntasks -i "$indir"', sub.options="", sub.command=paste(sub.app, sub.options, '"$cmd"', sep=' '), ...){
		
	if(class(max.filesize)=="numeric" | class(max.filesize)=="integer"){
		max.filesize <- max.filesize * 1024^3# DEFAULT IS GB
	}else{
		if(class(max.filesize)!="character") stop("max.filesize must be either a numeric or character value")
		str.time <- strsplit(max.filesize, "")[[1]]

		time.unit <- suppressWarnings(str.time[is.na(as.numeric(str.time)!=str.time)])
		time.unit <- tolower(time.unit[time.unit!=" "][1])
		max.filesize <- suppressWarnings(as.numeric(paste(na.omit(str.time[as.numeric(str.time)==str.time]) ,collapse="")))
	
		max.filesize <- max.filesize * switch(time.unit, g=1024^3, m=1024^2, k=1024, b=1, NA)
		if(is.na(max.filesize)) stop("Unrecognised unit of max file size -'", time.unit, "'")
	}
	
	if(class(wait.interval)=="numeric" | class(wait.interval)=="integer"){
		max.time <- wait.interval *60
	}else{
		if(class(wait.interval)!="character") stop("wait.interval must be either a numeric or character value")
		str.time <- strsplit(wait.interval, "")[[1]]

		time.unit <- suppressWarnings(str.time[is.na(as.numeric(str.time)!=str.time)])
		time.unit <- tolower(time.unit[time.unit!=" "][1])
		max.time <- suppressWarnings(as.numeric(paste(na.omit(str.time[as.numeric(str.time)==str.time]) ,collapse="")))
	
		max.time <- max.time * switch(time.unit, d=24*60*60, w=24*60*60*7, h=60*60, m=60, s=1, NA)
		if(is.na(max.time)) stop("Unrecognised unit of xgrid wait interval time -'", time.unit, "'")
		wait.interval <- max.time
	}
	command <- sub.command
	# Check xgrid is available
	if(.Platform$OS.type=='windows'){
		stop('xgrid functions are only available on machines running Mac OS X and with access to an Xgrid controller')
	}else{
		xgridavail <- suppressWarnings(system('xgrid 2>&1', intern=TRUE))
		if(length(xgridavail)==1){
			stop('xgrid is not available on this machine')
		}

		xgridlist <- suppressWarnings(system('xgrid -job list 2>&1', intern=TRUE))
		if(paste(xgridlist, collapse='\n')==paste(xgridavail, collapse='\n')){
			if(.Platform$GUI=='AQUA') stop('Testing basic xgrid functionality produced an error.  System environmental variables set in the .profile file are not accessible from the GUI version of R - either export the XGRID_CONTROLLER_HOSTNAME and password variables using Sys.setenv() or use the console version of R instead') else stop('Testing basic xgrid functionality produced an error.  Ensure that you have either specified the controller hostname and password in the xgrid command or set up envirnomental variables using Sys.setenv()')
		}
	}
	
	if(class(object.list)=='character'){
		new.list <- vector('list')
		for(i in object.list){
			new.list <- c(new.list, list(get(i, envir=parent.frame(2))))
		}
		names(new.list) <- object.list
		object.list <- new.list
	}
	
	if(class(object.list)!='list') stop('The object.list supplied must be a named list or character vector')
	if(is.null(names(object.list)) & length(object.list) > 0) stop('The object.list supplied must be a named list or character vector')
	
	objects <- object.list
	
	xg.extra.args <- list(...)
	xg.arguments <- arguments
	
	if(any(names(objects)=="")) stop('All objects to be passed to the function (either supplied in the object.list or specified in the xgrid.run function call) must be named')
	if(any(unlist(lapply(names(objects), function(x) return(strsplit(x, 'xg.')[[1]][1])))=="")) stop("Sorry - you can't use any variable names with the prefix 'xg.' in the object list as they could cause naming conflicts with variables used to start and run the function on Xgrid")
	
	command <- gsub('"', '\"', command)
	command <- gsub('\\"', '\"', command)
	command <- gsub("'", "\'", command)
	command <- gsub("\\'", "\'", command)
	
	if(xgrid.method=='xgrid.retrieve.part'){
		partialretrieve <- TRUE
		xgrid.method <- 'xgrid.retrieve'
	}else{
		partialretrieve <- FALSE
	}

	if(!xgrid.method=='xgrid.retrieve'){
	
		separatetasks <- xgrid.method=='separatetasks'
		separatejobs <- xgrid.method=='separatejobs'
	
		if(!any(xgrid.method==c('simple', 'separatejobs', 'separatetasks', 'xgrid.retrieve'))) stop(paste('Unrecognised xgrid.method - "', xgrid.method, '"', sep=''))
		if(xgrid.method=='separatetasks' & length(grep('xgrid -job', command))>0) stop('The basic xgrid command cannot be used with the separatetasks method - either install mgrid (this can be found in the "inst" folder in the runjags package source), specify a command to another script file to generate and submit a plist file, or use the simple or separatejobs method')
	
		if(threads > 1 & xgrid.method=='simple') stop("The simple xgrid method is limited to a single thread")
	
		if(class(objects)!='list') stop('The objects must be supplied as a named list')
		if(is.null(names(objects)) & length(objects)!=0) stop('The objects must be supplied as a named list')
		if(any(names(objects)=='')) stop('The objects must be supplied as a named list')
	
		if(length(objects)==0) objects$blank <- 0
	
		# Do this in a sub-function to avoid naming conflicts:
		saveobjects <- function(objects){
			for(i in 1:length(objects)){
				assign(names(objects)[i], objects[[i]])
			}
			save(list=names(objects), file='Robjects.Rsave')
			return(file.exists('Robjects.Rsave'))
		}
		
		if(separatejobs & length(command)!=1){
			if(length(command)!=threads) stop("The vector of commands supplied for the xgrid call did not match the number of threads specified")
		}
		if(separatejobs & length(command)==1){
			command <- replicate(threads, command)
		}
	
	}
	

	gottoend <- FALSE
	tryCatch({
	
	
	if(xgrid.method=='xgrid.retrieve'){
		if(!file.exists(jobname)) stop("The supplied jobname does not exist in the working directory")
		swcat('Retrieving the following xgrid job: "', jobname, '"\n', sep='')
		save.directory <- getwd()
		on.exit(setwd(save.directory))
		setwd(jobname)
		load('workingobj.Rsave')
		xgrid.method <- 'xgrid.retrieve'
		submitandstop <- FALSE
	}else{

		save.directory <- getwd()
		on.exit(setwd(save.directory))

		if(submitandstop & tempdir){
			warning('Cannot use a temporary directory with asynchronous submission')
			tempdir <- FALSE
		}
	
		if(threads > niters){
			warning('The number of threads was reduced to match the number of iterations')
			threads <- niters
		}
	
		if(is.na(jobname)){
			jobname <- paste('xgridrunR.', Sys.info()['user'], sep='')
		}
	
		if(tempdir){
			temp.directory <- tempfile('xgridrundir')
			dir.create(temp.directory)
		}else{
			temp.directory=jobname <- new_unique(jobname, touch=TRUE, type='folder')
			if((temp.directory=="Directory not writable")==TRUE){
				swcat("Directory not writable\n")
				return(c("Error", "Write permissions"))
			}
			temp.directory <- paste(getwd(), '/', temp.directory, sep='')
		}
		
		if(length(file.list)>0){
			for(p in file.list){
				success <- file.copy(from=p, to=temp.directory)
				if(!success) stop(paste("File '", p, "' not found in the working directory", sep=""))
			}
		}
		
		setwd(temp.directory)

		success <- saveobjects(objects)
		
		filesize <- as.numeric(system("ls -l Robjects.Rsave | awk '{print $5}'", intern=TRUE))
		if(filesize>max.filesize) stop('The object list supplied is greater than the maximum filesize specified.  You could reduce the number of objects supplied by making some large files available on a shared volume, or increase max.filesize (this might crash your xgrid controller...)')
		
		if(threads==niters){
			iterations <- as.list(1:niters)
		}else{
			t <- 1
			iterations <- vector('list', length=threads)
			for(i in 1:niters){
				iterations[[t]] <- c(iterations[[t]], i)
				t <- t+1
				if(t>threads) t <- 1
			}
		}
		
		# The maximum filesize is TOTAL, so assume all threads are similar and:
		if(xgrid.method=='separatetasks') max.filesize <- max.filesize/threads
		
		xg.f <- f
		xg.iterations <- iterations
		xg.max.filesize <- max.filesize
		
		save(xg.f, xg.iterations, xg.arguments, xg.extra.args, xg.max.filesize, file='Rfunction.Rsave')
		
		if(is.na(Rbuild)) Rbuild <- ''
		if(Rbuild==' ') Rbuild <- ''
		
		if(!any(Rbuild==c("64", "32", ""))) warning(paste('Ignoring unrecognised Rbuild "', Rbuild, '".  Use one of "32", "64" or leave blank if no preference', sep=""))
				
		if(separatejobs){
			jobnames <- paste(jobname, 1:threads, sep='.thread.')
			for(s in 1:threads){
				cat('#!/bin/sh
pid=$$

alias myr=\'', Rpath, '\'
', if(Rbuild!="") paste('if [ -f \'', Rpath, Rbuild, '\' ]; then
alias myr=\'', Rpath, Rbuild, '\'
fi
test=`', Rpath, Rbuild, ' --version 2>&1`
if [ `echo $?` != 0 ]; then
alias myr=\'', Rpath, '\'
fi
', sep=''), '

( ( (myr --slave --no-save -e "xg.task <- ', s, '; xg.path <- Sys.getenv(\'PATH\'); xg.path <- paste(xg.path, \':/bin:/usr/local/bin:/usr/bin\', sep=\'\'); Sys.setenv(PATH=xg.path); load(\'Robjects.Rsave\', envir=.GlobalEnv); load(\'Rfunction.Rsave\', envir=.GlobalEnv); xg.iteration <- xg.iterations[[xg.task]]; for(xg.i in xg.iteration){ xg.temp <- \'Error\'; try(xg.temp <- do.call(xg.f, c(list(xg.arguments[[xg.i]]), xg.extra.args))); assign(paste(\'iteration.\', xg.i, sep=\'\'), xg.temp)}; save(list=paste(\'iteration.\', xg.iteration, sep=\'\'), file=paste(\'results.\', xg.task, \'.Rsave\', sep=\'\')); xg.filesize <- as.numeric(system(paste(\'ls -l results.\', xg.task, \'.Rsave | awk \\\'{print \\$5}\\\'\', sep=\'\'), intern=TRUE)); if(xg.filesize>xg.max.filesize){ xg.temp <- \'Maximum file size exceeded.  You could either try again using more jobs, reduce the number of objects returned by the function, or save files to a shared drive rather than returning them\'; for(xg.i in xg.iteration) assign(paste(\'iteration.\', xg.i, sep=\'\'), xg.temp); save(list=paste(\'iteration.\', xg.iteration, sep=\'\'), file=paste(\'results.\', xg.task, \'.Rsave\', sep=\'\'))};"; echo $? > .retstat.$pid) 2>&1 1>&3 | tee Rerror.$1.txt) 3>&1 1>&2) 2>&1 | tee Rout.$1.txt

# This makes sure the process has finished before continuing:
wait

returnstat=`cat < .retstat.$$`
rm .retstat.$$

exit $returnstat

', sep='', file=jobnames[s])
				Sys.chmod(jobnames[s])
			}
		}else{
			cat('#!/bin/sh
pid=$$

alias myr=\'', Rpath, '\'
', if(Rbuild!="") paste('if [ -f \'', Rpath, Rbuild, '\' ]; then
alias myr=\'', Rpath, Rbuild, '\'
fi
test=`', Rpath, Rbuild, ' --version 2>&1`
if [ `echo $?` != 0 ]; then
alias myr=\'', Rpath, '\'
fi
', sep=''), '

echo "" > Rout.', if(separatetasks) '$1.' else '1', '.txt
echo "" > Rerror.', if(separatetasks) '$1.' else '1', '.txt
', if(separatetasks) '( ( echo "\nTask "$1":" 2>&1 1>&3 | tee -a Rerror.$1.txt) 3>&1 1>&2) 2>&1 | tee -a Rout.$1.txt', '

( ( (myr --slave --no-save -e "xg.task <- ', if(separatetasks) '$1' else '1', '; xg.path <- Sys.getenv(\'PATH\'); xg.path <- paste(xg.path, \':/bin:/usr/local/bin:/usr/bin\', sep=\'\'); Sys.setenv(PATH=xg.path); load(\'Robjects.Rsave\', envir=.GlobalEnv); load(\'Rfunction.Rsave\', envir=.GlobalEnv); xg.iteration <- xg.iterations[[xg.task]]; for(xg.i in xg.iteration){ xg.temp <- \'Error\'; try(xg.temp <- do.call(xg.f, c(list(xg.arguments[[xg.i]]), xg.extra.args))); assign(paste(\'iteration.\', xg.i, sep=\'\'), xg.temp)}; save(list=paste(\'iteration.\', xg.iteration, sep=\'\'), file=paste(\'results.\', xg.task, \'.Rsave\', sep=\'\')); xg.filesize <- as.numeric(system(paste(\'ls -l results.\', xg.task, \'.Rsave | awk \\\'{print \\$5}\\\'\', sep=\'\'), intern=TRUE)); if(xg.filesize>xg.max.filesize){ xg.temp <- \'Maximum file size per task exceeded.  You could either try again using multiple jobs, reduce the number of objects returned by the function, or save files to a shared drive rather than returning them\'; for(xg.i in xg.iteration) assign(paste(\'iteration.\', xg.i, sep=\'\'), xg.temp); save(list=paste(\'iteration.\', xg.iteration, sep=\'\'), file=paste(\'results.\', xg.task, \'.Rsave\', sep=\'\'))};"; echo $? > .retstat.$pid) 2>&1 1>&3 | tee -a Rerror.', if(separatetasks) '$1' else '1', '.txt) 3>&1 1>&2) 2>&1 | tee -a Rout.', if(separatetasks) '$1' else '1', '.txt

', if(separatetasks) '( ( echo "\n" 2>&1 1>&3 | tee -a Rerror.$1.txt) 3>&1 1>&2) 2>&1 | tee -a Rout.$1.txt', '

# This makes sure the process has finished before continuing:
wait

returnstat=`cat < .retstat.$$`
rm .retstat.$$

exit $returnstat

', sep='', file=jobname)
			Sys.chmod(jobname)
	
		}

	
		
	
		if(separatejobs){

			jobnum <- integer(threads)

			for(s in 1:threads){

				cat('#!/bin/sh
cmd="', jobnames[s], '"
ntasks=1
job=', s, '
indir="', temp.directory, '"
', command[s], '
', sep='', file='starter.sh')
			#}

				Sys.chmod('starter.sh')
				success <- system('( ./starter.sh 2>&3 | tee .starterout.txt) 3>&2 | tee starteroutput.txt', intern=FALSE)

				if(file.exists('jobid.txt')) tjobnum <- paste(readLines('jobid.txt'), collapse='\n') else tjobnum <- paste(readLines('.starterout.txt'), collapse='\n')

				jobnum[s] <- gsub('[^[:digit:]]', '', paste(tjobnum, collapse=''))

				xgrid.waiting <- TRUE
				Sys.sleep(2)
				xgrid.waiting <- FALSE
				
				if(jobnum[s]=='' | as.numeric(jobnum[s])>10^6) stop(paste("There was an error submitting job number ", s, " to Xgrid - no Job ID was returned by mgrid/xgrid", sep=''))
				
				swcat("Job ", s, " of ", threads, " submitted to xgrid\n", sep="")

			}

		}else{

			cat('#!/bin/sh

cmd="', jobname, '"
ntasks=', threads, '
job=1
indir="', temp.directory, '"
', command, '
', sep='', file='starter.sh')
		#}

			Sys.chmod('starter.sh')

			success <- system('( ./starter.sh 2>&3 | tee .starterout.txt) 3>&2 | tee starteroutput.txt', intern=FALSE)

			if(file.exists('jobid.txt')) jobnum <- paste(readLines('jobid.txt'), collapse='\n') else jobnum <- paste(readLines('starteroutput.txt'), collapse='\n')

			jobnum <- gsub('[^[:digit:]]', '', paste(jobnum, collapse=''))

			xgrid.waiting <- TRUE
			Sys.sleep(2)
			xgrid.waiting <- FALSE
			
			if(jobnum=='' | as.numeric(jobnum)>10^6) stop("There was an error submitting your job to Xgrid - no Job ID was returned")
			swcat('Your job (name: "', jobname, '") has been succesfully uploaded to xgrid\n', sep='')


		}
		xgrid.method <- 'xgrid.run'
	}
	
	if(submitandstop){
		cat(jobnum, file='jobid.txt', sep='\n')
		savelist <- ls()
		savelist <- savelist[savelist!='keep.files' & savelist!='cleanup' & savelist!='show.output' & savelist!='partialretrieve']
		save(list=savelist, file='workingobj.Rsave')
		gottoend <- TRUE		
		return(list(jobname=jobname, jobid=jobnum))
	}
	
	xgridoutput <- xgrid.retrieve(jobnum, wait=(xgrid.method=='xgrid.run'), wait.interval=wait.interval, silent=!show.output, cleanup=cleanup, directory=temp.directory, partialretrieve=partialretrieve)
	
	gottoend <- TRUE
		
	}, finally={
		
		if(!submitandstop & tempdir & keep.files){
			setwd(save.directory)
			new.directory <- new_unique(jobname, touch=TRUE, type='folder')
			if((new.directory=="Directory not writable")==TRUE){
				warning("Xgrid files could not be copied to the working directory as it is not writable")
			}else{
				file.copy(from=paste(temp.directory, list.files(temp.directory), sep=.Platform$file.sep), to=new.directory, recursive=TRUE)
			}
			unlink(temp.directory, recursive=TRUE)
		}
		
		if(!gottoend){
			if(xgrid.waiting) swcat("The execution was halted while waiting for the xgrid job to finish - the job has not been deleted\n")
		}
	})
	
	success <- try(results <- vector('list', length=length(unlist(iterations))), silent=TRUE)
	if(class(success)=='try-error') stop('The results were not in the expected format - if this job was started using xgrid.submit.jags then use the xgrid.results.jags function to retrieve the results')
	names(results) <- paste('iteration.', 1:length(results), sep='')
	failed <- logical(length(unlist(iterations)))
	for(t in 1:length(iterations)){
		options(show.error.messages = FALSE)
		suppressWarnings(success <- try(load(paste('results.', t, '.Rsave', sep='')), silent=TRUE))
		options(show.error.messages = TRUE)
		if(class(success)=='try-error'){
			for(i in iterations[[t]]){
				results[[paste('iteration.', i, sep='')]] <- 'Job or task incomplete'
				failed[i] <- TRUE
			}
		}else{
			for(i in iterations[[t]]){
				results[[paste('iteration.', i, sep='')]] <- get(paste('iteration.', i, sep=''))
			}
		}
	}
	
	#if(any(failed)) cat('Results for iterations ', paste(which(failed),collapse=','), ' were not returned\n', sep='')
	if(any(failed)) swcat('\nJob ', round(1-((sum(failed)/length(failed))), digits=2)*100, '% complete\n', sep='') else swcat('\nJob complete\n')	
	# Don't think tacking this onto the end of the return is really appropriate...
	#results <- c(results, list(output=paste(xgridoutput,collapse='\n')))
	return(results)
	
}

xgrid.submit <- function(f=function(iteration){}, niters, object.list=list(), file.list=character(0), threads=min(niters,100), arguments=as.list(1:niters), jobname=NA, xgrid.method=if(threads==1) 'simple' else if(!file.exists(Sys.which('mgrid'))) 'separatejobs' else 'separatetasks',  Rpath='/usr/bin/R', Rbuild='64', show.output=TRUE, max.filesize='1GB', sub.app=if(!file.exists(Sys.which('mgrid'))) 'xgrid -job submit -in "$indir"' else 'mgrid -t $ntasks -i "$indir"', sub.options="", sub.command=paste(sub.app, sub.options, '"$cmd"', sep=' '),  ...){

	return(xgrid.run(f=f, niters=niters, object.list=object.list, file.list=file.list, threads=threads, arguments=arguments, jobname=jobname, sub.command=sub.command, xgrid.method=xgrid.method, Rpath=Rpath, Rbuild=Rbuild, tempdir=FALSE, submitandstop=TRUE, show.output=show.output, max.filesize=max.filesize, ...))
}


xgrid.results <- function(jobname, partial.retrieve=FALSE, cleanup=!partial.retrieve, keep.files=FALSE, show.output=TRUE){
	
	if(is.list(jobname)) jobname <- jobname$jobname
	if(partial.retrieve) xm <- 'xgrid.retrieve.part' else xm <- 'xgrid.retrieve'
	# arguments=NA is needed to keep xgrid.run from complaining about 1:niters - niters is missing
	return(xgrid.run(xgrid.method=xm, jobname=jobname, cleanup=cleanup, keep.files=keep.files, show.output=show.output, arguments=NA, tempdir=FALSE))

}

xapply <- function(X, FUN, xgrid.options=list(), ...){
	
	if(class(X)!='list') X <- as.list(X)
	arguments <- xgrid.options
	arguments$f <- as.function(FUN)
	arguments$niters <- length(X)
	arguments$arguments <- X
	
	arguments <- c(arguments, list(...))
	
	return(do.call(xgrid.run, arguments, quote=FALSE))
	
}
