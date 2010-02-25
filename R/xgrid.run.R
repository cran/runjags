xgrid.run <- function(f=function(iteration){}, niters, object.list=list(), file.list=character(0), threads=min(niters,100), jobname=NA, wait.interval="10 min", xgrid.method=if(threads==1) 'simple' else if(Sys.which('mgrid')=="") 'separatejobs' else 'separatetasks',  Rpath='/usr/bin/R', cleanup=TRUE, submitandstop=FALSE, tempdir=!submitandstop, keep.files=FALSE, show.output=TRUE, max.filesize="1GB", sub.app=if(Sys.which('mgrid')=="") 'xgrid -job submit' else 'mgrid -t $ntasks', sub.options="", sub.command=paste(sub.app, sub.options, '-i $indir $cmd', sep=' '), ...){
		
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
		xgridavail <- system('xgrid 2>&1', intern=TRUE)
		if(length(xgridavail)==1){
			stop('xgrid is not available on this machine')
		}

		xgridlist <- system('xgrid -job list 2>&1', intern=TRUE)
		if(paste(xgridlist, collapse='\n')==paste(xgridavail, collapse='\n')){
			if(.Platform$GUI=='AQUA') stop('Testing basic xgrid functionality produced an error.  System environmental variables set in the .profile file are not accessible from the GUI version of R - either export the XGRID_CONTROLLER_HOSTNAME and password variables using Sys.setenv() or use the console version of R instead') else stop('Testing basic xgrid functionality produced an error.  Ensure that you have either specified the controller hostname and password in the xgrid command or set up envirnomental variables using Sys.setenv()')
		}
	}
	
	if(class(object.list)!='list') stop('The object.list supplied must be a named list')
	if(is.null(names(object.list)) & length(object.list) > 0) stop('The object.list supplied must be a named list')
	
	newobjlist <- list(...)
	objects <- c(object.list, newobjlist)
	if(any(names(objects)=="")) stop('All objects to be passed to the function (either supplied in the object.list or specified in the xgrid.run function call) must be named')
	
	command <- gsub('"', '\"', command)
	command <- gsub('\\"', '\"', command)
	command <- gsub("'", "\'", command)
	command <- gsub("\\'", "\'", command)
	
	if(!xgrid.method=='xgrid.retrieve'){
	
		separatetasks <- xgrid.method=='separatetasks'
		separatejobs <- xgrid.method=='separatejobs'
	
		if(!any(xgrid.method==c('simple', 'separatejobs', 'separatetasks', 'xgrid.retrieve'))) stop(paste('Unrecognised xgrid.method - "', xgrid.method, '"', sep=''))
		if(xgrid.method=='separatetasks' & command=='xgrid -job submit -in $indir $cmd') stop('The basic xgrid command cannot be used with the separatetasks method - either install mgrid (this can be found in the "inst" folder in the runjags package source), specify a command to another script file to generate and submit a plist file, or use the simple or separatejobs method')
	
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
				cat("Directory not writable\n")
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
		
		save(f, iterations, max.filesize, file='Rfunction.Rsave')
				
		if(separatejobs){
			jobnames <- paste(jobname, 1:threads, sep='.thread.')
			for(s in 1:threads){
				cat('#!/bin/sh
pid=$$
( ( (', Rpath, ' --slave --no-save -e "task <- ', s, '; path <- Sys.getenv(\'PATH\'); path <- paste(path, \':/bin:/usr/local/bin:/usr/bin\', sep=\'\'); Sys.setenv(PATH=path); load(\'Robjects.Rsave\'); load(\'Rfunction.Rsave\'); iteration <- iterations[[task]]; for(i in iteration){ temp <- \'Error\'; try(temp <- f(i)); assign(paste(\'iteration.\', i, sep=\'\'), temp)}; save(list=paste(\'iteration.\', iteration, sep=\'\'), file=paste(\'results.\', task, \'.Rsave\', sep=\'\')); filesize <- as.numeric(system(paste(\'ls -l results.\', task, \'.Rsave | awk \\\'{print \\$5}\\\'\', sep=\'\'), intern=TRUE)); if(filesize>max.filesize){ temp <- \'Maximum file size exceeded.  You could either try again using more jobs, reduce the number of objects returned by the function, or save files to a shared drive rather than returning them\'; for(i in iteration) assign(paste(\'iteration.\', i, sep=\'\'), temp); save(list=paste(\'iteration.\', iteration, sep=\'\'), file=paste(\'results.\', task, \'.Rsave\', sep=\'\'))};"; echo $? > .retstat.$pid) 2>&1 1>&3 | tee Rerror.$1.txt) 3>&1 1>&2) 2>&1 | tee Rout.$1.txt

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

echo "" > Rout.', if(separatetasks) '$1.' else '1', '.txt
echo "" > Rerror.', if(separatetasks) '$1.' else '1', '.txt
', if(separatetasks) '( ( echo "Task "$1":" 2>&1 1>&3 | tee -a Rerror.$1.txt) 3>&1 1>&2) 2>&1 | tee -a Rout.$1.txt', '

( ( (', Rpath, ' --slave --no-save -e "task <- ', if(separatetasks) '$1' else '1', '; path <- Sys.getenv(\'PATH\'); path <- paste(path, \':/bin:/usr/local/bin:/usr/bin\', sep=\'\'); Sys.setenv(PATH=path); load(\'Robjects.Rsave\'); load(\'Rfunction.Rsave\'); iteration <- iterations[[task]]; for(i in iteration){ temp <- \'Error\'; try(temp <- f(i)); assign(paste(\'iteration.\', i, sep=\'\'), temp)}; save(list=paste(\'iteration.\', iteration, sep=\'\'), file=paste(\'results.\', task, \'.Rsave\', sep=\'\')); filesize <- as.numeric(system(paste(\'ls -l results.\', task, \'.Rsave | awk \\\'{print \\$5}\\\'\', sep=\'\'), intern=TRUE)); if(filesize>max.filesize){ temp <- \'Maximum file size per task exceeded.  You could either try again using multiple jobs, reduce the number of objects returned by the function, or save files to a shared drive rather than returning them\'; for(i in iteration) assign(paste(\'iteration.\', i, sep=\'\'), temp); save(list=paste(\'iteration.\', iteration, sep=\'\'), file=paste(\'results.\', task, \'.Rsave\', sep=\'\'))};"; echo $? > .retstat.$pid) 2>&1 1>&3 | tee -a Rerror.', if(separatetasks) '$1' else '1', '.txt) 3>&1 1>&2) 2>&1 | tee -a Rout.', if(separatetasks) '$1' else '1', '.txt

', if(separatetasks) '( ( echo "" 2>&1 1>&3 | tee -a Rerror.$1.txt) 3>&1 1>&2) 2>&1 | tee -a Rout.$1.txt', '

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
				success <- system('./starter.sh 2>&1 | tee starteroutput.txt', intern=FALSE)

				if(file.exists('jobid.txt')) tjobnum <- paste(readLines('jobid.txt'), collapse='\n') else tjobnum <- paste(readLines('starteroutput.txt'), collapse='\n')

				jobnum[s] <- gsub('[^[:digit:]]', '', paste(tjobnum, collapse=''))

				xgrid.waiting <- TRUE
				Sys.sleep(2)
				xgrid.waiting <- FALSE
				cat("Job ", s, " of ", threads, " submitted to xgrid\n", sep="")

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

			success <- system('./starter.sh 2>&1 | tee starteroutput.txt', intern=FALSE)

			if(file.exists('jobid.txt')) jobnum <- paste(readLines('jobid.txt'), collapse='\n') else jobnum <- paste(readLines('starteroutput.txt'), collapse='\n')

			jobnum <- gsub('[^[:digit:]]', '', paste(jobnum, collapse=''))

			xgrid.waiting <- TRUE
			Sys.sleep(2)
			xgrid.waiting <- FALSE
			cat("Job submitted to xgrid\n")


		}
		xgrid.method <- 'xgrid.run'
	}
	
	if(submitandstop){
		cat(jobnum, file='jobid.txt', sep='\n') 
		save(list=ls(), file='workingobj.Rsave')
		gottoend <- TRUE
		return(list(jobname=jobname, jobid=jobnum))
	}
		
	xgrid.retrieve(jobnum, wait=(xgrid.method=='xgrid.run'), wait.interval=wait.interval, silent=!show.output, cleanup=cleanup, directory=temp.directory)
	
	gottoend <- TRUE
		
	}, finally={
		
		if(!submitandstop & keep.files){
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
			if(xgrid.waiting) cat("The execution was halted while waiting for the xgrid job to finish - the job has not been deleted\n")
		}
	})
				
	results <- vector('list', length=length(unlist(iterations)))
	names(results) <- paste('iteration.', 1:length(results), sep='')
	for(t in 1:length(iterations)){
		load(paste('results.', t, '.Rsave', sep=''))
		for(i in iterations[[t]]){
			results[[paste('iteration.', i, sep='')]] <- get(paste('iteration.', i, sep=''))
		}
	}
	
	return(results)
	
}

xgrid.submit <- function(f=function(iteration){}, niters, object.list=list(), file.list=character(0), threads=min(niters,100), jobname=NA, xgrid.method=if(threads==1) 'simple' else if(Sys.which('mgrid')=="") 'separatejobs' else 'separatetasks',  Rpath='/usr/bin/R', cleanup=TRUE, keep.files=FALSE, show.output=TRUE, max.filesize='1GB', sub.app=if(Sys.which('mgrid')=="") 'xgrid -job submit' else 'mgrid -t $ntasks', sub.options="", sub.command=paste(sub.app, sub.options, '-i $indir $cmd', sep=' '), ...){

	return(xgrid.run(f=f, niters=niters, object.list=object.list, file.list=file.list, threads=threads, jobname=jobname, sub.command=sub.command, xgrid.method=xgrid.method, Rpath=Rpath, cleanup=cleanup, tempdir=FALSE, keep.files=keep.files, submitandstop=TRUE, show.output=show.output, max.filesize=max.filesize, ...))
}


xgrid.results <- function(jobname){
	
	if(is.list(jobname)) jobname <- jobname$jobname
	return(xgrid.run(xgrid.method='xgrid.retrieve', jobname=jobname))

}
