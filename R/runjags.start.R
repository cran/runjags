# Methods can return either a character string representing an error to be returned, or TRUE if files are to be read immediately or FALSE if JAGS run is ongoing.  Or a list starting with TRUE or FALSE (optionally named complete), and containing other things that will be returned in the runjags object if the JAGS run is ongoing.  If it returns an error the JAGS run is assumed to be not successful.

runjags.simple <- function(jags, silent.jags, jags.refresh, batch.jags, os, libpaths, nsims, jobname, cl, remote.jags, rjags){
	
	retval <- 'An unknown error occured while calling JAGS using the simple method'
	
	stopifnot(nsims==1)
	
	tryCatch({

		swcat("Running the simulation using the simple method... (output will be displayed once the simulation has termianted)\n")
		flush.console()

		if (os == "windows"){
			success <- shell(paste(shQuote(jags), if(!batch.jags) " <", " sim.1/script.cmd > sim.1/jagsoutput.txt 2>&1", sep = ""), intern=FALSE, wait=TRUE)
		}else{
			suppressWarnings(success <- system(paste(shQuote(jags), if(!batch.jags) " <", " sim.1/script.cmd > sim.1/jagsoutput.txt 2>&1", sep=""), intern=TRUE, wait=TRUE))
		}
	
		# In theory more portable code but can't guarantee interleaving of stdout/stderr:
		# output <- system2(jags, args=if(batch.jags) "script.cmd" else character(0), stdout=if(silent.jags) TRUE else "", stderr=if(silent.jags) TRUE else "", stdin=if(batch.jags) "" else "script.cmd")

		if(!silent.jags){
			cat(readLines("sim.1/jagsoutput.txt",warn=FALSE),sep="\n")
		}
	
		retval <- TRUE
		
	})

	return(retval)
}


runjags.snow <- function(jags, silent.jags, jags.refresh, batch.jags, os, libpaths, nsims, jobname, cl, remote.jags, rjags){
		
	retval <- 'An unknown error occured while calling JAGS using the snow method'
	
	if(is.na(remote.jags)) jags <- "*//*usefindjags*//*" else jags <- remote.jags
	
	files <- vector('list', length=nsims+1)
	
	files[[1]]$model.txt <- paste(readLines("model.txt", warn=FALSE), collapse="\n")
	files[[1]]$data.txt <- paste(readLines("data.txt", warn=FALSE), collapse="\n")
	
	thed <- getwd()
		
	for(s in 1:nsims){
		
		setwd(paste("sim",s,sep="."))

		files[[s+1]] <- list()
		file <- list.files()
		for(f in 1:length(file)){
			files[[s+1]][file[f]] <- paste(readLines(file[f], warn=FALSE), collapse="\n")
		}
		
		setwd(thed)
	}
	
	tryCatch({

		f <- function(s, files, jags, batch.jags, silent.jags){
			
			if(!require(runjags)) return(paste("The runjags package was not found on the snow node '", Sys.info()['nodename'], "'", sep=""))

			retval <- "An error occured on the snow cluster"

			tryCatch({
							
				mdfiles <- files[[1]]
				simfiles <- files[[s+1]]
				if(s>1) silent.jags <- TRUE

				if(class(jags)=="function") jags <- jags()					
				if(jags=="*//*usefindjags*//*") jags <- findjags()

				testjags <- testjags(jags, silent=TRUE)
				if(!testjags$JAGS.available && !testjags$JAGS.found){
					return(paste("JAGS was not found on the snow node '", Sys.info()['nodename'], "' at the path '", jags, "'", sep=""))
				}
				jags <- testjags$JAGS.path
				
				cwd <- getwd()
								
				#print("TEMPORARY")
				#unlink(paste("sim",s,sep="."),recursive=TRUE)
				
				# Check to see that the sim files are there (which they will be if we created the cluster inside the function), if not create the folder:
				if(paste("sim",s,sep=".") %in% list.files()){
					cleanup <- FALSE
				}else{
					cleanup <- TRUE
					for(f in 1:length(mdfiles)){
						cat(mdfiles[[f]], file=names(mdfiles)[f])
					}
					dir.create(paste("sim",s,sep="."))
					setwd(paste("sim",s,sep="."))
					for(f in 1:length(simfiles)){
						cat(simfiles[[f]], file=names(simfiles)[f])
					}
				}
				
				setwd(cwd)
				
				os <- .Platform$OS.type
			
				retval <- "An error occured while calling JAGS"
				
				if (os == "windows"){		
					currentsyspath <- Sys.getenv('PATH')
					if(!grepl(libpaths$PATH,currentsyspath,fixed=TRUE)){
						Sys.setenv(PATH=paste(currentsyspath, ';', testjags$libpaths$PATH, sep=''))
					}

					currentsysbinpath <- Sys.getenv('LTDL_LIBRARY_PATH')
					if(!grepl(libpaths$LTDL_LIBRARY_PATH,currentsysbinpath,fixed=TRUE)){
						Sys.setenv(LTDL_LIBRARY_PATH=paste(currentsysbinpath, if(currentsysbinpath!='') ';', testjags$libpaths$LTDL_LIBRARY_PATH, sep=''))
					}		
								
					success <- shell(paste(shQuote(jags), if(!batch.jags) " <", " sim.", s, "/script.cmd > sim.", s, "/jagsoutput.txt 2>&1", sep = ""), intern=FALSE, wait=TRUE)
				}else{
					suppressWarnings(success <- system(paste(shQuote(jags), if(!batch.jags) " <", " sim.", s, "/script.cmd > sim.", s, "/jagsoutput.txt 2>&1", sep=""), intern=TRUE, wait=TRUE))
				}
				
				retval <- "An error occured transferring the model output from the snow cluster"
				codafound <- FALSE
				# Wait for up to 5 secs for the coda files to be created:
				for(i in 1:5){
					Sys.sleep(1)
					if(file.exists("CODAindex.txt")) break
				}
				
				# Grab any new files from inside the simulation folder (there shouldn't be any folders):
				newfilenames <- list.files(paste("sim",s,sep="."))[! list.files(paste("sim",s,sep=".")) %in% names(simfiles)]
				newfiles <- vector('list',length=length(newfilenames))
				names(newfiles) <- newfilenames
				for(f in 1:length(newfiles)) newfiles[f] <- paste(readLines(paste("sim.",s,"/",newfilenames[f],sep="")), collapse="\n")
				
				#print("TEMPORARY")
				#unlink(paste("sim",s,sep="."),recursive=TRUE)
				
				if(cleanup){
					unlink(paste("sim",s,sep="."),recursive=TRUE)
					for(f in 1:length(mdfiles)){
						unlink(names(mdfiles)[f])
					}					
				}
				})
			
			return(list(retval=retval, newfiles=newfiles))
			
		}
		
		if(identical(cl, NA)){
			cl <- makeCluster(nsims)
			on.exit(stopCluster(cl))
		}
		
		clname <- capture.output(print(cl))
		if(nsims==1) swcat("Starting the simulation using a ", clname, "\n") else swcat("Starting the simulations using a ", clname, "\n")		
		
		tryCatch({
			retval <- "There was an error calling the function via snow"
			returned <- FALSE
							
			returns <- parLapply(cl, 1:nsims, f, files=files, jags=jags, batch.jags=batch.jags, silent.jags=silent.jags)
#			returns <- lapply(1:nsims, f, files=files, jags=jags, batch.jags=batch.jags, silent.jags=silent.jags)
			returned <- TRUE
		}, finally={
			if(!returned){
				return(retval)
			}
		})		
		
		charclass <- sapply(returns, function(x) return(length(x)==1 && class(x)=="character"))
		if(any(charclass)){
			cat("The following error(s) occured on one or more snow nodes:",unlist(returns[charclass]),sep="\n","")
		}
			
		retval <- "An error occured whilst writing new files to disk"
		
		success <- try({
			for(s in 1:nsims){
				cwd <- getwd()
				if(!file.exists(paste("sim",s,sep="."))) dir.create(paste("sim",s,sep="."))
				setwd(paste("sim",s,sep="."))
				newfilenames <- names(returns[[s]]$newfiles)[! names(returns[[s]]$newfiles) %in% list.files()]
				# print(paste(length(newfilenames), "new files found for simulation", s))
				if(length(newfilenames)>0){
					for(f in 1:length(newfilenames)){
						cat(unlist(returns[[s]]$newfiles[newfilenames[f]]), file=newfilenames[f])
						# Close to make sure it's written out:
						close(file(newfilenames[f]))
					}
				}
				setwd(cwd)
			}
			})
		
		if(class(success)!="try-error"){
			retval <- TRUE
		}
		
	})
	
	return(retval)
	
}




runjags.background <- function(jags, silent.jags, jags.refresh, batch.jags, os, libpaths, nsims, jobname, cl, remote.jags, rjags){
	
	if(nsims==1) retval <- 'An unknown error occured while calling JAGS using the background method' else retval <- 'An unknown error occured while calling JAGS using the parallel background method'
	
	thed <- getwd()
	
	tryCatch({

		if(nsims==1) swcat("Starting the simulation in the background...\n") else swcat("Starting the simulations in the background...\n")
			
		success <- numeric(nsims)
		for(s in 1:nsims){
			if (os == "windows"){
					success[s] <- shell(paste(shQuote(jags), if(!batch.jags) " <", " sim.", s, "/script.cmd > sim.", s, "/jagsoutput.txt", if(silent.jags) " 2>&1", sep = ""), intern=FALSE, wait=FALSE)
			}else{
				if(silent.jags){
					success[s] <- system(paste(shQuote(jags), if(!batch.jags) " <", " sim.", s, "/script.cmd > sim.", s, "/jagsoutput.txt 2>&1", sep=""), intern=FALSE, wait=FALSE)
				}else{
					success[s] <- system(paste(shQuote(jags), if(!batch.jags) " <", " sim.", s, "/script.cmd > sim.", s, "/jagsoutput.txt", sep=""), intern=FALSE, wait=FALSE)					
				}
			}
		}
		
		Sys.sleep(1)
	
		retval <- FALSE
		
		if(nsims==1) swcat("The JAGS process is now running in the background\n") else swcat("The JAGS processes are now running in the background\n")
	})
	
	return(retval)
}


runjags.interruptible <- function(jags, silent.jags, jags.refresh, batch.jags, os, libpaths, nsims, jobname, cl, remote.jags, rjags){
	
	swcat("Calling the simulation...\n")

	retval <- 'An unknown error occured while calling JAGS using the interruptible method'
	
	stopifnot(nsims==1)
	
	#filecon <- file('scriptlauncher.sh', 'w')
	cat('#!/bin/sh
	i=$1
	', shQuote(jags), if(!batch.jags) ' <', ' sim.$i/script.cmd > sim.$i/jagsoutput.txt 2>&1 &

	echo $! > sim.$i/jagspid.txt
	', sep='', file='scriptlauncher.sh')
	#close(filecon)

	Sys.chmod('scriptlauncher.sh')

	thed <- getwd()
	
	tryCatch({

		
		if(os == "windows"){
		
			tasks <- system('TASKLIST', intern=TRUE)

			allpid=as.numeric(unlist(lapply(tasks[grepl("jags-terminal", tasks) | grepl("JAGS-T~1.EXE", tasks) ], function(x){
				chars <- strsplit(x, split=" ")[[1]]
				return(chars[chars!=""][2])
				})))

			output <- shell(paste(shQuote(jags), if(!batch.jags) " <", " sim.1/script.cmd > sim.1/jagsoutput.txt 2>&1", sep = ""), intern=FALSE, wait=FALSE)

			tasks <- system('TASKLIST', intern=TRUE)
			newpid=as.numeric(unlist(lapply(tasks[grepl("jags-terminal", tasks) | grepl("JAGS-T~1.EXE", tasks) ], function(x){
				chars <- strsplit(x, split=" ")[[1]]
				return(chars[chars!=""][2])
				})))

			pid <- newpid[! newpid %in% allpid]
			
		}else{
			
			success <- system('./scriptlauncher.sh 1', wait=TRUE, intern=FALSE)	

			# Allow simulation to start before we look for the PID:
			Sys.sleep(0.1)
			if(!file.exists('sim.1/jagspid.txt')) Sys.sleep(1)
			if(!file.exists('sim.1/jagspid.txt')) Sys.sleep(1)
			
			suppressWarnings(output <- readLines('sim.1/jagspid.txt'))
			pid <- output[1]
			if(as.numeric(pid)!=as.integer(pid) | pid=="") stop("A problem occured when reading the output of a started process")
	
		}

		output <- tailf('sim.1/jagsoutput.txt', refresh=jags.refresh, start=1, min.static=2, stop.text='Deleting model', print=!silent.jags, return=TRUE)
	
		if(output$interrupt){
			
			retval  <- "The JAGS process was terminated by the user"
			
			if(os == 'windows'){
				# Make sure length(pid) == 1
				if(length(pid)!=1){
					warning("Unable to identify correct JAGS process to terminate; no processes have been killed (the JAGS model will continue until it has finished or you restart your computer)", call.=FALSE)
				}else{
					suppressWarnings(killout <- system(paste('taskkill /F /PID ', pid, sep=''), intern=TRUE))
				}

			}else{
				system(paste('kill ', pid, sep=''), ignore.stderr=TRUE)
			}
			
		}else{
			retval <- TRUE	
		}
	})
	
	cat("\n")
	setwd(thed)
	return(retval)	

}

runjags.parallel <- function(jags, silent.jags, jags.refresh, batch.jags, os, libpaths, nsims, jobname, cl, remote.jags, rjags){
	
	swcat("Calling the simulation using the parallel method...\n")

	retval <- 'An unknown error occured while calling JAGS using the parallel method'
	
	#filecon <- file('scriptlauncher.sh', 'w')
	cat('#!/bin/sh
	i=$1
	', shQuote(jags), if(!batch.jags) ' <', ' sim.$i/script.cmd > sim.$i/jagsoutput.txt 2>&1 &

	echo $! > sim.$i/jagspid.txt
	', sep='', file='scriptlauncher.sh')
	#close(filecon)

	Sys.chmod('scriptlauncher.sh')

	thed <- getwd()
	
	
	tryCatch({

		if(os == "windows"){
		
			tasks <- system('TASKLIST', intern=TRUE)

			allpid=as.numeric(unlist(lapply(tasks[grepl("jags-terminal", tasks) | grepl("JAGS-T~1.EXE", tasks) ], function(x){
				chars <- strsplit(x, split=" ")[[1]]
				return(chars[chars!=""][2])
			})))

			for(s in 1:nsims){
 				output <- shell(paste(shQuote(jags), if(!batch.jags) " <", " sim.", s, "/script.cmd > sim.", s, "/jagsoutput.txt 2>&1", sep = ""), intern=FALSE, wait=FALSE)
				# Allow simulation to start before we move onto the next one:
				Sys.sleep(0.1)
			}
						
			# Allow first simulation to start before we look for the PID:
			tries <- 0
			repeat{
				if(file.exists('sim.1/jagsoutput.txt')) break
				Sys.sleep(0.5)
				tries <- tries +1
				if(tries==6) stop("Timed out waiting for output to appear from JAGS")
			}
			
			tasks <- system('TASKLIST', intern=TRUE)

			newpid=as.numeric(unlist(lapply(tasks[grepl("jags-terminal", tasks) | grepl("JAGS-T~1.EXE", tasks) ], function(x){
				chars <- strsplit(x, split=" ")[[1]]
				return(chars[chars!=""][2])
			})))

			pid <- newpid[! newpid %in% allpid]
		
		}else{
			pid <- character(nsims)
			# Start with last simulation first, so the first simulation will be the last to finish most of the time:
			for(s in nsims:1){
				success <- suppressWarnings(system(paste('./scriptlauncher.sh ', s, sep=''), wait=TRUE, intern=FALSE))
				# Allow simulation to start before we look for the PID:
				tries <- 0
				repeat{
					if(file.exists(paste('sim.', s, '/jagspid.txt', sep=''))) break
					Sys.sleep(0.5)
					tries <- tries +1
					if(tries==6) stop("Timed out waiting for output to appear from JAGS")
				}

				suppressWarnings(output <- readLines(paste('sim.', s, '/jagspid.txt', sep='')))
				pid[s] <- output[1]
				if(as.numeric(pid[s])!=as.integer(pid[s]) | pid[s]=="") stop("A problem occured when reading the output of a started process")		
			}
		}

		# Loop through and follow chains sequentially until they've all finished:
		s <- 1
		repeat{
			if(!silent.jags) cat("Following the progress of chain ", s, " (the program will wait for all chains to finish before continuing):\n", sep="")			
			output <- tailf(paste('sim.', s, '/jagsoutput.txt', sep=''), refresh=jags.refresh, start=1, min.static=2, stop.text="Deleting model", print=!silent.jags, return=TRUE)
			cat("\n")
			if(output$interrupt) break

			# The first sim should be finished last ... but give the others up to 5 seconds to catch up before switching to them...
			tries <- 0
			repeat{
				# Check to see which chains have (a) started, and (b) finished:
				simsdone <- sapply(1:nsims, function(x) return(file.exists(paste('sim.', x, '/jagsoutput.txt', sep='')) && grepl("Deleting model",paste(readLines(paste('sim.', x, '/jagsoutput.txt', sep=''),warn=FALSE),collapse="\n"))))
				
				if(all(simsdone)) break
				tries <- tries +1
				if(tries==5) break
				Sys.sleep(1)
			}

			if(all(simsdone)){
				cat("All chains have finished\n")
				break
			}

			# If one still hasn't finished, start watching it:
			s <- which(!simsdone)[1]
		}					
				
		if(output$interrupt){

			retval  <- "The JAGS process was terminated by the user"
			if(os == 'windows'){
				# Make sure length(pid) == nsims
				if(length(pid)!=nsims){
					warning("Unable to identify correct JAGS processes to terminate; no processes have been killed (the JAGS model will continue until it has finished or you restart your computer)", call.=FALSE)
				}else{
					for(k in pid){
						suppressWarnings(killout <- system(paste('taskkill /F /PID ', k, sep=''), intern=TRUE))
					}
				}
				
			}else{
				for(s in 1:nsims){
					system(paste('kill ', pid[s], sep=''), ignore.stderr=TRUE)
				}
			}			

		}else{
			
			retval <- TRUE
				
		}
	
	})
		
	return(retval)

}


runjags.rjags <- function(jags, silent.jags, jags.refresh, batch.jags, os, libpaths, nsims, jobname, cl, remote.jags, rjags){
	
	# rjags object should have been compiled by extend.jags or autoextend.jags if this was necessary - and the modules loaded just before that
	extra.options <- rjags[names(rjags)!='rjags']
	rjags <- rjags$rjags

	for(i in 1:length(extra.options$factories)){
		if(extra.options$factories[i]!=""){
			f <- strsplit(gsub(")","",extra.options$factories[i],fixed=TRUE),"(",fixed=TRUE)[[1]]					
			fa <- ""
			try(fa <- as.character(list.factories(f[2])$factory))
			if(!f[1] %in% fa) stop(paste("The factory '", f[1], "' of type '", f[2], "' is not available - ensure any required modules are also provided", sep=""))
			success <- try(set.factory(f[1],f[2],TRUE))			
			if(class(success)=="try-error") stop(paste("Failed to load the factory '", f[1], "' of type '", f[2], "'", sep=""))
		}
	}
		
	if(silent.jags) extra.options$progress.bar <- "none"
	
	if(!silent.jags) swcat("Calling the simulation using the rjags method...\n")

	if(extra.options$adapt>0){
		if(!silent.jags) cat("  Adapting the model for ", format(extra.options$adapt,scientific=FALSE), " iterations...\n",sep="")
		flush.console()
		by <- if(is.na(extra.options$by)) min(100, extra.options$adapt/50) else extra.options$by
		finishedadapting <- adapt(rjags,n.iter=extra.options$adapt,progress.bar=extra.options$progress.bar,by=by,end.adaptation=TRUE)
		flush.console()
	}
	if(extra.options$burnin>0){
		if(!silent.jags) cat("  Burning in the model for ", format(extra.options$burnin,scientific=FALSE), " iterations...\n",sep="")
		flush.console()
		by <- if(is.na(extra.options$by)) min(100, extra.options$burnin/50) else extra.options$by
		update(rjags,n.iter=extra.options$burnin,progress.bar=extra.options$progress.bar)
		flush.console()
	}

	# There should never be a popt or pd.i monitor here but remove just in case:
	monitor <- extra.options$monitor[! extra.options$monitor %in% c("popt","pd.i","dic")]
	monitor[monitor=="pd"] <- "pD"
	by <- if(is.na(extra.options$by)) min(100, extra.options$burnin/50) else extra.options$by
	if(!silent.jags) cat("  Running the model for ", format(extra.options$sample,scientific=FALSE), " iterations...\n",sep="")
	flush.console()
	samples <- jags.samples(rjags,variable.names=monitor,n.iter=extra.options$sample,progress.bar=extra.options$progress.bar, thin=extra.options$thin)
	# This is just a dummy call so that we can get the names of the variables:
	suppressWarnings(varnames <- varnames(coda.samples(rjags,variable.names=monitor[monitor!="pD"],n.iter=1,progress.bar="none", thin=1)))
	flush.console()
	
	mcmcout <- lapply(samples[names(samples)!='pD'], as.mcmc.list)

	nvar <- length(varnames)
	niter <- sapply(mcmcout,niter)
	if(!all(niter==niter[1])) stop("An error occured with the rjags method - variables returned with differing numbers of iterations")
	niter <- niter[1]
	
	mcmc <- vector('list',length=extra.options$n.chains)
	
	for(i in 1:extra.options$n.chains){
		
		mcmc[[i]] <- mcmc(do.call('cbind',lapply(mcmcout, function(x) return(x[[i]]))), start=extra.options$burnin+1, thin=extra.options$thin)		
		dimnames(mcmc[[i]]) <- list(1:niter, varnames)
	}
	
	mcmc <- as.mcmc.list(mcmc)
	if(any(names(samples)=="pD")){
		pd <- mcmc(matrix(unlist(samples[names(samples)=='pD']),ncol=1,dimnames=list(NULL,"pD")), start=extra.options$burnin+1, thin=extra.options$thin)
		dimnames(pd) <- list(1:niter(pd), dimnames(pd)[[2]])
	}else{
		pd <- NA
	}
	
	end.state <- sapply(rjags$state(internal=TRUE),dump.format)
	
	if(!silent.jags) swcat("Simulation complete\n")
	
	return(list(complete=TRUE, mcmc=mcmc, pd=pd, popt=NA, pd.i=NA, end.state=end.state))

}

runjags.xgrid <- function(jags, silent.jags, jags.refresh, batch.jags, os, libpaths, nsims, jobname, cl, remote.jags, command, customart, jagspath, submitandstop, max.threads, mgridpath, hostname, password){
	
	swcat("Submitting the simulation to Xgrid... (this may take some time)\n")
		
	retval <- 'An unknown error occured while calling JAGS using the xgrid method'
	
	# max.threads is only passed through for consistency, and should be redundant by nsims:
	stopifnot(nsims<=max.threads)
			
	#filecon <- file('scriptlauncher.sh', 'w')
	cat('#!/bin/sh
	i=$1
	', shQuote(jags), if(!batch.jags) ' <', ' sim.$i/script.cmd > sim.$i/jagsoutput.txt 2>&1 &

	echo $! > sim.$i/jagspid.txt
	', sep='', file='scriptlauncher.sh')
	#close(filecon)

	Sys.chmod('scriptlauncher.sh')
	
	thed <- getwd()
	
	tryCatch({


		cat(customart, file="customart.sh")

	# cd doesn't work on some nodes, so the working directory is left where it is now and paths adjusted accordingly in the script
	#	', if(method.options$separate.tasks) 'cd sim.$1', '
		cat('#!/bin/sh


		pid=$$

		', if(!silent.jags) 'echo "" > sim.$1/jagsout.txt', ' 
		', if(!silent.jags) 'echo "" > sim.$1/jagsout.txt', ' 
		', if(nsims>1) '( ( echo "Chain "$1":" 2>&1 1>&3 | tee -a sim.$1/jagserror.txt) 3>&1 1>&2) 2>&1 | tee -a sim.$1/jagsout.txt', if(nsims>1 && silent.jags) '', '', '

		( ( (', jagspath, ' < sim.$1/script.cmd; echo $! > sim.$1/.retstat.$pid) 2>&1 1>&3 | tee -a sim.$1/jagserror.txt) 3>&1 1>&2) 2>&1 | tee -a sim.$1/jagsout.txt', if(silent.jags) '', '

		', if(nsims>1) '( ( echo "" 2>&1 1>&3 | tee -a sim.$1/jagserror.txt) 3>&1 1>&2) 2>&1 | tee -a sim.$1/jagsout.txt', if(nsims>1 & silent.jags) '', '', '

		# This makes sure the process has finished before continuing:
		wait
	
		returnstat=`cat < sim.$1/.retstat.$$`
		rm sim.$1/.retstat.$$
	
		', if(!silent.jags) 'echo ""', '
	
		exit $returnstat
			', sep='', file=jobname)
			#close(filecon)
	
		Sys.chmod(jobname)

		
		cat('#!/bin/sh		
	cmd="', jobname, '" 
	ntasks=', nsims, '
	job=1
	indir="', thed, '"
	# Start the process in the background so we can get the pid:
	', command, ' &
	# Echo the pid to a file - we might need to kill it if interrupted:
	echo $! > mgridpid.txt
	# And wait for mgrid to finish:
	wait
	rm mgridpid.txt
	exit 0
		', sep='', file='starter.sh')
	#}

		Sys.chmod('starter.sh')
		
		# For some reason wrapping the actual system call inside a shell script fixes the display issues in Aqua, so no need to use tailf workaroud here:
		success <- system('( ./starter.sh 2>&3 | tee .starterout.txt) 3>&2 | tee starteroutput.txt', intern=FALSE, wait=TRUE)

		# This should be remvoed by the shell script, if it's still there the shell script didn't finish:
		if(file.exists("mgridpid.txt")){
			interrupt <- TRUE
			pid <- as.numeric(readLines('mgridpid.txt'))
			success <- system(paste("kill ", pid, sep=""))
			return("The process was terminated while submitting or waiting for the results of the xgrid job")
		}else{
				
			if(success!=0){
				return("An unknown error occured while starting the xgrid command")
			}
	
			if(file.exists('jobid.txt')) joboutput <- paste(readLines('jobid.txt'), collapse='\n') else joboutput <- paste(readLines('starteroutput.txt'), collapse='\n')
				
			results <- list()
			
			if(submitandstop){
				jobnum <- gsub('[^[:digit:]]', '', paste(joboutput, collapse=''))
				if(jobnum=='' | as.numeric(jobnum)>10^6){
			#				cat(readLines("starteroutput.txt"))
					return("There was an error submitting your job to Xgrid - no Job ID was returned")			
				}
				swcat('Your job (name: "', jobname, '";  ID: "', jobnum, '") has been succesfully uploaded to xgrid\n', sep='')
				results$complete <- FALSE
				results$jobid <- jobnum
			}else{
				results$complete <- TRUE
				results$jobid <- NA
			}
			
			retval <- results
	
		}
	
	})
	
	return(retval)
	
}

runjags.start <- function(model, monitor, data, inits, modules, factories, burnin, sample, adapt, thin, tempdir, dirname, method, method.options, internal.options){
	
	# Reset failedjags stuff:
	failedjags$model <- "No failed model available!"
	failedjags$data <- "No failed data available!"
	failedjags$inits <- "No failed initial values available!"
	failedjags$output <- "No failed model output available!"
	failedjags$end.state <- "No failed model parameter state available!"
	
	chains=n.chains <- length(inits)		

	updates <- sample
	if(sample<1) stop("The specified value for sample must be a positive integer")

	# Method for calling JAGS is now encapsulated within method=function() and method.options=list()
	# jags, silent.jags, tempdir, method, jags.refresh, batch.jags){

	# Some methods don't require writing files:
	writefiles <- TRUE
	
	# Checking of DIC and JAGS availability etc will already have been done, but we need to check vailability of methods (and xgrid options)
	if(class(method)!="function"){
		
		method <- getrunjagsmethod(method)
		
		cl = inputsims = remote.jags = rjags = by = progress.bar <- NA
		if(method=="xgrid"){			
			xgrid.options <- method.options
			method.options <- internal.options
		}else{
			if(!is.null(method.options$nsims) && is.numeric(method.options$nsims)) inputsims <- method.options$nsims
			if(!is.null(method.options$cl) && any(grepl('cluster',class(method.options$cl)))) cl <- method.options$cl 
			if(!is.null(method.options$remote.jags)) remote.jags <- method.options$remote.jags 
			if(!is.null(method.options$rjags)) rjags <- method.options$rjags 
			if(!is.null(method.options$by)) by <- method.options$by 
			if(!is.null(method.options$progress.bar)){
				progress.bar <- method.options$progress.bar
			}else{
				progress.bar <- unlist(options('jags.pb'))
			}
								
			if(!identical(method.options, list()) && !identical(names(method.options), c('jobname', 'command', 'customart', 'jagspath', 'submitandstop', 'max.threads', 'mgridpath', 'hostname', 'password')) && !all(names(method.options)%in%c("nsims","cl","remote.jags","rjags","by","progress.bar"))) warning("The supplied value for method.options is ignored when using an inbuilt method (except for 'nsims' which can be provided for parallel methods, 'cl' and 'remote.jags' which can be provided for the snow method, and 'by' and 'progress.bar' which can be supplied for the rjags method)")
			method.options <- internal.options
		}
		
		# Getting a strange no visible binding error from R CMD check - can't track it down but this should cure it:
		jags <- method.options$jags
		
		if((method=='parallel' | method=='interruptible') &.Platform$OS.type!='windows' ) if(length(suppressWarnings(system('/bin/ps', intern=TRUE, ignore.stderr=TRUE)))==0 & Sys.info()['user']=='nobody'){
			warning('The interruptible and parallel methods are unavailable when running over xgrid, using the simple method instead')
			method <- 'simple'
		}
		if((method=='parallel' | method=='interruptible') &.Platform$OS.type=='windows'){
			if(Sys.which('TASKLIST')[1]==""){
				warning("Parallel and interruptible methods aren't available on your machine because the TASKLIST system command couldn't be found; switching to the simple method", call.=FALSE)
				method <- 'simple'
			}		
			if(Sys.which('TASKKILL')[1]==""){
				warning("Parallel and interruptible methods aren't available on your machine because the TASKKILL system command couldn't be found; switching to the simple method", call.=FALSE)
				method <- 'simple'
			}
			ret <- system2('TASKLIST',stdout=FALSE,stderr=FALSE)
			if(ret!=0){
				warning("Parallel and interruptible methods aren't available on your machine because testing the TASKLIST system command produced an error; switching to the simple method", call.=FALSE)
				method <- 'simple'			
			}
			ret <- system2('TASKKILL','/?',stdout=FALSE,stderr=FALSE)
			if(ret!=0){
				warning("Parallel and interruptible methods aren't available on your machine because testing the TASKKILL system command produced an error; switching to the simple method", call.=FALSE)
				method <- 'simple'			
			}
		}		
	
		jags.status <- testjags(method.options$jags, silent=TRUE)
		if(jags.status$JAGS.available==FALSE){
			if(jags.status$os=="windows"){
				# Try it again - sometimes this seems to clear it up:
				Sys.sleep(0.2)
				jags.status <- testjags(method.options$jags, silent=TRUE)
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
	
		if(jags.status$JAGS.version=="unknown" | is.na(jags.status$JAGS.version)){
			warning('Unable to verify the version number of JAGS.  If any functions do not work as expected, you could try checking your JAGS installation for problems.')
			jags.status$JAGS.version <- Inf
		}
	
		if(length(grep('base::Mersenne-Twister', inits)>0) & as.numeric(jags.status$JAGS.version) < 2) warning('Using the RNG "base::Mersenne-Twister" (used by default for chain 4) may cause problems with restarting subsequent simulations using the end state of previous simulations due to a bug in JAGS version 1.x.  If you encounter the error "Invalid .RNG.state", please update JAGS to version 2.x and try again.  Or, you can change the random number generator by changing the .RNG.name to (for example) "base::Super-Duper" and remove the .RNG.state element of the list.')
		
		if(tempdir){
			if(is.na(dirname)) dirname <- "runjagsfiles"
			temp.directory <- tempfile(dirname)
			jobname <-  if(method=='xgrid') paste("xgrid.",dirname,sep="") else temp.directory
			dir.create(temp.directory)
		}else{
			# Change jobname to match folder name:
			if(method=='xgrid'){
				jobname <- new_unique(xgrid.options$jobname, touch=TRUE, type='folder')
			}else{
				if(is.na(dirname)){
					dirname <- "runjagsfiles"
					jobname <- new_unique(dirname, touch=TRUE, type='folder')
				}else{
					jobname <- new_unique(dirname, touch=TRUE, type='folder')
					if(jobname!=dirname) warning(paste("The specified folder name was already taken - saving to '", jobname, "' instead", sep=""))
				}
			}
			if(jobname=="Directory not writable"){
				stop("Directory not writable", call.=FALSE)
			}
			temp.directory <- file.path(getwd(), jobname)
		}

		strmethod <- method
		if(strmethod=="simple"){
			nsims <- 1
			method <- runjags.simple
		}
		if(strmethod=="interruptible"){
			nsims <- 1
			method <- runjags.interruptible
		}
		if(strmethod=="parallel"){
			ncores <- suppressWarnings(detectCores())
			if(is.na(ncores)){
				ncores <- 2
				warning("Unable to detect the available number of cores on your machine - using a maximum of 2 cores as a default")
			}
			nsims <- min(n.chains, ncores)
			if(!is.na(inputsims)){
				if(inputsims!=as.integer(inputsims) || inputsims <1) warning("The supplied nsims option is not a positive integer and was ignored") else nsims <- min(n.chains, inputsims)
			}
			method <- runjags.parallel
		}
		if(strmethod=="snow"){
			ncores <- suppressWarnings(detectCores())
			if(is.na(ncores)){
				ncores <- 2
				warning("Unable to detect the available number of cores on your machine - using a maximum of 2 cores as a default")
			}
			nsims <- min(n.chains, ncores)
			if(!is.na(inputsims)){
				if(inputsims!=as.integer(inputsims) || inputsims <1) warning("The supplied nsims option is not a positive integer and was ignored") else nsims <- min(n.chains, inputsims)
			}
			if(any(grepl('cluster',class(cl)))){
				if(!is.na(inputsims)) warning("Supplied value for nsims has been ignored, and the number of available cores on the supplied cluster used instead")
				nsims <- min(n.chains, length(cl))
			}
			method <- runjags.snow
			if(!is.na(remote.jags)){
				rjt <- remote.jags
				if(class(rjt)=="function"){
					s <- try(rjt <- rjt())
					if(class(s)=="try-error") stop("The function supplied to remote.jags must take no arguments")
				}
				if(!is.character(rjt) && length(rjt)!=1) stop("The argument supplied for remote.jags must be a character string specifying the path to JAGS on the snow nodes, a function (with no arguments) returning a character string specifying the path to JAGS, or NA in which case JAGS will be found using findjags()")
			}			
		}
		if(strmethod=="background"){
			nsims <- 1
			method <- runjags.background
		}
		if(strmethod=="bgparallel"){
			ncores <- suppressWarnings(detectCores())
			if(is.na(ncores)){
				ncores <- 2
				warning("Unable to detect the available number of cores on your machine - using a maximum of 2 cores as a default")
			}
			nsims <- min(n.chains, ncores)
			if(!is.na(inputsims)){
				if(inputsims!=as.integer(inputsims) || inputsims <1) warning("The supplied nsims option is not a positive integer and was ignored") else nsims <- min(n.chains, inputsims)
			}
			method <- runjags.background
		}
		if(strmethod=='xgrid'){
			# Leave here as a failsafe in case extend.jags is used on an xgrid method object:
			test <- setup.xgrid(mgridpath=xgrid.options$mgridpath, hostname=xgrid.options$hostname, password=xgrid.options$password, testonly=TRUE)
			nsims <- min(n.chains, xgrid.options$max.threads)
			method <- runjags.xgrid
		}
		extramethodargs <- list()
		if(strmethod=='rjags'){
			nsims <- 1
			# nsims can never be more than 1 for rjags in case we don't specify parallel methods!!!
			method <- runjags.rjags
			extramethodargs <- list(monitor=monitor, modules=modules, factories=factories, adapt=adapt, burnin=burnin, sample=sample, n.chains=n.chains, thin=thin, by=by, progress.bar=progress.bar)
			writefiles <- FALSE
		}
		
		method.options <- c(method.options, list(os=jags.status$os, libpaths=jags.status$libpaths, nsims=nsims, jobname=jobname, cl=cl, remote.jags=remote.jags, rjags=c(list(rjags=rjags),extramethodargs)))
		method.options$jags <- jags.status$JAGS.path
		
		if(strmethod=='xgrid'){
			method.options <- c(method.options, xgrid.options[names(xgrid.options)!="jobname"])
			if(!identical(names(method.options), c("jags", "silent.jags", "jags.refresh", "batch.jags", "os", "libpaths", "nsims", "jobname", "cl", "remote.jags", "rjags", "command", "customart", "jagspath", "submitandstop", "max.threads", "mgridpath", "hostname", "password"))) stop("Invalid method.options list provided - ensure xgrid jags jobs are started with the correct xgrid.xx.jags functions", call.=FALSE)
		}else{
			stopifnot(identical(names(method.options), c("jags", "silent.jags", "jags.refresh", "batch.jags", "os", "libpaths", "nsims", "jobname", "cl", "remote.jags", "rjags")))	
		}
		
		stopifnot(is.function(method))
		
	}else{
		
		if(!class(method.options)=="list" || any(names(method.options)=="")) stop("The method.options argument must be provided as a named list")
		
		if(!any(names(method.options)=="nsims")){
			warning("No 'nsims' value provided in the method.options list; assuming a single simulation directory is required")
			method.options$nsims <- 1
		}else{
			if(method.options$nsims!=as.integer(method.options$nsims) || method.options$nsims <1){
				warning("The supplied nsims option is not a positive integer and was ignored; assuming a single simulation directory is required")
				method.options$nsims <- 1
			}else{
				method.options$nsims <- min(n.chains, method.options$nsims)
			}
		}
		
		if(tempdir){
			temp.directory <- tempfile('runjagsdir')
			jobname <-  temp.directory
			dir.create(temp.directory)
		}else{
			# Change jobname to match folder name:
			jobname <- new_unique('runjagsfiles', touch=TRUE, type='folder')
			if(jobname=="Directory not writable"){
				stop("Directory not writable", call.=FALSE)
			}
			temp.directory <- file.path(getwd(), jobname)
		}
		if(any(names(method.options)=="write.files")) write.files <- method.options$write.files
		
	}	
	
	if(method.options$nsims != as.integer(method.options$nsims)) stop("The number of simulations required must be an integer")
	nsims <- method.options$nsims
	
	# Check that the function matches the function arguments:
	if(any(is.na(pmatch(names(method.options), names(formals(method)))))) warning("One or more method.options supplied does not match an argument to the supplied method function")

	method.options <- method.options[!is.na(pmatch(names(method.options), names(formals(method))))]
		
	
	if(any(c("pd","pd.i","popt") %in% monitor)){
		if(n.chains < 2) stop("The DIC, pD, pD.i and popt cannot be assessed with only 1 chain")
		if(nsims > 1) stop("The DIC, pD, pD.i and popt cannot be assessed when using parallel or separate chains")
	}

	sim.chains <- matrix(NA, ncol=ceiling(n.chains/nsims), nrow=nsims)
	sim.chains[1:n.chains] <- 1:n.chains

	sim.chains <- lapply(1:nsims, function(x) return(sim.chains[x,!is.na(sim.chains[x,])]))
	nsim.chains <- sapply(sim.chains, length)

	stopifnot(sum(nsim.chains)==n.chains)
	
	real.runs <- as.integer(updates)
	ini.runs <- as.integer(burnin)
	adapt.runs <- as.integer(adapt)
	
	realmonitor <- monitor[! (monitor %in% c("dic","popt","pd","pd.i")) ]
	# We have already checked for monitors but that included dic/popt/pd/pd.i - but we can't have zero real monitors (not including popt/pd.i/pd):
	if(length(realmonitor)==0) stop("Cannot run a model with only popt, pd or pd.i monitored - add a named variable to the monitors", call.=FALSE)
	monitorcollapse <- paste(", thin(", thin, ")\nmonitor ", sep="")
	monitors <- paste("monitor ", paste(realmonitor, collapse=monitorcollapse), ", thin(", thin, ")\n", sep="")

	n.params <- length(monitor)
	params.names <- monitor
	
	total.updates <- n.params * updates
	
	results <- list()
	
	save.directory <- getwd()


	on.exit(setwd(save.directory))

	# None of this will ever be needed for rjags methods:
	if(nsims!=1){

		norng <- sum(grepl('.RNG.name', inits, fixed=TRUE))

		if(norng!=n.chains){
			if(norng!=0) stop('Attempting to use parallel chains with some (but not all) .RNG.name values specified - make sure you specify a .RNG.name for each chain and try again')
			warning('You attempted to start parallel chains without setting different PRNG for each chain, which is not recommended.  Different .RNG.name values have been added to each set of initial values.', call.=FALSE)
		
			# Different behaviours for nchains <= 4 and > 5:

			if(n.chains <= 4){
				rngname <- paste('\".RNG.name\" <- \"', rep(c('base::Wichmann-Hill', 'base::Marsaglia-Multicarry', 'base::Super-Duper', 'base::Mersenne-Twister'), ceiling(n.chains/4)), '\"\n',sep='')
			}else{
				rjagsloaded <- 'rjags' %in% .packages()
				if(!require(rjags)) stop("The rjags package is required to generate initial values for more than 4 parallel chains")
				success <- try(load.module("lecuyer"))
				if(class(success)=="try-error") stop("Failed to load the lecuyer module - ensure that the latest version of JAGS and rjags is installed")
				
				rngname <- sapply(parallel.seeds("lecuyer::RngStream", n.chains), dump.format)		
				modules <- c(modules, "lecuyer")
				modules <- unique(modules)
				if(!rjagsloaded) unloadNamespace('rjags')		
			}
					
			for(i in 1:n.chains){
				if(is.na(inits[i])) inits[i] <- rngname[i] else inits[i] <- paste(inits[i], '\n', rngname[i], sep='')
			}		

		}
		
	}
	
	initstring <- paste(inits, "\n", sep="")
	
	resetbinpath=resetsyspath <- FALSE
	if(writefiles){
		
		setwd(temp.directory)

		cat(model, file="model.txt",sep="")
		close(file("model.txt"))
		cat(data, file="data.txt",sep="")  
		close(file("data.txt"))
		
		save(sim.chains, file="simchainsinfo.Rsave")
		close(file("simchainsinfo.Rsave"))
			
		for(s in 1:nsims){

			sim <- paste("sim", s, sep=".")
			#system(paste('mkdir ', sim, sep=''))
			dir.create(sim)

			scriptstring <- ""

			if(!all(is.character(modules))) stop("The vector provided for 'modules' must be a character vector naming one or more modules to be loaded in JAGS")
			if(!all(is.character(factories)) || !all(factories=="" | (grepl("(",factories,fixed=TRUE) & grepl(")",factories,fixed=TRUE)))) stop("The vector provided for 'factories' must be a character vector naming one or more factories to be loaded in JAGS with the following format:  <facname>(<factype>)")
			
			if(any(modules!="")){
				for(i in 1:length(modules)){
					scriptstring <- paste(scriptstring, "load ", modules[i], "\n", sep="")
				}
			}
			
			if(any(factories!="")){
				for(i in 1:length(factories)){
					f <- strsplit(gsub(")","",factories[i],fixed=TRUE),"(",fixed=TRUE)[[1]]					
					scriptstring <- paste(scriptstring, "set factory \"", f[1], "\" on, type(", f[2], ")\n", sep="")
				}
			}

			scriptstring <- paste(scriptstring, "model in \"model.txt\"\n", sep="")
			if(data!=""){
				scriptstring <- paste(scriptstring, "data in \"data.txt\"\n", sep="")
			}

			scriptstring <- paste(scriptstring, "compile, nchains(", as.integer(nsim.chains[s]), ")\n", sep="")
			for(c in 1:nsim.chains[s]){
				i <- sim.chains[[s]][c]
				if(!is.na(inits[i]) && inits[i]!="") scriptstring <- paste(scriptstring, "parameters in \"inits", i, ".txt\", chain(", c, ")\n", sep="")
			}

			scriptstring <- paste(scriptstring, "initialize\n", sep="")
			if(adapt.runs > 0){
				scriptstring <- paste(scriptstring, "adapt ", adapt.runs, "\n", sep="")
			}
			if(ini.runs > 0){
				scriptstring <- paste(scriptstring, "update ", ini.runs, "\n", sep="")
			}

			scriptstring <- paste(scriptstring, monitors, if(any(monitor=="pd.i")) paste("monitor pD, type(mean) thin(", thin, ")\n", sep=""), if(any(monitor=="pd")) paste("monitor pD, thin(", thin, ")\n", sep=""), if(any(monitor=="popt")) paste("monitor popt, type(mean) thin(", thin, ")\n", sep=""))
			
			if(real.runs > 0) scriptstring <- paste(scriptstring, "update ", real.runs, "\n", sep="")
		
			if(any(monitor=="pd.i")) scriptstring <- paste(scriptstring, "coda pD, stem(\"sim.", s, "/pd\")\n", sep="")
			if(any(monitor=="popt")) scriptstring <- paste(scriptstring, "coda popt, stem(\"sim.", s, "/popt\")\n", sep="")

			for(c in 1:nsim.chains[s]){
				i <- sim.chains[[s]][c]
				scriptstring <- paste(scriptstring, "parameters to \"out", i, ".Rdump\", chain(", c, ")\n", sep="")
			}

			scriptstring <- paste(scriptstring, "coda *, stem(sim.", s, "/CODA)\n", sep="")

			# model clear is used to detect the model being finished, update 0 is used to detect the model not having crashed
			scriptstring <- paste(scriptstring, "update 0\nmodel clear\nexit\n", sep="")

			output <- file(paste("sim.",s,"/script.cmd", sep=""), 'w')
			cat(scriptstring, file=output,sep="")  
			close(output)
			
		}

		for(i in 1:chains){
			cat(initstring[i], file=paste("inits", i, ".txt", sep=""),sep="")
			close(file(paste("inits", i, ".txt", sep="")))
		}


		os <- .Platform$OS.type	
		if(os == "windows"){		
			currentsyspath <- Sys.getenv('PATH')
			if(!grepl(method.options$libpaths$PATH,currentsyspath,fixed=TRUE)){
				Sys.setenv(PATH=paste(currentsyspath, ';', method.options$libpaths$PATH, sep=''))
				resetsyspath <- TRUE
			}

			currentsysbinpath <- Sys.getenv('LTDL_LIBRARY_PATH')
			if(!grepl(method.options$libpaths$LTDL_LIBRARY_PATH,currentsysbinpath,fixed=TRUE)){
				Sys.setenv(LTDL_LIBRARY_PATH=paste(currentsysbinpath, if(currentsysbinpath!='') ';', method.options$libpaths$LTDL_LIBRARY_PATH, sep=''))
				resetbinpath <- TRUE
			}		
		}	
	}
		
	result <- try(do.call(method, method.options))
	
	if(resetsyspath) Sys.setenv(PATH=currentsyspath)
	if(resetbinpath) Sys.setenv(LTDL_LIBRARY_PATH=currentsysbinpath)

	
	if(class(result)=="try-error") stop("An error occured while attempting to run the JAGS model")
		
	if(class(result)=="list"){
		results <- result
		if(any(names(results)=="complete")){
			result <- results$complete 
		}else{
			result <- results[[1]]
			names(results)[1] <- "complete"
		}
	}else{
		results <- list(complete=result)
	}
	
	if(class(result)=="character") stop(result)
		
	if(!is.logical(result)) stop("Unsupported return type from the JAGS method function")
	
		# Done by on.exit call earlier:
	# setwd(save.directory)	
	
	results <- c(results, list(jobname=jobname, directory=temp.directory, nsims=nsims, started=Sys.time()))
	return(results)		
	
}

				
