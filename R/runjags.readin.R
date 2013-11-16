runjags.readin <- function(directory, copy, delete, silent.jags, target.iters, n.chains, monitor, method, method.options, suspended=FALSE){
	
	save.directory <- getwd()
	temp.directory <- directory
	
	on.exit({
		setwd(save.directory)
	})
	
	setwd(temp.directory)
	
	# dummy variable to get rid of binding warnings - actually loaded from simchainsinfo:
	sim.chains <- list()
	success <- try(load("simchainsinfo.Rsave"))
	if(class(success)=="try-error") stop("The required 'simchainsinfo.Rsave' file was not found in the root simulation directory, please file a bug report to the package developer!")
	n.sims <- length(sim.chains)
	

	allok <- FALSE
	on.exit({

		if (!allok){
			
			failedjagsmodel <- paste(readLines("model.txt", warn=FALSE), collapse="\n")
			class(failedjagsmodel) <- "runjags.model"
			assign("model", failedjagsmodel, envir=failedjags)

			failedd <- paste(readLines("data.txt", warn=FALSE), collapse="\n")
			class(failedd) <- "runjags.data"
			assign("data", failedd, envir=failedjags)
			
			failedi <- character(n.chains)
			for(i in 1:n.chains){
				failedi[i] <- paste(readLines(paste("inits",i,".txt",sep=""), warn=FALSE), collapse="\n")
			}
			class(failedi) <- "runjags.inits"
			assign("inits", failedi, envir=failedjags)
			
			failedo <- character(n.sims)
			for(i in 1:n.sims){
				failedo[i] <- paste(readLines(paste("sim.",i,"/jagsoutput.txt",sep=""), warn=FALSE), collapse="\n")
			}
			class(failedo) <- "runjags.output"
			assign("output",failedo, envir=failedjags)
			
		}
		
		setwd(save.directory)
		
		if(copy){
			new.directory <- if(class(method)=="list" && method$method=='xgrid') new_unique(method$jobname, touch=TRUE, type='folder') else new_unique('runjagsfiles', touch=TRUE, type='folder')			
			if((new.directory=="Directory not writable")==TRUE){
				warning("JAGS files could not be copied to the working directory as it is not writable")
			}else{
				# file.rename may not work on all platforms for directories so use file.copy instead:
				file.copy(from=file.path(temp.directory, list.files(temp.directory)), to=new.directory, recursive=TRUE)
				cat("JAGS files were saved to the '", new.directory, "' folder in your current working directory\n", sep="")
			}
		}
		if(delete) unlink(temp.directory, recursive = TRUE)
		
	})
	
	setwd(temp.directory)
	
  	if(length(list.files(pattern="JAGS.out",recursive=TRUE))>0){
   		stop("You are using a version of JAGS prior to 0.99.0, which is no longer supported.  Please update JAGS and try again")
	}
	
	
	simfolders <- paste("sim.",1:n.sims,sep="")

	if(length(simfolders)==0){
		# This skips all of the output checking as we can't be sure what has happened with custom JAGS methods
		warning("No simulation sub-folders found in the provided directory - looking for chain outputs in the root folder...")
	}else{
	
		outputs <- vector('list', length=n.sims)
		for(i in 1:n.sims){
			tries <- 0
			repeat{
				outputs[[i]] <- try(paste(readLines(paste("sim.",i,"/jagsoutput.txt",sep=""), warn=FALSE), collapse="\n"), silent=TRUE)
				if(class(outputs[[i]])!="try-error") break
				tries <- tries +1
				if(tries==11){
					if(n.sims==1) stop("Unable to read the output of the simulation") else stop(paste("Unable to read the output of simulation ", i, sep=""))
				}
				Sys.sleep(0.5)
			}
		}
		
		# Check that all simulations have finished (again - just to be sure):
		finished <- sapply(outputs,function(x) return(grepl("[[:space:]]Deleting model",x)))
		if(!all(finished)){
			if(suspended){
				allok <- TRUE
				delete <- FALSE
				copy <- FALSE
				stop("The simulations have not finished yet") 
			}else{
				stop("An unknown error occured - the simulation(s) appear to have not finished; check the model output in failedjags$output for clues")
			}
		}
			
		# Check for warnings about no monitored nodes ## MODIFY WHEN ALLOWING NO MONITORS ##:
		nomons <- sapply(outputs,function(x) return(grepl("[[:space:]]There are no monitors[[:space:]]",x)))
		if(any(nomons)) stop("The monitored nodes indicated do not exist in the model")
		
		# Check that all simulations didn't crash - if batch mode there will only be an Updating 0 if successful, if not batch mode there will always be a can't update no model if unsuccessful:
		successful <- sapply(outputs,function(x){
			return(grepl("[[:space:]]Updating 0[[:space:]]",x) & !grepl("[[:space:]]Can't update. No model![[:space:]]",x))
			})
	
		if(!all(successful)){
			#### LOOK FOR THE CRASHED DUMP FILES HERE??? ####
			if(n.sims==1) stop("The simulation appears to have crashed - check the model output in failedjags$output for clues") else stop(paste("Simulation number(s) ", paste(which(!successful),collapse=", "), " appear(s) to have crashed; check the output in failedjags$output for clues", sep=""))
		} 
		
		# Now we have established all the simulations exited successfully, but there may be delays in obtaining the coda output...		
		increment <- 0.5
		msgtime <- 3/increment
		timeout <- 16/increment
		
		# First wait for all of the codaindex files to appear:
		tries <- 0
		repeat{
			indexok <- file.exists(paste("sim.",1:n.sims,.Platform$file.sep,"CODAindex.txt",sep=""))
			if(all(indexok)) break
			tries <- tries +1
			if(tries==msgtime) cat("Waiting for the CODA index files to be created...\n")
			if(tries==timeout) stop(paste("Timed out waiting for the CODA index files to be created - the files available at time out were: ", paste(list.files(recursive=TRUE),collapse=", "), ".  Please file a bug report (including this message) to the runjags package author.", sep=""))
			Sys.sleep(increment)
		}
		
		# Now wait for all of the codachain files to appear:
		codapaths <- unlist(lapply(1:n.sims,function(x) return(paste("sim.",x,.Platform$file.sep,"CODAchain",1:length(sim.chains[[x]]),".txt",sep=""))))
		tries <- 0
		repeat{
			indexok <- file.exists(codapaths)
			if(all(indexok)) break
			tries <- tries +1
			if(tries==msgtime) cat("Waiting for the CODA files to be created...\n")
			if(tries==timeout) stop(paste("Timed out waiting for the CODA files to be created - the files available at time out were: ", paste(list.files(recursive=TRUE),collapse=", "), ".  Please file a bug report (including this message) to the runjags package author.", sep=""))
			Sys.sleep(increment)
		}
		
		# Now wait for all of the codachain files to be at least 95% of the max file size (always seem to be much closer than this):
		tries <- 0
		repeat{
			fi <- file.info(codapaths)
			if(all(fi[,'size']>0) && all(fi[,'size']/max(fi[,'size']) > 0.95)) break
			tries <- tries +1
			if(tries==msgtime) cat("Waiting for the CODA files to be completed...\n")
				# If the coda files exist, wait a LONG time for them to be the correct sizes before giving up!
			if(tries==(60*5/increment)) stop(paste("Timed out waiting for the CODA files to be completed - the file size and modification times at ", Sys.time(), " were: ", paste(paste(codapaths,fi[,'size'],as.character(fi[,'mtime']), sep=" : "),collapse=", "), ".  Please file a bug report (including this message) to the runjags package author.", sep=""))
			Sys.sleep(increment)
		}
		
		# At this point everything should be OK to proceed....
		
		for(s in 1:length(simfolders)){
			# This should never happen as we have already established sim.s/jagsoutput.txt exists...
			if(!paste("sim", s, sep=".")==simfolders[s]) stop(paste("The sub-folder for simulation ", s, " is missing from the root simulation directory", sep=""))
			
			setwd(simfolders[s])
			
			# Try to copy the script file back to the main folder, mostly for my benefit:
			suppressWarnings(try(file.rename("script.cmd", paste("../script.",s,".cmd",sep="")), silent=TRUE))
		
			# Copy the chains to the root simulation directory and renumber at the same time:
			fakenums <- 1:length(sim.chains[[s]])
			realnums <- sim.chains[[s]]
			for(c in fakenums){
				
				# One last repeat failsafe...
				tries <- 0
				repeat{
					success <- file.rename(paste("CODAchain",c,".txt",sep=""), paste("../CODAchain",realnums[c],".txt",sep=""))
					if(success) break
					tries <- tries +1
					if(tries==5) stop(paste("There was an error moving the coda file for chain ", realnums[c], sep=""))
					Sys.sleep(1)
				}
				close(file(paste("../CODAchain",realnums[c],".txt",sep="")))
			}

			# These were created before the coda files, so just assume they are OK:
			fm <- list.files(pattern="pd[[:graph:]]*.txt")
			file.rename(from=fm, to=file.path('..',fm))
			for(f in file.path("../",fm)) close(file(f))
			fm <- list.files(pattern="popt[[:graph:]]*.txt")
			file.rename(from=fm, to=file.path('..',fm))
			for(f in file.path("../",fm)) close(file(f))

			if(s==1){
				success <- file.rename(from='CODAindex.txt', to='../CODAindex.txt')
				if(!success) stop("There was an error copying the coda index file")
				close(file("../CODAindex.txt"))
				for(f in c("CODAchain0.txt","CODAindex0.txt","CODAtable0.txt")){
					if(file.exists(f)){
						file.rename(from=f, to=file.path("..",f))
						close(file(file.path("..",f)))
					}
				}
			}

			setwd(temp.directory)
			# Won't delete the sim folder if there was a problem - has command and potentially interesting stuff in it!!
			unlink(simfolders[s], recursive=TRUE)
		}
	}
	
	setwd(temp.directory)
	
	chainscopied <- sort(as.numeric(gsub("[CODAchain,.txt]", "", list.files(pattern="CODAchain[[:digit:]]+[.]txt"))))
	# We might have a codachain0 if monitoring DIC so remove from the fakenums:
	chainscopied <- chainscopied[chainscopied!=0]					

	if(length(chainscopied)!=n.chains){
		stop(paste("Expected ", n.chains, " chains to be output but found ", length(chainscopied), " in the root simulation directory - please file a bug report to the runjags package author", sep=""))
		
	}
	
	allok <- TRUE

	swcat("Simulation complete.  Reading coda files...\n")
	
	suppressWarnings(inputsuccess <- try(input.data <- read.openbugs(quiet=TRUE), silent=TRUE))
	if((class(inputsuccess)=="try-error")){
	
		filename <- paste("jags.dump", 1, ".R", sep="")
		suppressWarnings(try(inputsuccess <- try(tempinput <- readLines(filename)), silent=TRUE))
		if(class(inputsuccess)=="try-error"){
			stop("Unable to load model output - please file a bug report to the runjags package author.")
		}else{
			stop("The model appears to have crashed during the burnin period.  Check the failedjags$output variable for clues.")
		}
	}
	
	# Last failsafe:
	if(nchain(input.data)!=n.chains) stop(paste("Expected ", n.chains, " chains to be returned but only found ", nchain(input.data), " - please file a bug report to the runjags package author", sep=""))
	
	swcat("Coda files loaded successfully\n")
	achieved <- niter(input.data)

	pd=pd.i=popt <- NA
	
	suppressWarnings({
		if(any(monitor=="pd")){
			pd <- try(read.coda('CODAchain0.txt','CODAindex0.txt'), silent=TRUE)
			if(class(pd)=="try-error"){
				swcat("\n")			
				stop("There was an error reading the pD")
			}
		}
		if(any(monitor=="pd.i")){
			pdtab <- try(read.table('pdtable0.txt', header=FALSE), silent=TRUE)
			if(class(pdtab)=="try-error"){
				warning("There was an error reading the pd.i")
			}else{
				pd.i <- as.matrix(pdtab[,2])
				dimnames(pd.i) <- list(pdtab[,1], 'mean pd.i')
			}
		}
		if(any(monitor=="popt")){
			popttab <- try(read.table('popttable0.txt', header=FALSE), silent=TRUE)
			if(class(popttab)=="try-error"){
				warning("There was an error reading the popt")
			}else{
				popt <- as.matrix(popttab[,2])
				dimnames(popt) <- list(popttab[,1], 'popt')
			}
		}
	})
	
	if(achieved!=target.iters){
		crashed <- TRUE
		swcat("Warning:  Simulation crashed after ", achieved, " iterations\n", sep="")
		
		crash.end <- character(length=n.chains)
		for(i in 1:n.chains){
			filename <- paste("jags.dump", i, ".R", sep="")
			suppressWarnings(inputsuccess <- try(tempinput <- readLines(filename), silent=TRUE))
			if(class(inputsuccess)=="try-error"){
				swcat("Error reading crash point of chain ", i, ".\n", sep="")
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
		input.end <- character(length=n.chains)
		for(i in 1:n.chains){
			filename <- paste("out", i, ".Rdump", sep="")
			suppressWarnings(inputsuccess <- try(tempinput <- readLines(filename), silent=TRUE))
			if(class(inputsuccess)=="try-error"){
				swcat("Error reading end point of chain ", i, ".\n", sep="")
				input.end[i] <- NA
			}else{
				input.end[i] <- ""
				for(j in 1:length(tempinput)){
					input.end[i] <- paste(input.end[i], tempinput[j], "\n", sep="")
				}
			}
		}
	}
	



	if(any(is.na(unlist(input.data)))){
	
		nastring <- ""
	
		varnames <- dimnames(input.data[[1]])[[2]]
	
		for(i in 1:n.chains){
		
			for(j in 1:nvar(input.data)){
			
				if(any(is.na(input.data[[i]][,j]))){
				
					nastring <- paste(nastring, if(nastring!="") ", ", varnames[j], " (chain ", i, ")", sep="")
				
				}
			
			}
		
		}
	
		stop(paste("One or more of the values for the monitored variable(s) '", nastring, "' was invalid (missing data).  Ensure that the model syntax is correct and that appropriate starting values have been given.", sep=""))
	}

	if(crashed) inits <- unlist(crash.end) else inits <- unlist(input.end)
	class(inits) <- 'runjags.inits'
	return(list(mcmc=input.data, pd=pd, popt=popt, pd.i=pd.i, end.state=inits))
}
