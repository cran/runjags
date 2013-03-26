runjags.readin <- function(directory, copy, delete, silent.jags, target.iters, n.chains, monitor, method, method.options, suspended=FALSE){
	
	save.directory <- getwd()
	temp.directory <- directory
	
	on.exit({
		setwd(save.directory)
		})
	
	setwd(temp.directory)
	
	simfolders <- sort(list.files()[grepl("sim.[[:digit:]]",list.files())])
	success <- logical(length(simfolders))

	if(length(simfolders)==0){
		warning("No simulation sub-folders found in the provided directory - looking for chain outputs in the root folder...")
	}else{
				
		for(s in 1:length(simfolders)){
			if(!paste("sim", s, sep=".")==simfolders[s]) stop("One or more of the simulation sub-folders appears to be missing from the provided directory")
			
			setwd(simfolders[s])
			
		  	if (file.exists("JAGS.out")){
		   		stop("You are using a version of JAGS prior to 0.99.0, which is no longer supported.  Please update JAGS and try again")
			}
			
			if(file.exists("CODAindex.txt")){
				codaok <- TRUE
			}else{
				# If the coda file doesn't exist initially, wait a few secs:
				Sys.sleep(2)
				
				# If it still doesn't exist, skip reading files and stop with an error later on:				
				if(file.exists("CODAindex.txt")){
					codaok <- TRUE
				}else{
					codaok <- FALSE					
					break
				}
			}
			
			# Files may not exist if simulation crashed:
			reads <- try(suppressWarnings({
				# Inits and out files will have correct numbering - but CODAchain will start at 1 for each sim, so adjust:
				realnums <- sort(as.numeric(gsub("[inits,.txt]", "", list.files()[grepl("inits[[:digit:]]+.txt",list.files())])))
				fakenums <- sort(as.numeric(gsub("[CODAchain,.txt]", "", list.files()[grepl("CODAchain[[:digit:]]+.txt",list.files())])))				
				# We might have a codachain0 if monitoring DIC so remove from the fakenums:
				fakenums <- fakenums[fakenums!=0]
				
				# If we're still waiting for coda to finish outputting chains - wait a bit longer:
				if(length(realnums)!=length(fakenums)) Sys.sleep(5)

				if(!all(fakenums==(1:length(fakenums)))) stop("There was an error in renaming the chain output")

				# Temporarily rename to avoid conflicts while we swap the numbers around:
				for(c in 1:length(realnums)) file.rename(paste("CODAchain",fakenums[c],".txt",sep=""), paste("TCODAchain",fakenums[c],".txt",sep=""))
				# Then rename back and reorder:
				for(c in 1:length(realnums)) file.rename(paste("TCODAchain",fakenums[c],".txt",sep=""), paste("CODAchain",realnums[c],".txt", sep=""))

				# Use close to make sure the files are written out:
				fm <- list.files()[grepl("CODA[[:graph:]]*.txt",list.files())]
				file.copy(from=fm, to='..')
				for(f in paste("../",fm,sep="")) close(file(f))
				fm <- list.files()[grepl("pd[[:graph:]]*.txt",list.files())]
				file.copy(from=fm, to='..')
				for(f in paste("../",fm,sep="")) close(file(f))
				fm <- list.files()[grepl("popt[[:graph:]]*.txt",list.files())]
				file.copy(from=fm, to='..')
				for(f in paste("../",fm,sep="")) close(file(f))
				fm <- list.files()[grepl("inits[[:digit:]]+.txt",list.files())]
				file.copy(from=fm, to='..')
				for(f in paste("../",fm,sep="")) close(file(f))
				fm <- list.files()[grepl("out[[:digit:]]+.Rdump",list.files())]
				file.copy(from=fm, to='..')
				for(f in paste("../",fm,sep="")) close(file(f))
				if(s==1){
					file.copy(from='CODAindex.txt', to='..')
					close(file("../CODAindex.txt"))
				}
			}))
			if(class(reads)=="try-error"){
				success[s] <- FALSE
			}else{
				success[s] <- TRUE
					
			}
			setwd(temp.directory)
				# Don't delete the sim folder - has command and potentially interesting stuff in it:
			# unlink(sim, recursive=TRUE)
		}
	}
	
	setwd(temp.directory)
	
	if(!codaok && suspended){
		stop("The CODA files were not found for one or more simulations - JAGS may not have finished, or there may have been an error with the model.  Check to see if the processes have finished and try again.", call.=FALSE)
	}
	
	on.exit({
		setwd(save.directory)
		if(copy){
			new.directory <- if(class(method)=="list" && method$method=='xgrid') new_unique(method$jobname, touch=TRUE, type='folder') else new_unique('runjagsfiles', touch=TRUE, type='folder')			
			if((new.directory=="Directory not writable")==TRUE){
				warning("JAGS files could not be copied to the working directory as it is not writable")
			}else{
				file.copy(from=paste(temp.directory, list.files(temp.directory), sep=.Platform$file.sep), to=new.directory, recursive=TRUE)
				cat("JAGS files were saved to the '", new.directory, "' folder in your current working directory\n", sep="")
			}
		}
		if(delete) unlink(temp.directory, recursive = TRUE)
		
		})
	
	
	if (any(!success)){
		
		if(!codaok )
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
		
		if(silent.jags){
			stop("No model output from one or more simulations; the model may not have compiled correctly, or there was a conflict between the initial values provided and data, or the monitored node(s) don't exist.  Run the model with silent.jags=FALSE and check the model output for clues.", call.=FALSE)
		}else{
			stop("No model output from one or more simulations; the model may not have compiled correctly, or there was a conflict between the initial values provided and data, or the monitored node(s) don't exist.  Check the model output above for clues (it may also help to inspect the jags model, data and initial values which have been saved to failedjags$model, failedjags$data and failedjags$inits respectively).", call.=FALSE)
		}
	}

	if(!silent.jags & class(method)=="character" ) swcat("\n")

	swcat("Simulation complete.  Reading coda files...\n")
	

	
	
	suppressWarnings(inputsuccess <- try(input.data <- read.openbugs(quiet=TRUE), silent=TRUE))

	if((class(inputsuccess)=="try-error")){
		
		# Sometimes it's taking a while to move files maybe?
		Sys.sleep(2)
		
		suppressWarnings(inputsuccess <- try(input.data <- read.openbugs(quiet=TRUE), silent=TRUE))
		
		if((class(inputsuccess)=="try-error")){
		
			filename <- paste("jags.dump", 1, ".R", sep="")
			suppressWarnings(try(inputsuccess <- try(tempinput <- readLines(filename)), silent=TRUE))
			if(class(inputsuccess)=="try-error"){
				if(silent.jags){
					stop("Unable to load model output; the model may not have compiled correctly, or there was a conflict between the initial values provided and data, or the monitored node(s) don't exist.  Run the model with silent.jags=FALSE and check the model output for clues.")
				}else{
					stop("Unable to load model output; the model may not have compiled correctly, or there was a conflict between the initial values provided and data, or the monitored node(s) don't exist.  Check the model output above for clues.")
				}			
			}else{
				if(silent.jags){
					stop("The model appears to have crashed during the burnin period.  Ensure that the syntax is correct and that appropriate prior distributions and starting values have been given.  Also try running the model with silent.jags=FALSE and check the model output for clues.")
				}else{
					stop("The model appears to have crashed during the burnin period.  Ensure that the syntax is correct and that appropriate prior distributions and starting values have been given.  Also try checking the model output above for clues.")
				}
			}
		}
	}
	
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
