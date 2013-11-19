setup.jags <- function(model, monitor = stop("No monitored variables supplied"), data=NA,  n.chains=2, inits = replicate(n.chains, NA), modules=c(""), factories=c(""), jags = runjags.getOption('jagspath'), method="simple"){
	
	if(method %in% runjagsprivate$rjagsmethod) rjagsmethod <- TRUE else rjagsmethod <- FALSE
	
	if(rjagsmethod && !require("rjags")) stop("The rjags package is not installed")
	
	# Reset failedjags stuff:
	failedjags$model <- "No failed model available!"
	failedjags$data <- "No failed data available!"
	failedjags$inits <- "No failed initial values available!"
	failedjags$output <- "No failed model output available!"
	failedjags$end.state <- "No failed model parameter state available!"

	# We may be passed some unevaluated function arguments so evaluate everything here:
	argnames <- names(formals(setup.jags))
	for(i in 1:length(argnames)){
		assign(argnames[i], eval(get(argnames[i])))
	}
	
	jags.status <- testjags(jags, silent=TRUE)
	if(jags.status$JAGS.available==FALSE){
		if(jags.status$os=="windows"){
			# Try it again - sometimes this seems to clear it up:
			Sys.sleep(0.2)
			jags.status <- testjags(jags, silent=TRUE)
		}		
		jags <- jags.status$JAGS.path
		
		if(jags.status$JAGS.available==FALSE){			
			swcat("Unable to call JAGS using '", jags, "' - try specifying the path to the JAGS binary as the jags argument, or installing the rjags package.  Use the testjags() function for more detailed diagnostics.\n", sep="")
			stop("Unable to call JAGS", call.=FALSE)
		}
	}
	jags <- jags.status$JAGS.path
		
	if(!jags.status$JAGS.found && ! method%in%c("snow",runjagsprivate$rjagsmethod)){
		swcat("Unable to call JAGS using '", jags, "' - try specifying the path to the JAGS binary as the jags argument, or using the rjags method.  Use the testjags() function for more detailed diagnostics.\n", sep="")
		stop("Unable to call JAGS", call.=FALSE)
	}
	if(method%in%runjagsprivate$rjagsmethod && !require("rjags")){
		swcat("The rjags package was not found, either install the rjags package or use another method\n", sep="")
		stop("The rjags package was not found", call.=FALSE)
	}
		
	if(class(monitor)!="character" | all(is.na(monitor))){
		stop("Monitored variable(s) must be provided in the form of a character vector")
	}

	if(any(tolower(monitor)=="dic")){
		monitor <- c(monitor, "pd", "deviance")
	}
	if(any(tolower(monitor)=="deviance") & (any(tolower(monitor)=="pd") | any(tolower(monitor)=="popt"))){
		monitor <- c(monitor, "dic", "pd")
	}
	
	monitor[monitor=="DIC"] <- "dic"
	monitor[monitor=="pD"] <- "pd"
	monitor[monitor=="pD.i"] <- "pd.i"
	monitor[monitor=="pOpt"] <- "popt"	
	monitor <- unique(monitor)
	monitor <- na.omit(monitor[monitor!=""])
	
	if(any(c("popt", "pd", "pd.i", "deviance", "dic") %in% monitor)){
		modules <- c(modules, "dic")
	}
	modules[modules=="DIC"] <- "dic"
	modules <- unique(modules)
	modules <- na.omit(modules[modules!=""])
	
	if(class(model)!="character" | length(model)!=1){
		stop("The model must be provided in the form of a character string")
	}
	
	# Find references to functions in the runjags module (unless we have already been told to use the standalone runjags module):
	if(!any(modules=="runjagsmodule")){
		# Find any matching functions used (not in comments, and definitely used as a function not a variable):
		fs <- c("par1","par2","par3","par4","lomax","mouch","genpar")

		fs <- apply(expand.grid(c("~","<-"),c("d","p","q"),fs,"("),1,paste,collapse="")
		# Get rid of commented lines and remove all spaces:
		nohashstring <- paste(lapply(strsplit(model, "[\n\r]")[[1]], function(x) gsub("#.*", "", x)), collapse="\n")
		nohashstring <- gsub('[[:space:]]','',nohashstring)

		# Find any matches and add runjags to the modules if rjags method:
		if(any(sapply(fs,function(x) return(grepl(x,nohashstring,fixed=TRUE))))){
			if(rjagsmethod) modules <- c(modules, 'runjags') else warning("Pareto family functions provided by the runjags module are only available using the rjags method; to use these functions with other methods install (and specify using the module argument) the 'runjagsmodule' standalone module")
		}
	}
	
	if(class(data)=="list") suppressWarnings(try(data <- dump.format(data), silent=TRUE))
	suppressWarnings(if(is.na(data)) data <- "")	
	if(class(data)!="character" | length(data)!=1){
		stop("The data must be provided either as a named list or in R dump format (see dump.format())")
	}
	data <- paste(data, "\n", sep="")
	if(gsub("[[:space:]]", "", data)=="") data <- ""
		
	valid <- checkvalidforjags(data)	
	if(!valid$valid) stop(paste("The following problem was identified in the data provided:  ", valid$probstring, sep=""))		
		
	### SORT OUT INITS
	# If initial value list is a function, run it to get some actual initial values:
	if(class(inits)=="function"){
		if(is.null(formals(inits))){
			newinits <- lapply(1:n.chains, function(x) return(inits()))
		}else{
			s <- try(newinits <- lapply(1:n.chains, function(x) return(inits(x))))
			if(class(s)=="try-error") stop("There was an error using the function supplied for 'inits' - this function must take either 0 or 1 arguments")
		}
		if(! (all(sapply(newinits,class)=="list") || all(sapply(newinits,class)=="character"))) stop("There was an error using the function supplied for 'inits' - this function must return either a named list or a character vector")
		inits <- newinits
	}
	
	# If initial value list is a list of character strings, make it a vector of characters:
	if(class(inits)=="list" && all(sapply(inits,class)=="character")) inits <- unlist(inits)
	
	# If initial value list is a list of non-lists (i.e. specified for 1 chain we hope), make it into the expected format:
	if(class(inits)=="list" && !all(sapply(inits,class)=="list")){
		if(any(names(inits)=="")) stop("Unable to determine the intended format of the list provided as initial values; this should either be a list of named lists, or a character vector representing values for each chain")
		inits <- list(inits)
	}

	# Now convert inits to characters if it is a list:
	if(class(inits)=="list"){
		newinits <- character(length(inits))
		for(i in 1:length(inits)){
			suppressWarnings(try(newinits[i] <- dump.format(inits[[i]]), silent=TRUE))				
		}
		inits <- newinits		
	}
	
	if(class(inits)!='character' && (class(inits)!='list' || !(all(sapply(inits,class)=="list") || all(sapply(inits,class)=="character")))) stop("The format of the 'inits' variable supplied was not recognised - consult the help file for run.jags for the supported options")
		
	for(i in 1:length(inits)){
		valid <- checkvalidforjags(inits[i])	
		if(!valid$valid) stop(paste("The following problem was identified in the initial values provided for chain ", i, ":  ", valid$probstring, sep=""))				
	}
	
	if(all(is.na(inits))) inits <- replicate(n.chains, NA)
	if(length(inits)==1){
		inits <- replicate(n.chains, inits)
		if(n.chains!=1 && runjags.getOption('inits.warning')) warning("Only one set of initial values was provided.  The same initial values will be used across all chains (this is not recommended)", call.=FALSE)
	}
	
	if(all(!is.na(inits)) & any(class(inits)!="character")){
		stop("Initial values must be provided as a list of named lists, or a character vector in the R dump format (see dump.format()), with length equal to the number of chains required")
	}

	if(!is.na(n.chains)){
		if(length(inits) != n.chains){
			temp <- inits
			inits <- character(n.chains)
		
			suppressWarnings(inits[] <- temp)
			if(runjags.getOption('inits.warning')) warning("The number of chains specified did not match the number of initial value strings supplied.  Some initial value strings will be recycled or ignored", call.=FALSE)
		}
	}
	n.chains <- length(inits)
	
	if(length(grep('base::Mersenne-Twister', inits)>0) & as.numeric(jags.status$JAGS.version) < 2) warning('Using the RNG "base::Mersenne-Twister" (used by default for chain 4) may cause problems with restarting subsequent simulations using the end state of previous simulations due to a bug in JAGS version 1.x.  If you encounter the error "Invalid .RNG.state", please update JAGS to version 2.x and try again.  Or, you can change the random number generator by changing the .RNG.name to (for example) "base::Super-Duper" and remove the .RNG.state element of the list.', call.=FALSE)
	
	if(any(c("pd","pd.i","popt") %in% monitor) & (n.chains < 2 )) stop("The DIC, pD, pD.i and popt cannot be assessed with only 1 chain")
	if(any(c("pd","pd.i","popt","deviance") %in% monitor) & jags.status$JAGS.version < 2) stop('Support for the deviance, pD and popt monitors is no longer available for JAGS version 1.x.  Please update to JAGS version 3.x')
	
	
	# Combine model blocks, change \r to \n and get rid of double spacing:
	model <- paste(model, "\n", sep="", collapse="\n")
	model <- gsub("\r","\n",model)
	model <- gsub("\n\n","\n",model)
	model <- gsub("\n\n","\n",model)
	
	# Combine data blocks, change \r to \n and get rid of double spacing:
	data <- paste(data, "\n", sep="", collapse="\n")
	data <- gsub("\r","\n",data)
	data <- gsub("\n\n","\n",data)
	data <- gsub("\n\n","\n",data)
	if(gsub("[[:space:]]", "", data)=="") data <- ""
	
	monitorcollapse <- ">\nmonitor set <"
	monitors <- paste("monitor set <", paste(monitor, collapse=monitorcollapse), ">\n", sep="")
	n.params <- length(monitor)
	params.names <- monitor
	
	
	temp.directory <- tempfile('runjagsdir')
	dir.create(temp.directory)
	cwd <- getwd()
	on.exit({
		setwd(cwd)
		unlink(temp.directory, recursive=TRUE)
		})

	setwd(temp.directory)

	cat(model, file="model.txt",sep="")  
	cat(data, file="data.txt",sep="")  
	for(i in 1:n.chains){
		if(is.na(inits[i])) inits[i] <- ""
		
		# Combine inits blocks, change \r to \n and get rid of double spacing:
		inits[i] <- paste(inits[i], "\n", sep="", collapse="\n")
		inits[i] <- gsub("\r","\n",inits[i])
		inits[i] <- gsub("\n\n","\n",inits[i])
		inits[i] <- gsub("\n\n","\n",inits[i])

		if(gsub("[[:space:]]", "", inits[i])=="") inits[i] <- ""
			
		cat(inits[i], file=paste("inits",i,".txt",sep=""),sep="")
	}
	
	if(!all(is.character(modules))) stop("The vector provided for 'modules' must be a character vector naming one or more modules to be loaded in JAGS")
	if(!all(is.character(factories)) || !all(factories=="" | (grepl("(",factories,fixed=TRUE) & grepl(")",factories,fixed=TRUE)))) stop("The vector provided for 'factories' must be a character vector naming one or more factories to be loaded in JAGS with the following format:  <facname>(<factype>)")

	if(!rjagsmethod){
		# Modules/factories and model setup/checking etc can be done by as.jags.runjags later
		
		resetsyspath=resetbinpath <- FALSE
		if(.Platform$OS.type == "windows"){		
			currentsyspath <- Sys.getenv('PATH')
			if(!grepl(jags.status$libpaths$PATH,currentsyspath,fixed=TRUE)){
				Sys.setenv(PATH=paste(currentsyspath, ';', jags.status$libpaths$PATH, sep=''))
				resetsyspath <- TRUE
			}

			currentsysbinpath <- Sys.getenv('LTDL_LIBRARY_PATH')
			if(!grepl(jags.status$libpaths$LTDL_LIBRARY_PATH,currentsysbinpath,fixed=TRUE)){
				Sys.setenv(LTDL_LIBRARY_PATH=paste(currentsysbinpath, if(currentsysbinpath!='') ';', jags.status$libpaths$LTDL_LIBRARY_PATH, sep=''))
				resetbinpath <- TRUE
			}		
		}	
		
		scriptstring <- ""
		if(length(modules)>0) for(i in 1:length(modules)){
			if(modules[i]=="runjags") stop("The runjags module is only available using the rjags method; to use the functions provided with other methods install (and specify using the module argument) the 'runjagsmodule' standalone module")
			if(modules[i]!=""){
				scriptstring <- paste(scriptstring, "load ", modules[i], "\n", sep="")
			}
		}
			
		if(length(factories)>0) for(i in 1:length(factories)){
			if(factories[i]!=""){
				f <- strsplit(gsub(")","",factories[i],fixed=TRUE),"(",fixed=TRUE)[[1]]					
				scriptstring <- paste(scriptstring, "set factory \"", f[1], "\" on, type(", f[2], ")\n", sep="")
			}
		}

	
		cat(scriptstring, "\nexit\n", file="script.cmd", sep="")  
		output <- system2(jags, stdout=TRUE, stderr=TRUE, stdin="script.cmd", wait=TRUE)
		if(grepl("file not found", paste(output,collapse="\n"))){
			cat(output, sep="\n")
			stop("Error reading modules or factories (see output above for more details)")
		}
	
		scriptstring <- paste(scriptstring, "model in <\"model.txt\">\n", sep="")

		cat(scriptstring, "\nexit\n", file="script.cmd", sep="", append=FALSE)  
		output <- system2(jags, stdout=TRUE, stderr=TRUE, stdin="script.cmd", wait=TRUE)
		if(grepl("error", tolower(deparse(paste(output,collapse="\n"))))){
			cat(output, sep="\n")
			
			failedjagsmodel <- model
			class(failedjagsmodel) <- "runjags.model"
			assign("model", failedjagsmodel, envir=failedjags)

			stop("Error reading model (see output above for more details, and examine 'failedjags$model' to see model syntax with line numbers)")
		}
	
		if(data!=""){
			scriptstring <- paste(scriptstring, "data in <\"data.txt\">\n", sep="")
	
			cat(scriptstring, "\nexit\n", file="script.cmd", sep="", append=FALSE)  
			output <- system2(jags, stdout=TRUE, stderr=TRUE, stdin="script.cmd", wait=TRUE)
			if(grepl("error", tolower(deparse(paste(output,collapse="\n"))))){
				cat(output, sep="\n")
				class(data) <- 'runjags.data'
				assign("data", data, envir=failedjags)
				
				stop("Error reading data (see output above for more details, and examine 'failedjags$data' to see data file syntax with line numbers)")
			}
		}
	
		for(i in 1:n.chains){
			# Would have to compile the model to check inits, but we can check them as data:
			if(inits[i]!=""){
				cat(paste(scriptstring, "data in <\"inits",i,".txt\">\n", sep=""), "\nexit\n", file="script.cmd", sep="", append=FALSE)  
				output <- system2(jags, stdout=TRUE, stderr=TRUE, stdin="script.cmd", wait=TRUE)
				if(grepl("error", tolower(deparse(paste(output,collapse="\n"))))){
					cat(output, sep="\n")
					class(inits) <- 'runjags.inits'
					assign("inits", inits, envir=failedjags)
					
					stop(paste("Error reading initial values for chain ", i, " (see output above for more details, and use 'failedjags$inits' to see init file syntax with line numbers)",sep=""), call.=FALSE)
				}
			}
		}
		
		if(resetsyspath) Sys.setenv(PATH=currentsyspath)
		if(resetbinpath) Sys.setenv(LTDL_LIBRARY_PATH=currentsysbinpath)
	}

	class(model) <- "runjags.model"
	class(inits) <- "runjags.inits"
	class(data) <- "runjags.data"
	
	output <- list(mcmc=as.mcmc.list(lapply(1:n.chains, function(x) return(as.mcmc(NA)))), pd=NA, popt=NA, pd.i=NA, end.state=inits, burnin=0, sample=0, thin=1, summary="", HPD="", hpd="", mcse="", psrf="", autocorr="", crosscorr="", stochastic="", dic="", trace=NA, density=NA, model=model, data=data, monitor=monitor, modules=modules, factories=factories, method=NA, method.options=list(), timetaken=0)
	class(output) <- "runjags"
	
	return(output)
}


setup.jagsfile <- function(model, datalist = NA, initlist = NA, n.chains=NA, data=NA, inits=NA, monitor=NA, modules=c(""), factories=c(""), jags=runjags.getOption('jagspath'), method="simple", call.setup=TRUE, failincomplete=TRUE){
	
	# We may be passed some unevaluated function arguments so evaluate everything here:
	argnames <- names(formals(setup.jagsfile))
	for(i in 1:length(argnames)){
		assign(argnames[i], eval(get(argnames[i])))
	}
	
	if(class(data)=="list") suppressWarnings(try(data <- dump.format(data), silent=TRUE))

	suppressWarnings(if(is.na(data)) class(data) <- "character")
	
	if(class(initlist)=="function"){
		success <- suppressWarnings(try(newinitlist <- initlist(1), silent=TRUE))
		if(class(success)=="try-error") newinitlist <- initlist()
		if(class(newinitlist)!="list") stop("initlist must return a named list if specified as a function")
	}
	if(class(datalist)=="function"){
		success <- suppressWarnings(try(newdatalist <- datalist(1), silent=TRUE))
		if(class(success)=="try-error") newdatalist <- datalist()
		if(class(newdatalist)!="list") stop("datalist must return a named list if specified as a function")
	}
	
	if(is.na(n.chains) && !any(is.function(inits)) && any(!is.na(inits))) n.chains <- length(inits)
	
	path <- model
	params <- read.winbugs(path)

	autodata <- params$autodata
	autoinits <- params$autoinits
	maindata <- params$data
	maininits <- params$inits
	
	if(params$model=="model{\n\n}\n") stop("No valid model was specified or found in the model block")
	outmodel <- params$model
	
	# This function should always (within runjags package) be called from the top level function, AND NOT DIRECTLY, otherwise the scoping will be out:
	parselevel <- 3
	if(is.na(data)){
		if(all(is.na(maindata))) maindata <- ""
		outdata <- as.character(maindata)
		
		if(!all(is.na(autodata))){
			autodata <- find.parameters(autodata, datalist, parselevel)
			outdata <- paste(outdata, dump.format(autodata), sep="\n")
		}
	}else{
		# Hidden option for datalist to be NaN in which case the warning about data being ignored from the model file (used by run.jags.study)
		if(!is.nan(datalist) && (!identical(autodata, NA) | !identical(maindata, NA)) && runjags.getOption('blockcombine.warning')) warning("Data was specified in the model block but will be ignored since data was specified in the arguments for (auto)run.jagsfile", call.=FALSE)
		outdata <- data
	}
	
	if(!is.function(inits) && all(is.na(inits))){
		if(all(is.na(maininits))){
			if(is.na(n.chains)){
				n.chains <- 2
				if(runjags.getOption('inits.warning')) warning("No initial value blocks found and n.chains not specified.  2 chains were used.", call.=FALSE)
			}
			outinits <- character(length=n.chains)
		}else{
			if(is.na(n.chains)) n.chains <- length(maininits)
			if(length(maininits)!=n.chains){
				if(runjags.getOption('inits.warning')) warning("The number of initial value blocks found does not correspond to the number of chains specified.  Some initial values were recycled or ignored.", call.=FALSE)
				
				temp <- maininits
				outinits <- character(n.chains)

				suppressWarnings(outinits[] <- temp)
				
			}else{
				outinits <- maininits
			}

		}
				
		if(!all(is.na(autoinits))){
			for(i in 1:n.chains){
				geninits <- find.parameters(autoinits, initlist, parselevel, chain=i)
				outinits[i] <- paste(outinits[i], dump.format(geninits), sep="\n")
			}
		}
	}else{
		if((!identical(autoinits, NA) | !identical(maininits, NA)) && runjags.getOption('blockcombine.warning')) warning("Initial values were specified in the model block but will be ignored since initial values were specified in the arguments for (auto)run.jagsfile", call.=FALSE)
		outinits <- inits
	}
	
	if(!is.function(outinits)) outinits[outinits==""] <- NA
	
	if(outdata==""){
		outdata <- NA
		warning("The model was run without data since no data was provided or found in the model block.", call.=FALSE)
	}
		
	if(!all(is.na(monitor))){
		if(!identical(params$monitor, NA)  && runjags.getOption('blockcombine.warning')) warning("Monitors were specified in the model block but will be ignored since monitors were specified in the arguments to the function call", call.=FALSE)
		outmonitor <- monitor
	}else{
		outmonitor <- params$monitor
	}
	
	outmonitor <- as.character(outmonitor)
	outmonitor[outmonitor==""] <- NA
	outmonitor <- sort(outmonitor[!is.na(outmonitor)])
	
	if(length(outmonitor)==0 && failincomplete) stop("No monitors were specified or found in the model block", call.=FALSE)
	
	if(is.na(n.chains)){
		if(is.function(outinits)) stop("Unable to determine the number of chains required from the init function; please supply a value for n.chains")	
		n.chains <- length(outinits)
	}
	
	if(!is.function(outinits) && n.chains!=length(outinits)){
		
		temp <- outinits
		outinits <- character(n.chains)
		
		suppressWarnings(outinits[] <- temp)
		if(runjags.getOption('inits.warning')) warning("The number of chains specified did not match the number of initial value strings supplied.  Some initial value strings were recycled or ignored", call.=FALSE)
	}
	
	
	lengths <- lapply(params, length)
	if(any(lengths==0) && failincomplete) stop(paste("No ", paste(names(lengths[lengths==0]), collapse=" or "), " blocks or tags were found", sep=""))
	
	if(call.setup){
		return(setup.jags(model=outmodel, monitor = outmonitor, data=outdata, n.chains=n.chains, inits = outinits, modules=modules, factories=factories, jags = jags, method=method))
	}else{
		return(list(model=outmodel, monitor = outmonitor, data=outdata, n.chains=n.chains, inits = outinits, modules=modules, factories=factories, jags = jags))
	}
	
}

setup.JAGS <- setup.jags