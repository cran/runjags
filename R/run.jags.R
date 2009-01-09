run.jags <- function(model=stop("No model supplied"), monitor = stop("No monitored variables supplied"), data=NA,  n.chains=2, inits = replicate(n.chains, NA), burnin = 5000, sample = 10000, adapt=if(burnin<200) 100 else 0, jags = findjags(), silent.jags = FALSE, check.conv=TRUE){

updates <- sample

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
if(test[[2]][1]==FALSE){
	cat("Unable to call JAGS using '", jags, "'\n", sep="")
	stop("Unable to call JAGS")
}

inits <- unlist(inits)
if(all(is.na(inits))) inits <- replicate(n.chains, NA)
if(length(inits)==1) inits <- replicate(n.chains, inits)

if(length(inits) != n.chains) warning("Number of chains does not match length of initial values, n.chains specified will be ignored")

n.chains <- length(inits)


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

if(class(monitor)!="character"){
	stop("Monitored variable(s) must be provided in the form of a character vector")
}

if(check.conv==TRUE & chains==1){
	cat("Warning:  Convergence cannot be assessed with only 1 chain\n")
}

modelstring <- paste(model, "\n", sep="")

monitors <- paste("monitor set <", paste(monitor, collapse=">\nmonitor set <"), ">\n", sep="")
n.params <- length(monitor)
params.names <- monitor
	
initstring <- paste(inits, "\n", sep="")
	
datastring <- paste(data, "\n", sep="")
	
#  Write model file

total.updates <- n.params * updates

save.directory <- getwd()
on.exit(setwd(save.directory))

temp.directory <- new_unique(name=c("temp.", NA))
if((temp.directory=="Directory not writable")==TRUE){
	cat("Directory not writable\n")
	return(c("Error", "Write permissions"))
}

dir.create(temp.directory)
setwd(temp.directory)


output <- file("model.txt", 'w')
cat(modelstring, file=output,sep="")  
close(output)


## Write data and script and init files

output <- file("data.txt", 'w')

cat(datastring, file=output,sep="")  
close(output)

scriptstring <- "model in <\"model.txt\">\n"
if(!is.na(data)) scriptstring <- paste(scriptstring, "data in <\"data.txt\">\n", sep="")

scriptstring <- paste(scriptstring, "compile, nchains(<", as.integer(chains), ">)\n", sep="")
for(i in 1:chains){
	if(!is.na(inits[i])) scriptstring <- paste(scriptstring, "parameters in <\"inits", i, ".txt\">, chain(<", i, ">)\n", sep="")
}
scriptstring <- paste(scriptstring, "initialize\n", sep="")
if(adapt > 0){
	scriptstring <- paste(scriptstring, "adapt <", adapt.runs, ">\n", sep="")
}
if(burnin > 0 | adapt <= 0){
	scriptstring <- paste(scriptstring, "update <", ini.runs, ">\n", sep="")
}
scriptstring <- paste(scriptstring, monitors, "update <", real.runs, ">
coda <*>\n", sep="")
for(i in 1:chains){
scriptstring <- paste(scriptstring, "parameters to <\"out", i, ".Rdump\">, chain(<", i, ">)\n", sep="")
}
scriptstring <- paste(scriptstring, "exit\n", sep="")

output <- file("script.cmd", 'w')
cat(scriptstring, file=output,sep="")  
close(output)

for(i in 1:chains){
	output <- file(paste("inits", i, ".txt", sep=""), 'w')
	cat(initstring[i], file=output,sep="")
	close(output)
}

cat("Calling the simulation... (this may take some time)\n")

jags.status <- testjags(jags, silent=TRUE)

jags <- jags.status$JAGS.path

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

if (file.exists("CODAindex.txt") == FALSE){
  	if (file.exists("JAGS.out") == TRUE){
 		cat("You are using a version of JAGS prior to 0.99.0, which is no longer supported.  Please update JAGS and try again\n")
   		stop("JAGS version not supported")
   	}else{
   		cat("ERROR:  The coda files were not found\n")
   		suppressWarnings(try(cat(success, "\n", sep=""), silent=TRUE))
   		setwd(save.directory)
		unlink(temp.directory, recursive = TRUE)
		results <- c("Unable to load coda files")
		return(results)
   	}
}


if(silent.jags == FALSE){
	for(i in 1:3000000) {
		hold <- exp(100)
	}
	cat("\n")
}

cat("Simulation complete.  Reading coda files...\n")

suppressWarnings(inputsuccess <- try(input.data <- read.openbugs(quiet=TRUE), silent=TRUE))

if((class(inputsuccess)=="try-error")){
	filename <- paste("jags.dump", 1, ".R", sep="")
	suppressWarnings(try(inputsuccess <- try(tempinput <- readLines(filename)), silent=TRUE))
	if(class(inputsuccess)=="try-error"){
		
		setwd(save.directory)
		unlink(temp.directory, recursive = TRUE)
		cat("Unable to load coda files or output of a crashed model.  The model may not have compiled correctly, or the monitored nodes may not exist.  Also check that the initial values or prior distributions given do not conflict with the data.\n")
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

# stuff removed from here for unequal chain lengths

	
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


setwd(save.directory)
unlink(temp.directory, recursive = TRUE)

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


if(check.conv==TRUE & achieved!=0){
	success <- try({
	mcmc <- vector('list', length=chains)

	for(i in 1:chains){
		mcmc[[i]] <-input.data[[i]][,1:n.params]
	}
	
	if(n.chains > 1){
		convergence <- gelman.diag(mcmc.list(mcmc))
	}else{	
		convergence <- "Convergence cannot be assessed using only 1 chain"
		unconverged <- 0
		param.conv <- 1
	}
	autocorrelation <- autocorr.diag(mcmc.list(mcmc))

	autocorrelated <- 0
	unconverged <- 0

	for(j in 1:n.params){
		if(n.chains > 1){
			param.conv <- convergence$psrf[j, 1]
			if(!is.na(param.conv)){
				if(param.conv > 1.05){
					unconverged <- unconverged + 1
				}
			}
		}
		param.autocorr <- autocorrelation[3,j]
		if(!is.na(param.autocorr)){
			if(param.autocorr > 0.1){
				autocorrelated <- autocorrelated + 1
			}
		}
	
	}
	
	if(!is.na(param.conv)){
		if(unconverged > 0){
			if(n.params==1) cat("Convergence failed for this run after ", updates, " iterations (psrf = ", round(convergence$psrf[1,1], digits=3), ")\n", sep="") else cat("Convergence failed for this run for ", unconverged, " parameter(s) after ", updates, " iterations (multi-variate psrf = ", round(convergence$mpsrf, digits=3), ")\n", sep="")
		}else{
			if(n.chains > 0) cat("The Gelman-Rubin statistic is below 1.05 for all parameters\n")
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
	}
	
	tsummary <- suppressWarnings(summary(input.data))
	
		if(crashed==TRUE){
			return(list(mcmc=input.data, crash.end=unlist(crash.end), burnin=burnin+adapt, sample=achieved, summary=tsummary, psrf=convergence, autocorr=autocorrelation))
		}else{
			return(list(mcmc=input.data, end.state=unlist(input.end), burnin=burnin+adapt, sample=sample, summary=tsummary, psrf=convergence, autocorr=autocorrelation))
		}
	
}else{

	if(crashed==TRUE){
		return(list(mcmc=input.data, crash.end=unlist(crash.end), burnin=burnin+adapt, sample=achieved))
	}else{
		return(list(mcmc=input.data, end.state=unlist(input.end), burnin=burnin+adapt, sample=sample))
	}

}
}

run.JAGS <- run.jags