run.jags <- function(model=stop("No model supplied"), monitor = stop("No monitored variables supplied"), data=NA,  n.chains=2, inits = replicate(n.chains, NA), burnin = 5000*thin, sample = 10000*thin, adapt=if(burnin<200) 100 else 0, jags = findjags(), silent.jags = FALSE, check.conv = TRUE, plots = TRUE, psrf.target = 1.05, normalise.mcmc = TRUE, check.stochastic = TRUE, modules=c(""), thin = 1, monitor.deviance = FALSE, monitor.pd = FALSE, monitor.popt = FALSE, keep.jags.files = FALSE){

updates <- sample

if(any(c(monitor.pd, monitor.popt)) & n.chains < 2){
	warning("The pD and popt cannot be assessed with only 1 chain")
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
if(test[[2]][1]==FALSE){
	cat("Unable to call JAGS using '", jags, "' - try specifying the path to the JAGS binary as the jags argument\n", sep="")
	stop("Unable to call JAGS")
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

scriptstring <- ""

if(any(modules!="")){
	for(i in 1:length(modules)){
		scriptstring <- paste(scriptstring, "load <", modules[i], ">\n", sep="")
	}
}

scriptstring <- paste(scriptstring, "model in <\"model.txt\">\n", sep="")
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
scriptstring <- paste(scriptstring, monitors, if(monitor.deviance) paste("monitor, type(deviance) thin(", thin, ")\n", sep=""), if(monitor.pd) paste("monitor, type(pD) thin(", thin, ")\n", sep=""), if(monitor.popt) paste("monitor, type(popt) thin(", thin, ")\n", sep=""), "update <", real.runs, ">
coda <*>\n", sep="")
for(i in 1:chains){
scriptstring <- paste(scriptstring, "parameters to <\"out", i, ".Rdump\">, chain(<", i, ">)\n", sep="")
}
if(monitor.deviance) scriptstring <- paste(scriptstring, "monitors to <\"deviance.Rdump\">, type(deviance)\n", sep="")
if(monitor.pd) scriptstring <- paste(scriptstring, "monitors to <\"pd.Rdump\">, type(pD)\n", sep="")
if(monitor.popt) scriptstring <- paste(scriptstring, "monitors to <\"popt.Rdump\">, type(popt)\n", sep="")

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
		if(!keep.jags.files) unlink(temp.directory, recursive = TRUE)
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
		
		setwd(save.directory)
		if(!keep.jags.files) unlink(temp.directory, recursive = TRUE)
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


setwd(save.directory)
if(!keep.jags.files) unlink(temp.directory, recursive = TRUE)

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

if(plots==TRUE & achieved!=0){
	success <- try({
	final.mcmc <- input.data
	plot1 = plot2 = vector('list', length=length(varnames(final.mcmc)))
	names(plot1) = names(plot2) <- varnames(final.mcmc)
	thinned.mcmc <- combine.mcmc(list(final.mcmc), return.samples=1000)

	#startdev <- dev.list()

	#a <- dev.new()
	#if(options("device")$device=="x11") x11()

	for(i in 1:length(varnames(final.mcmc))){

		plotdata <- thinned.mcmc[,c(i,i)]
		varnames(plotdata)[1] <- ""		
		plot1[[i]] <- xyplot(plotdata, layout=c(1,1), ylab="Value", xlab="Iteration")
		class(plot1[[i]]) <- "plotindpages"
		plot2[[i]] <- densityplot(plotdata, layout=c(1,1), ylab="Density", xlab="Value")
		class(plot2[[i]]) <- "plotindpages"

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
	
	if(class(convergence$mpsrf)!="numeric"){
		mpsrfstring <- " (Unable to calculate the multi-variate psrf)"
	}else{
		mpsrfstring <- paste(" (multi-variate psrf = ", round(convergence$mpsrf, digits=3), ")", sep="")
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
	suppressWarnings(tsummary <- summary(combine.mcmc(input.data, collapse.chains=TRUE)))
	options(show.error.messages = TRUE)	
	
	if(crashed==TRUE){
		return(c(list(mcmc=input.data, crash.end=unlist(crash.end), burnin=burnin+adapt, sample=achieved, thin=thin,  summary=tsummary, psrf=convergence, autocorr=autocorrelation, trace=plot1, density=plot2), otheroutputs))
	}else{
		return(c(list(mcmc=input.data, end.state=unlist(input.end), burnin=burnin+adapt, sample=sample, thin=thin, summary=tsummary, psrf=convergence, autocorr=autocorrelation, trace=plot1, density=plot2), otheroutputs))
	}
	
}else{

	if(crashed==TRUE){
		return(c(list(mcmc=input.data, crash.end=unlist(crash.end), burnin=burnin+adapt, sample=achieved, thin=thin, summary="Summary not produced when check.conv==FALSE", psrf="Potential scale reductionf factors not produced when check.conv==FALSE", autocorr="Autocorrelation statistics not produced when check.conv==FALSE", trace=plot1, density=plot2), otheroutputs))
	}else{
		return(c(list(mcmc=input.data, end.state=unlist(input.end), burnin=burnin+adapt, sample=sample, thin=thin, summary="Summary not produced when check.conv==FALSE", psrf="Potential scale reductionf factors not produced when check.conv==FALSE", autocorr="Autocorrelation statistics not produced when check.conv==FALSE", trace=plot1, density=plot2), otheroutputs))
	}

}
}

run.JAGS <- run.jags