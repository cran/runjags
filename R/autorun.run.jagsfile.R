run.jagsfile <- function(path=stop("No path or model string supplied"), datalist = NA, initlist = NA, n.chains=NA, data=NA, model=NA, inits=NA, monitor=NA, call.jags=TRUE, autorun=FALSE, ...){

	if(class(data)=="list") suppressWarnings(try(data <- dump.format(data), silent=TRUE))

	suppressWarnings(if(is.na(data)) class(data) <- "character")
	
	if(!is.na(n.chains)) if(n.chains < 2 & autorun) stop("The number of chains must be 2 or more so that convergence can be assessed")
	
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
	
	fromautorun <- 3
	
	if(all(!is.na(inits))){
		if(any(inits[1]==c("fromautorun", "fromxgrid"))){
			fromautorun <- 4
			inits <- NA
		}
	}
	if(all(!is.na(data))){
		if(any(data[1]==c("fromautorun", "fromxgrid"))){
			fromautorun <- 4
			data <- NA
		}
	}
	
	if(is.na(n.chains) & any(!is.na(inits))) n.chains <- length(inits)
	
	params <- read.winbugs(path)
	
	autodata <- params$autodata
	autoinits <- params$autoinits
	maindata <- params$data
	maininits <- params$inits
	
	if(is.na(model)){
		if(params$model=="model{\n\n}\n") stop("No valid model was specified or found in the model block")
		outmodel <- params$model
	}else{
		outmodel <- model
		if(params$model!="model{\n\n}\n") warning("A model was found in the model block but will be ignored since a model was specified in the arguments for (auto)run.jagsfile")
	}
	
	if(is.na(data)){
		if(all(is.na(maindata))) maindata <- ""
		outdata <- as.character(maindata)
		
		if(!all(is.na(autodata))){
			autodata <- find.parameters(autodata, datalist, fromautorun)
			outdata <- paste(outdata, dump.format(autodata), sep="\n")
		}
	}else{
		if(!identical(autodata, NA) | !identical(maindata, NA)) warning("Data was specified in the model block but will be ignored since data was specified in the arguments for (auto)run.jagsfile")
		outdata <- data
	}
	
	if(all(is.na(inits))){
		if(all(is.na(maininits))){
			if(is.na(n.chains)){
				n.chains <- 2
				warning("No initial value blocks found and n.chains not specified.  2 chains were used.")
			}
			outinits <- character(length=n.chains)
		}else{
			if(is.na(n.chains)) n.chains <- length(maininits)
			if(length(maininits)!=n.chains){
				warning("The number of initial value blocks found does not correspond to the number of chains specified.  Some initial values were recycled or ignored.")
				
				temp <- maininits
				outinits <- character(n.chains)

				suppressWarnings(outinits[] <- temp)
				
			}else{
				outinits <- maininits
			}

		}
		if(!all(is.na(autoinits))){
			for(i in 1:n.chains){
				geninits <- find.parameters(autoinits, initlist, fromautorun, chain=i)
				outinits[i] <- paste(outinits[i], dump.format(geninits), sep="\n")
			}
		}
	}else{
		if(!identical(autoinits, NA) | !identical(maininits, NA)) warning("Initial values were specified in the model block but will be ignored since initial values were specified in the arguments for (auto)run.jagsfile")
		outinits <- inits
	}
	
	outinits[outinits==""] <- NA
	
	if(outdata==""){
		outdata <- NA
		warning("The model was run without data since no data was provided or found in the model block.")
	}
		
	if(!all(is.na(monitor))){
		if(!identical(params$monitor, NA)) warning("Monitors were specified in the model block but will be ignored since monitors were specified in the arguments for (auto)run.jagsfile")
		outmonitor <- monitor
	}else{
		outmonitor <- params$monitor
	}
	
	outmonitor <- as.character(outmonitor)
	outmonitor[outmonitor==""] <- NA
	outmonitor <- outmonitor[!is.na(outmonitor)]
	
	if(length(outmonitor)==0) stop("No monitors were specified or found in the model block.")
	
	if(is.na(n.chains)) n.chains <- length(outinits)
	
	if(n.chains!=length(outinits)){
		
		temp <- outinits
		outinits <- character(n.chains)
		
		suppressWarnings(outinits[] <- temp)
		warning("The number of chains specified did not match the number of initial value strings supplied.  Some initial value strings were recycled or ignored")
	}
	
	
	lengths <- lapply(params, length)
	if(any(lengths==0)) stop(paste("No ", paste(names(lengths[lengths==0]), collapse=" or "), " blocks or tags were found", sep=""))
	
	if(!call.jags) return(list(data=outdata, model=params$model, inits=outinits, monitor=outmonitor, n.chains=n.chains))
	
	if(!autorun){
		return(run.jags(data=outdata, model=outmodel, inits=outinits, monitor=outmonitor, n.chains=n.chains, ...))
	}else{
		return(autorun.jags(data=outdata, model=outmodel, inits=outinits, monitor=outmonitor, n.chains=n.chains, ...))
	}	
	
}

autorun.jagsfile <- function(path=stop("No path or model string supplied"), datalist=NA, initlist=NA, n.chains=NA, data=NA, model=NA, inits=NA, monitor=NA, call.jags=TRUE, ...){
	
	if(all(is.na(inits))) inits <- "fromautorun"
	if(all(is.na(data))) data <- "fromautorun"
	return(run.jagsfile(path, datalist=datalist, initlist=initlist, n.chains=n.chains, data=data, model=model, inits=inits, monitor=monitor, autorun=TRUE, call.jags=call.jags, ...))

}

run.JAGSfile <- run.jagsfile
autorun.JAGSfile <- autorun.jagsfile

