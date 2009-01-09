run.jagsfile <- function(path=stop("No path or model string supplied"), datalist = NA, initlist = NA, n.chains=NA, data=NA, model=NA, inits=NA, monitor=NA, call.jags=TRUE, autorun=FALSE, ...){

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
	
	findparams <- function(parameter, inputlist, environment=1, chain=1){

		if(class(inputlist)=="function"){
			success <- suppressWarnings(try(inputlist <- inputlist(chain), silent=TRUE))
			if(class(success)=="try-error") inputlist <- inputlist()
		}

		if(identical(list(), inputlist) | identical(list(list()), inputlist)) inputlist <- list("")

		names <- names(inputlist)

		value <- list()
		
		for(i in 1:length(parameter)){	

			if(any(names==parameter[i])){

				value <- c(value, (inputlist[names==parameter[i]]))
			}else{
				
				suppressWarnings(success <- try(temp <- get(parameter[i], sys.frame(sys.parent(n=environment))), silent=TRUE))
				if(class(success)!="try-error"){
					if(class(temp)=="function"){
						success <- suppressWarnings(try(temp <- temp(chain), silent=TRUE))
						if(class(success)=="try-error") temp <- temp()
					}
				}else{
					suppressWarnings(success <- try(temp <- get(parameter[i], pos=".GlobalEnv"), silent=TRUE))
					if(class(success)!="try-error"){
						if(class(temp)=="function"){
							success <- suppressWarnings(try(temp <- temp(chain), silent=TRUE))
							if(class(success)=="try-error") temp <- temp()
						}
					}else{
						temp <- NA
					}
				}

				if(any(is.na(temp))) stop(paste(parameter[i], "not found")) 

				value[[i]] <- temp
				
			}

			names(value)[[i]] <- parameter[i]

		}


		return(value)

		}
	fromautorun <- 3
	
	if(all(!is.na(inits))){
		if(inits[1]=="fromautorun"){
			fromautorun <- 4
			inits <- NA
		}
	}
	if(all(!is.na(data))){
		if(data[1]=="fromautorun"){
			fromautorun <- 4
			data <- NA
		}
	}
	
	if(is.na(n.chains) & all(!is.na(inits))) n.chains <- length(inits)
	
	params <- read.winbugs(path)
	
	autodata <- params$autodata
	autoinits <- params$autoinits
	maindata <- params$data
	maininits <- params$inits

	if(is.na(data)){
		if(!is.null(autodata)){
			autodata <- findparams(autodata, datalist, fromautorun)
			maindata <- paste(maindata, dump.format(autodata), sep="\n")
			params$data <- maindata
		}
		if(maindata=="") maindata <- NA
		params$data <- as.character(maindata)
	}else{
		params$data <- data
	}
	
	if(all(is.na(inits))){
		if(all(maininits=="")){
			if(is.na(n.chains)){
				n.chains <- 2
				warning("No initial value blocks found and n.chains not specified.  2 chains will be used.")
			}
			maininits <- character(length=n.chains)
		}else{
			if(is.na(n.chains)) n.chains <- length(maininits)
			if(length(maininits)==1) maininits <- replicate(n.chains, maininits)
			if(length(maininits)!=n.chains) warning("Number of initial value blocks found does not correspond to the number of chains specified.  The latter will be ignored.")

		}
		if(!is.null(autoinits)){
			for(i in 1:n.chains){
				geninits <- findparams(autoinits, initlist, fromautorun, chain=i)
				maininits[i] <- paste(maininits[i], dump.format(geninits), sep="\n")
			}
		}
		params$inits <- maininits
	}else{
		params$inits <- inits
	}
	
	params$inits[params$inits==""] <- NA
	
	if(!is.na(n.chains) & length(params$inits)==0) params$inits <- NA
	
	if(length(params$inits)==0 & is.na(n.chains)) stop("No initial value blocks or tags were found and n.chains was not specified.  Either provide initial values or specify n.chains.")
	
	n.chains <- length(params$inits)
	
	lengths <- lapply(params, length)
	if(any(lengths==0)) stop(paste("No ", paste(names(lengths[lengths==0]), collapse=" or "), " blocks or tags were found", sep=""))
	
	if(!call.jags) return(list(data=params$data, model=params$model, inits=params$inits, monitor=params$monitor, n.chains=n.chains))
	
	if(any(is.null(params$monitor))) stop("No monitored variables specified")
	if(any(is.na(params$monitor))) stop("No monitored variables specified")

	if(!autorun){
		return(run.jags(data=params$data, model=params$model, inits=params$inits, monitor=params$monitor, n.chains=n.chains, ...))
	}else{
		return(autorun.jags(data=params$data, model=params$model, inits=params$inits, monitor=params$monitor, n.chains=n.chains, ...))
	}	
	
}

autorun.jagsfile <- function(path=stop("No path or model string supplied"), datalist=NA, initlist=NA, n.chains=NA, data=NA, model=NA, inits=NA, monitor=NA, call.jags=TRUE, ...){
	
	if(all(is.na(inits))) inits <- "fromautorun"
	if(all(is.na(data))) data <- "fromautorun"
	if(!is.na(n.chains)) if(n.chains < 2) stop("The number of chains must be 2 or more so that convergence can be assessed")
	return(run.jagsfile(path, datalist=datalist, initlist=initlist, n.chains=n.chains, data=data, model=model, inits=inits, monitor=monitor, autorun=TRUE, call.jags=call.jags, ...))

}

run.JAGSfile <- run.jagsfile
autorun.JAGSfile <- autorun.jagsfile

