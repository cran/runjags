print.gelman.with.target <- function(x, digits = 3, ...){
    cat("Potential scale reduction factors:\n\n")
    print.default(x$psrf, digits = digits, ...)
    if (!is.null(x$mpsrf)) {
        cat("\nMultivariate psrf\n\n")
        cat(format(x$mpsrf, digits = digits))
    }
    
    cat("\n\nTarget psrf\n\n")
    cat(format(x$psrf.target, digits = digits))
    cat("\n")
}

print.dic.stats <- function(x, digits=3, ...){
	string <- paste("Model fit statistics\n\nDeviance information criterion [mean(deviance)+mean(pd)]:  ", round(x$dic, digits=digits), "\n", sep="")
	string <- paste(string, "(Individual chains: ", paste(round(x$dic.chains, digits=digits), collapse=", "), ")\n\n", sep='')
	string <- paste(string, "Penalized Expected Deviance [mean(deviance)+sum(popt)]):  ", round(x$ped, digits=digits), "\n", sep="")
	string <- paste(string, "(Individual chains: ", paste(round(x$ped.chains, digits=digits), collapse=", "), ")\n", sep='')
	
	swcat(string)
}

print.plotindpages <- function(x, ...){
	if(!exists("dev.new")) dev.new <- x11
	dev.new()
	class(x) <- "trellis"
	print(x, ...)
}


swcat <- function(...){
	
	pargs <- list(...)
	pasted <- do.call(paste, pargs)
	pasted <- gsub('\r', '\n', pasted)
	
	# White space is destroyed by strwrap so preserve \n by splitting on them (and append a ' ' [which is removed by strwrap anyway] to preserve any trailing \n)
	pasted <- unlist(strsplit(paste(pasted,' ',sep=''), '\n'))
	pasted <- strwrap(pasted)
	cat(paste(pasted, collapse='\n'))
	
}


winbugs.extract.big <- function(find, string){
	
split <- strsplit(string, "")[[1]]

newstring <- ""

newlinelast = found = started <- FALSE
openbracket = closebracket = find.no <- 0

newlinelast <- TRUE
listfound <- logical()

for(i in 1:length(split)){
		
	if(found){
		
		if(any(split[i]==c("", " ", "\n", "\r", "\t")) & !started) next
		
		if(split[i]=="{"){
			openbracket <- openbracket + 1
			started <- TRUE
			if(openbracket==1) next
		}
		if(split[i]=="}") closebracket <- closebracket + 1
				
		if(is.na(newstring[find.no])) newstring[find.no] <- ""
		
		if(openbracket==closebracket){
			if(list){
				temp <- rev(strsplit(newstring[find.no], "")[[1]])
				temp[which(temp==")")[1]] <- ""
				newstring[find.no] <- paste(rev(temp), collapse="")
			}
			newstring[find.no] <- paste(newstring[find.no], "\n", sep="")
			found <- FALSE
		}else{
			newstring[find.no] <- paste(newstring[find.no], split[i], sep="")
		}
	}
	
	if(paste(split[i:(i-1+length(strsplit(find, "")[[1]]))], collapse="") == find & newlinelast==TRUE){

		remaining <- split[(i+length(strsplit(find, "")[[1]])):length(split)]
		
		if(remaining[remaining!=" " & remaining!="\t" & remaining!="\n" & remaining!="\r"][1]=="{"){
						
			split[i:(i-1+length(strsplit(find, "")[[1]]))] <- ""
			
			if(paste(remaining[remaining!=" " & remaining!="\t" & remaining!="\n" & remaining!="\r" & remaining!=""][2:6], collapse="") =="list("){
				remaining[remaining!=" " & remaining!="\t" & remaining!="\n" & remaining!="\r"][2:6] <- ""
				split[(i+length(strsplit(find, "")[[1]])):length(split)] <- remaining
				list <- TRUE				
			}else{
				list <- FALSE
			}
			
			listfound <- c(listfound, list)
			
			started <- FALSE
			found <- TRUE
			find.no <- find.no + 1
			openbracket = closebracket = 0
		}		
			
		
	}
	
	if(!all(split[i]==c("", " ", "\t", "	"))){
		if(any(split[i]==c("\n", "\r"))){
			newlinelast <- TRUE
		}else{
			newlinelast <- FALSE
			}
	}
}

if(all(newstring=="")) listfound <- FALSE
return(list(newstring, listfound=listfound))
}





winbugs.extract.small <- function(find, string){
	
split <- strsplit(string, "")[[1]]

newstring <- ""

newlinelast = found <- FALSE
find.no = hash <- 0

for(i in 1:length(split)){
		
	if(found){
		
		if(split[i]=="#"){
			hash <- hash + 1
			next
		}

		if(hash!=2) next
		
		if(any(split[i]==c(",", ";", ":", "&"))){
			
			find.no <- find.no + 1
			next
		}
		
		if(is.na(newstring[find.no])) newstring[find.no] <- ""
		
		if(any(split[i]==c("", " ", "\t", "@", "%"))) next
		if(any(split[i]==c("\n", "\r"))){
			found <- FALSE
		}else{
			newstring[find.no] <- paste(newstring[find.no], split[i], sep="")
		}
				
	}

	if(!all(split[i]==c("", " ", "\t"))){
		temp <- split[i:length(split)]
		temp <- temp[temp!=" " & temp!= "" & temp!="\t"]
		if(paste(temp[1:(length(strsplit(find, "")[[1]]))], collapse="") == find){ # newlinelast not necessary for extract.small
			found <- TRUE
			hash <- 1
			split[min(which(split=="#")[which(split=="#")>=i])] <- ""
			find.no <- find.no + 1
		}
	
	
		if(any(split[i]==c("\n", "\r"))){
			newlinelast <- TRUE
		}else{
			newlinelast <- FALSE
			}
	}

}
for(i in 1:length(newstring)){
	temp <- strsplit(newstring[i], "")[[1]]
	temp[temp=="="] <- "<-"
	newstring[i] <- paste(temp, collapse="")
}
return(newstring[newstring!="" & newstring!=" "])
}



find.parameters <- function(parameter, inputlist, environment=1, chain=1){

	if(class(inputlist)=="function"){
		success <- suppressWarnings(try(inputlist <- inputlist(chain), silent=TRUE))
		if(class(success)=="try-error") inputlist <- inputlist()
	}

	if(identical(list(), inputlist) | identical(list(list()), inputlist)) inputlist <- list("")

	names <- names(inputlist)
	value <- vector('list', length=length(parameter))

	for(i in 1:length(parameter)){	

		if(any(names==parameter[i])){
			temp <- inputlist[names==parameter[i]]
			
			if(class(temp)=="function"){
				success <- suppressWarnings(try(temp <- temp(chain), silent=TRUE))
				if(class(success)=="try-error") temp <- temp()
			}
			
		}else{
			
			suppressWarnings(success <- try(temp <- get(parameter[i], sys.frame(sys.parent(n=environment))), silent=TRUE))

			if(class(success)!="try-error"){
				if(class(temp)=="function"){
					success <- suppressWarnings(try(temp <- temp(chain), silent=TRUE))
					if(class(success)=="try-error") temp <- temp()
				}
			}else{
				suppressWarnings(success <- try(temp <- get(parameter[i]), silent=TRUE)) #, pos=".GlobalEnv"
				if(class(success)!="try-error"){
					if(class(temp)=="function"){
						success <- suppressWarnings(try(temp <- temp(chain), silent=TRUE))
						if(class(success)=="try-error") temp <- temp()
					}
				}else{
					temp <- NULL
				}
			}

			if(is.null(temp)) stop(paste(parameter[i], "not found (or has/returns value NULL)")) 
			
		}
		
		# necessary to remove compound listing somehow introduced by initlist function or something:
		while(class(temp)=="list") temp <- temp[[1]]
		
		value[[i]] <- temp
		names(value)[[i]] <- parameter[i]

	}
	
	value <<- value
	return(value)

}

normalise.mcmcfun <- function(mcmc.list, normalise = TRUE, warn = TRUE, check.stochastic = TRUE){

if(class(mcmc.list)=="mcmc") mcmc <- mcmc.list(mcmc.list) else mcmc <- mcmc.list

if(class(mcmc)!="mcmc.list") stop("Object to be normalised must be an mcmc list or mcmc object")

usevec <- 1:nvar(mcmc)

parnames <- dimnames(mcmc[[1]])[[2]][!is.na(usevec)]	

if(check.stochastic){

anydone <- FALSE

for(i in 1:nvar(mcmc)){
	testa <- if(nvar(mcmc)==1) mcmc[[1]][2] else mcmc[[1]][1,i]
	testb <- if(nvar(mcmc)==1) mcmc[[1]][1] else mcmc[[1]][2,i]
	if(testa==testb){
	values <- unlist(mcmc[,i])
	if(all(values==values[1])){
		if(!anydone){
			anydone <- TRUE
			if(warn==TRUE) swcat("\n")
		}
		usevec[i] <- NA
		if(warn==TRUE) swcat(paste("*WARNING* The monitored variable '", parnames[i], "' appears to be non-stochastic.  It will not be included in the convergence diagnostic\n", sep=""))
		if(warn=="warning") warning(paste("The monitored variable '", parnames[i], "' appears to be non-stochastic.  It will not be included in the convergence diagnostic", sep=""))
	}}
}
if(anydone & warn==TRUE) swcat("\n")
if(anydone){
	new.mcmc <- vector('list', length=nchain(mcmc))

	for(i in 1:nchain(mcmc)){
		new.mcmc[[i]] <- mcmc(matrix(mcmc[[i]][,na.omit(usevec)], ncol=length(na.omit(usevec)), dimnames=list(1:niter(mcmc), dimnames(mcmc[[1]])[[2]][!is.na(usevec)])))
	}

	class(new.mcmc) <- "mcmc.list"

	mcmc <- new.mcmc
}
}

success <- try({
if(normalise){
	
	if(niter(mcmc)>1000) use <- sample(1:niter(mcmc), size=1000, replace=FALSE) else use <- 1:niter(mcmc)
	shap.res <- apply(combine.mcmc(mcmc, collapse.chains=TRUE), 2, function(x){
		if(all(x==x[1])){
			return(1)
		}else{
			if(all(x > 0)){
				if(all(x < 1)){
					if(shapiro.test(x[use])$statistic > shapiro.test(log(x[use]/(1-x[use])))$statistic) return(3) else return(1)
				}else{
					if(shapiro.test(x[use])$statistic > shapiro.test(log(x[use]))$statistic) return(2) else return(1)
				}
			}else{
				return(1)
			}
		}})
	
	change <- which(shap.res!=1)
	for(parameter in change){
		for(chain in 1:nchain(mcmc)){
			newvalues <- unlist(mcmc[[chain]][,parameter])
			if(shap.res[parameter]==3) newvalues <- log(newvalues/(1-newvalues)) else newvalues <- log(newvalues)
			mcmc[[chain]][,parameter] <- newvalues
		}
	}


	# Really slow code:
	
	#for(parameter in 1:nvar(mcmc)){
		
	#	data <- unlist(mcmc[,parameter])

	#	if(!all(data > 0)) next

	
	#	if(all(data > 0) & all(data < 1)){
	#		logdat <- log(data/(1-data))
	#		logit <- TRUE
	#	}else{
	#		logdat <- log(data)
	#		logit <- FALSE
	#	}
		
		# having problems with some probabilites coming out at Inf, although this should be fixed now I am checking all values not just the thinned ones, but it doesn't do any harm to leave so:
	
	#	if(any(is.na(logdat))) next
	#	if(any(logdat==Inf) | any(logdat==-Inf)) next


		# sample for max length of shapiro.test:
		
	#	done <- 0
		
	#	alldata <- data
	#	alllogdat <- logdat
		
		# sometimes shapiro.test returns an error for the normal data (if very skewed?)
		# repeating it up to 100 times should be OK
		
	#	while(done < 100){
		
	#		if(length(alldata) > 1000) data <- sample(alldata, 1000)
	#		if(length(alllogdat) > 1000) logdat <- sample(alllogdat, 1000)
	#	
	#		norm.s <- shapiro.test(data)$statistic
	#		log.s <- shapiro.test(logdat)$statistic
			
	#		done <- done + 1
	#		
	#		if(!is.na(norm.s) & !is.na(log.s)) break
	#	}
		
	#	if(done==100){
			# in case repeating up to 100 times isn't OK
	#		if(warn) warning(paste("Attempt to normalise mcmc chain failed for the parameter '", varnames(mcmc)[parameter], "'", sep=""))
	#		next
	#	}
		
	#	if(norm.s >= log.s){
	#		use <- 1	
	#	}else{
	#		use <- if(logit) 3 else 2
	#	}
		
		# for bug testing log transformations:
		#use <- if(logit) 3 else 2
		
	#	if(use!=1){
	#		for(chain in 1:nchain(mcmc)){
	#			newvalues <- unlist(mcmc[[chain]][,parameter])
	#			if(logit) newvalues <- log(newvalues/(1-newvalues)) else newvalues <- log(newvalues)
	#			mcmc[[chain]][,parameter] <- newvalues
				
				
	#		}
	#	}
	#}
}
})

if(class(success)=="try-error"){
	#print("TEMP")
	#name <- new_unique("normalise.failed", ".Rsave")
	#save(failedmcmc, file=name)
	stop("An error occured while normalising the mcmc chain")
}

if(class(mcmc.list)=="mcmc") return(mcmc[[1]]) else return(mcmc)

}

safe.autocorr.diag <- function(x, ...){
	y <- autocorr.diag(x[,1])
	if(nvar(x)>1) for(i in 2:nvar(x)) y <- cbind(y, autocorr.diag(x[,i]))
	dimnames(y)[[2]] <- dimnames(x[[1]])[[2]]
	return(y)
}

safe.gelman.diag <- function(x, warn=TRUE,...){

	success <- try(gelman <- gelman.diag(x, ...), silent=TRUE)
	if(class(success)=="try-error"){
		
		nvars <- nvar(x)
		psrfs <- matrix(ncol=2, nrow=nvars, dimnames=list(varnames(x), c("Point est.", "97.5% quantile")))
		
		success <- try({
		
		for(i in 1:nvars){
			psrfs[i,] <- gelman.diag(x[,i], ...)$psrf
		}
		}, silent=TRUE)
		
		if(class(success)=="try-error"){
			#print("TEMP")
			#name <- new_unique("gelman.failed", ".Rsave")
			#save(failedmcmc, file=name)
			stop("An error occured while calculating the Gelman-Rubin statistic")
		}
		
		if(warn) swcat("Note:  Unable to calculate the multivariate psrf due to an error calculating the Gelman-Rubin statistic\n")
		
		
		
		y <- list(psrf=psrfs, mpsrf="Unable to calculate multivariate psrf")
		
		class(y) <- "gelman.diag"
		return(y)
	}else{
		return(gelman)
	}
	
}


xgrid.retrieve <- function(jobnum, wait, wait.interval, silent, cleanup, directory, partialretrieve, jags=FALSE){
	
	if(length(jobnum) > 1) separatejobs <- TRUE else separatejobs <- FALSE
	nsims <- length(jobnum)
		
	if(separatejobs){
		
		status <- character(nsims)
		done <- replicate(nsims, FALSE)
		for(s in 1:nsims){
			statusout <- system(paste('xgrid -job attributes -id ', jobnum[s], sep=''), intern=TRUE)
			if(paste(statusout, collapse='')=="{    error = InvalidJobIdentifier;}"){
				if(wait){
					swcat("Error:  One of the job ids was not found on xgrid.  This can sometimes occur when a job is being initialised - waiting to try again in one minute...\n")
					flush.console()
					Sys.sleep(60)
					statusout <- system(paste('xgrid -job attributes -id ', jobnum, sep=''), intern=TRUE)
					if(paste(statusout, collapse='')=="{    error = InvalidJobIdentifier;}") stop("One or more of the job ids was still not found.  The job may have been deletd from xgrid")
				}else{
					stop("One of the jobs specified is not on xgrid.  This can sometimes occur when a job is being initialised - you could try again in a minute or so...")
				}
			}
			tstatus <- statusout[grep('jobStatus', statusout)]
			status[s] <- gsub('[[:space:]]', '', gsub(';', '', gsub('jobStatus = ', '', tstatus)))
			if(any(status[s]==c('Finished', 'Canceled', 'Failed'))) done[s] <- TRUE
		}
		
		if(!wait & !all(done) & !partialretrieve){
			if(!silent){
				swcat('The job outputs (if any) are shown below:\n')
				for(s in 1:nsims){
					system(paste('xgrid -job results -id ', jobnum[s], sep=''))
					swcat('\n')
				}
				swcat('Jobs not finished.  Statuses are "', paste(status, collapse=', '), '"\n', sep='')
			}
			stop(paste('Jobs not finished.  Statuses are "', paste(status, collapse=', '), '"', sep=''))
		}
		
		if(wait & !all(done)){
		swcat('Job statuses at ', format(Sys.time(), "%a %b %d %H:%M:%S"), ' were "', paste(status, collapse=', '), '".  Will try again on ', format(Sys.time()+wait.interval, "%b %d %H:%M"), '\n', sep='')

		repeat{
			assign('xgrid.waiting', TRUE, envir=parent.frame())
			flush.console()
			Sys.sleep(wait.interval)
			assign('xgrid.waiting', FALSE, envir=parent.frame())
			for(s in 1:nsims){
				statusout <- system(paste('xgrid -job attributes -id ', jobnum[s], sep=''), intern=TRUE)
				if(paste(statusout, collapse='')=="{    error = InvalidJobIdentifier;}") stop("One or more of the jobs has been deleted from xgrid")
				tstatus <- statusout[grep('jobStatus', statusout)]
				status[s] <- gsub('[[:space:]]', '', gsub(';', '', gsub('jobStatus = ', '', tstatus)))
				if(any(status[s]==c('Finished', 'Canceled', 'Failed'))) done[s] <- TRUE
			}
			
			if(all(done)) break
			swcat('Job statuses at ', format(Sys.time(), "%a %b %d %H:%M:%S"), ' were "', paste(status, collapse=', '), '".  Will try again on ', format(Sys.time()+wait.interval, "%b %d %H:%M"), '\n', sep='')
		}
		}

		if(all(status=='Finished')){
			swcat('The xgrid jobs have finished\n')
		}else{
			swcat('The xgrid jobs are showing the statuses "', paste(status, collapse=', '), '"\n', sep="")
			silent <- FALSE
		}
		
		xgridoutput <- vector('list', length=nsims)
		for(s in 1:nsims){
			xgridoutput[[s]] <- c(paste('\n', if(jags) 'Chain' else 'Task', ' ', s, ':', sep=''), system(paste('xgrid -job results -id ', jobnum[s], ' -out "', directory, if(jags) '/sim.', if(jags) s, '"', sep=''), intern=TRUE))
			if(length(xgridoutput[[s]])==0 & jags) stop(paste("The job produced no output for chain ", s, "; ensure that the jagspath supplied is accurate", sep=''))
		}

		swcat("Job was successfully retreived from xgrid\n")
		if(!silent)	cat('\nThe xgrid output is displayed below:\n', unlist(xgridoutput), sep='\n')
		if(cleanup){
			for(s in 1:nsims){
				xgriddeleteout <- system(paste('xgrid -job delete -id ', jobnum[s], sep=''), intern=TRUE)
				if(paste(xgriddeleteout, collapse='')!='{}') warning(paste('Possible error deleting xgrid job number ', jobnum[s], ' - please check this manually', sep=''))
			}
		}
		
	}else{
		
		statusout <- system(paste('xgrid -job attributes -id ', jobnum, sep=''), intern=TRUE)
		if(paste(statusout, collapse='')=="{    error = InvalidJobIdentifier;}"){
			if(wait){
				swcat("Error:  The job id was not found on xgrid.  This can sometimes occur when a job is being initialised - waiting to try again in one minute...\n")
				flush.console()
				Sys.sleep(60)
				statusout <- system(paste('xgrid -job attributes -id ', jobnum, sep=''), intern=TRUE)
				if(paste(statusout, collapse='')=="{    error = InvalidJobIdentifier;}") stop("The job id was still not found.  The job specified may have been deleted.")
			}else{
				stop("The job specified is not on xgrid.  This can sometimes occur when a job is being initialised - you could try again in a minute or so...")
			}
		}
		status <- statusout[grep('jobStatus', statusout)]
		status <- gsub('[[:space:]]', '', gsub(';', '', gsub('jobStatus = ', '', status)))
		
		if(!wait & !status=='Finished' & !partialretrieve){
			if(!silent){
				swcat('The job output (if any) is shown below:\n')
				system(paste('xgrid -job results -id ', jobnum, sep=''))
				swcat('\nJob not finished.  Status is "', status, '"\n', sep='')
			}
			stop(paste('Job not finished.  Status is "', status, '"', sep=''))
		}
		
		if(!any(status==c('Finished', 'Canceled', 'Failed')) & wait){
		swcat('Job status at ', format(Sys.time(), "%a %b %d %H:%M:%S"), ' was "', status, '".  Will try again on ', format(Sys.time()+wait.interval, "%b %d %H:%M"), '\n', sep='')
		
		repeat{
			assign('xgrid.waiting', TRUE, envir=parent.frame())
			flush.console()
			Sys.sleep(wait.interval)
			assign('xgrid.waiting', FALSE, envir=parent.frame())
			statusout <- system(paste('xgrid -job attributes -id ', jobnum, sep=''), intern=TRUE)
			if(paste(statusout, collapse='')=="{    error = InvalidJobIdentifier;}") stop("The job has been deleted from xgrid")
			status <- statusout[grep('jobStatus', statusout)]
			status <- gsub('[[:space:]]', '', gsub(';', '', gsub('jobStatus = ', '', status)))
		
			if(any(status==c('Finished', 'Canceled', 'Failed'))) break
			swcat('Job status at ', format(Sys.time(), "%a %b %d %H:%M:%S"), ' was "', status, '".  Will try again on ', format(Sys.time()+wait.interval, "%b %d %H:%M"), '\n', sep='')
		}
		}
	
		if(status=='Finished'){
			swcat('The xgrid job has finished\n')
		}else{
			swcat('The xgrid job is showing the status "', status, '"\n', sep="")
			silent <- FALSE
		}
		xgridoutput <- system(paste('xgrid -job results -id ', jobnum, ' -out "', directory, '"', sep=''), intern=TRUE)
		
		swcat("Job was successfully retreived from xgrid\n")
		if(!silent){
			# Note not using swcat for xgrid output!
			if(length(xgridoutput)==0) swcat('\nThe xgrid job did not produce any output to screen\n') else cat('\nThe xgrid output is displayed below:\n', xgridoutput, sep='\n')
		}
		
		if(cleanup){
			xgriddeleteout <- system(paste('xgrid -job delete -id ', jobnum, sep=''), intern=TRUE)
			if(paste(xgriddeleteout, collapse='')!='{}') warning(paste('Possible error deleting xgrid job number ', jobnum, ' - please check this manually', sep=''))
		}
		
	}
	
	return(xgridoutput)
}
