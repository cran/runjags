getjagsnames <- function(targets){
	
	retval <- unlist(lapply(1:length(targets), function(i){
	
		x <- targets[[i]]
		n <- names(targets)[i]
		new <- as.list(x)

		if(length(x)==1){
			names(new) <- n
			return(new)
		}
		if(!is.array(x)){
			names(new) <- paste(n, "[",1:length(x),"]",sep="")
			return(new)
		}else{
			dims <- apply(expand.grid(lapply(dim(x), function(x) return(1:x))),1,paste,collapse=",")
			names(new) <- paste(n,"[",dims,"]",sep="")
			return(new)
		}
		stop(paste("Unsupported argument type:",class(x)))
	}))
	
	return(retval)
	
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

versionmatch <- function(required, actual){
	actual <- as.character(actual)
	
	matched <- FALSE
	for(r in as.character(required)){
		# Default:
		type <- "gteq"
		# If only an equals match precise version:
		if(grepl("=", r, fixed=TRUE)) type <- "eq"
		# Greater than takes precedence:
		if(grepl(">", r, fixed=TRUE)) type <- "gt"
		# Greater than or equal also possible:
		if(grepl(">=", r, fixed=TRUE)) type <- "gteq"	
		r <- gsub(">|=", "", r)
		if((compareVersion(actual, r)==0) & (type=="eq" | type=="gteq")){
			matched <- TRUE
		}
		if((compareVersion(actual, r)==1) & (type=="gt" | type=="gteq")){
			matched <- TRUE
		}
	}
	
	return(matched)	
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
				if(class(temp)=="list"){
					# Allow recycling values if the list is of length 1 (this is equivalent to a non-list anyway):
					if(length(temp)<chain){
						if(length(temp)==1) temp <- temp[[1]] else temp <- NULL 
					}else{
						temp <- temp[[chain]]
					}
				}
			}else{
				suppressWarnings(success <- try(temp <- get(parameter[i]), silent=TRUE)) #, pos=".GlobalEnv"
				if(class(success)!="try-error"){
					if(class(temp)=="function"){
						success <- suppressWarnings(try(temp <- temp(chain), silent=TRUE))
						if(class(success)=="try-error") temp <- temp()
					}
					# Allow recycling values if the list is of length 1 (this is equivalent to a non-list anyway):
					if(length(temp)<chain){
						if(length(temp)==1) temp <- temp[[1]] else temp <- NULL 
					}else{
						temp <- temp[[chain]]
					}
				}else{
					temp <- NULL
				}
			}

			if(is.null(temp)) stop(paste(parameter[i], " not found (or has/returns value NULL", if(chain>1) " for chain ", if(chain>1) chain, ")", sep="")) 
			
		}
		
		# necessary to remove compound listing somehow introduced by initlist function or something:
		while(class(temp)=="list") temp <- temp[[1]]
		
		value[[i]] <- temp
		names(value)[[i]] <- parameter[i]

	}
	
	return(value)

}

normalise.mcmcfun <- function(mcmc.list, normalise = TRUE, warn = TRUE, check.stochastic = TRUE){

	if(class(mcmc.list)=="mcmc") mcmc <- mcmc.list(mcmc.list) else mcmc <- mcmc.list

	if(class(mcmc)!="mcmc.list") stop("Object to be normalised must be an mcmc list or mcmc object")

	vnames <- lapply(mcmc, dimnames)
	
	if(check.stochastic){
		anyvariancezero <- sapply(mcmc.list, function(x) return(apply(x,2,var)==0), simplify="array")
		if(!is.matrix(anyvariancezero)) anyvariancezero <- matrix(anyvariancezero, nrow=1)
		if(any(apply(anyvariancezero,1,any)!=apply(anyvariancezero,1,all))) warning("Variance of one or more parameters is zero in one chain but non-zero in another chain")
		nonstochastic <- apply(anyvariancezero,1,any)
		if(all(nonstochastic)) stop("All monitored variables appear to be non-stochastic; try adding monitored variables either using #monitor# in the model code or by specifying a monitor argument (even if this is just 'deviance') to run.jags", call.=FALSE)
		if(any(nonstochastic)){
			removed <- names(nonstochastic)[nonstochastic]
			warnmessage <- paste("*WARNING* The monitored variable", if(sum(nonstochastic)>1) "s", " '", if(sum(nonstochastic)>1) paste(removed[1:(length(removed)-1)], collapse="', '"), if(sum(nonstochastic)>1) "' and '", removed[length(removed)], "' appear", if(sum(nonstochastic)==1) "s", " to be non-stochastic; ", if(sum(nonstochastic)>1) "they" else "it", " will not be included in the convergence diagnostic", sep="")	
			if(warn==TRUE) swcat(warnmessage,"\n")
			if(warn=="warning") warning(warnmessage, call.=FALSE)
			mcmc <- mcmc[,-which(nonstochastic),drop=FALSE]
		}
	}

	if(normalise){
	
		if(niter(mcmc)>1000) use <- sample(1:niter(mcmc), size=1000, replace=FALSE) else use <- 1:niter(mcmc)
		shap.res <- apply(combine.mcmc(mcmc, collapse.chains=TRUE), 2, function(x){
			rv <- 1
			if(var(x)!=0){
				# We might get an error if we sample 1000 funny values - in which case leave them alone (likely to be small variance so transform probably unnecessary anyway)
				suppressWarnings(success <- try({
					if(all(x > 0)){
						if(all(x < 1)){
							if(shapiro.test(x[use])$statistic > shapiro.test(log(x[use]/(1-x[use])))$statistic) rv <- 3
						}else{
							if(shapiro.test(x[use])$statistic > shapiro.test(log(x[use]))$statistic) rv <- 2
						}
					}
				}, silent=TRUE))
			}
			return(rv)
		})
	
		change <- which(shap.res!=1)
		for(parameter in change){
			for(chain in 1:nchain(mcmc)){
				newvalues <- unlist(mcmc[[chain]][,parameter])
				if(shap.res[parameter]==3) newvalues <- log(newvalues/(1-newvalues)) else newvalues <- log(newvalues)
				mcmc[[chain]][,parameter] <- newvalues
			}
		}
	}

	for(i in 1:length(mcmc)) dimnames(mcmc[[i]])[[1]] <- vnames[[i]][[1]]
	
	if(class(mcmc.list)=="mcmc") return(mcmc[[1]]) else return(mcmc)

}

safe.autocorr.diag <- function(x, ...){
	y <- autocorr.diag(x[,1],...)
	if(nvar(x)>1) for(i in 2:nvar(x)) y <- cbind(y, autocorr.diag(x[,i],...))
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


getRversion <- function(){
	vers <- R.version$version.string
	version.string <- gsub("\\(.*?\\)","",vers,perl=FALSE)
	return(getversion(version.string))
}

getversion <- function(version.string){
	vers <- gsub("[[:space:]]","",gsub("[[:alpha:]]","",version.string))
	vers <- gsub("[[:punct:]]", "", gsub("."," ",vers, fixed=TRUE))
	vers <- strsplit(vers," ",fixed=TRUE)[[1]]
	version <- (10^((length(vers)-1):0))^3 * as.numeric(vers)
	version <- sum(version)
	return(version)
}


getargs <- function(functions, passed, returnall=TRUE){
	
	N <- length(functions)
	args <- vector('list', length=N)
	names <- vector('list', length=N)
		
	# Argument names for the functions relative to this function:
	for(i in 1:N){
		args[[i]] <- as.list(formals(get(functions[i], sys.frame(sys.parent(n=1)))))
		args[[i]] <- args[[i]][names(args[[i]])!="..."]
		names[[i]] <- names(args[[i]])
	}
	
 	argnames <- unique(unlist(names))
 	argmatch <- pmatch(names(passed), argnames)

 	if(any(is.na(argmatch))){
 		nomatches <- names(passed)[which(is.na(argmatch))]
 		
 		functstring <- paste(if(length(functions)>1) paste(functions[1:(length(functions)-1)], collapse="', '"), if(length(functions)>1)"' or '", functions[length(functions)], "' function", if(length(functions)>1) "s", sep="")
 		
 		argstring <- paste(if(length(nomatches)>1) "s", " '", if(length(nomatches)>1) paste(nomatches[1:(length(nomatches)-1)], collapse="', '"), if(length(nomatches)>1)"' or '", nomatches[length(nomatches)], "'", sep="")

 		stop(paste("No unambiguous match in arguments for '",functstring," for supplied argument", argstring, sep=""), call.=FALSE)
 	}
 	
	names(passed) <- argnames[argmatch]
	passed <- passed[!is.na(argmatch)]
 	
	if(returnall){
		# Now get defaults from specified functions, giving priority to earlier functions if arguments appear in more than 1:
		alreadymatched <- names(passed)
		for(i in 1:N){
			newget <- names[[i]][!(names[[i]] %in% alreadymatched)]
			newargs <- lapply(newget, function(x) try(as.expression(get(x, pos=args[[i]])), silent=TRUE))
			names(newargs) <- newget
			newargs <- newargs[!sapply(newargs,class)=="try-error"]
			passed <- c(passed, newargs)
			alreadymatched <- c(alreadymatched, newget)
		}
	}
	return(passed)
}

tailf <- function(file, start=1, refresh=0.1, min.static=1, max.static=Inf, stop.function=function() return(FALSE), stop.text=character(0), print=TRUE, return=!print){

	done <- FALSE

	readupto <- 1

	# Allow the simulation to start:
	if(!file.exists(file)) Sys.sleep(1)

	tryCatch({
	if(!file.exists(file)) stop("The named file does not exist")

	linesto <- start+1
	going <- TRUE
	static <- 0
	lastline <- ""

	text <- ""

	# Wait for file to start being written to:
	repeat{
		suppressWarnings(output <- readLines(file))
		# Catch occasional error with na being introduced:
		if(is.na(lastline)) lastline <- ""
		if(length(output)==0){
			static <- static+1
			if(static > max.static) break
		}else{
			break
		}
	}

	repeat{
	Sys.sleep(refresh)

	suppressWarnings(output <- readLines(file))
	# Catch occasional error with na being introduced:
	if(is.na(lastline)) lastline <- ""

	if(length(output)==(linesto-1)){
		if(output[length(output)]==lastline){
			static <- static+1
			if(static > max.static) break
		}else{
			static <- 0
			newsameline <- output[readupto]		
			new <- strsplit(newsameline, '')[[1]]
			last <- length(strsplit(lastline, '')[[1]])
			new <- paste(new[(last+1):length(new)], collapse='')
			if(print) cat(new)
			text <- paste(text, new, sep='')
			lastline <- output[length(output)]
		}
	}else{
		if(output[linesto-1]!=lastline){
			new <- output[linesto-1]
			new <- strsplit(new, '')[[1]]
			last <- length(strsplit(lastline, '')[[1]])
			new <- paste(new[(last+1):length(new)], collapse='')
			if(print) cat(new)
			text <- paste(text, new, sep='')
		}
		static <- 0
		new <- paste(output[linesto:length(output)], collapse='\n')
		if(print) cat('\n', new, sep='')
		text <- paste(text, '\n', new, sep='')
		linesto <- length(output)+1
		lastline <- output[length(output)]
	}

	flush.console()
	if(static > min.static & stop.function()) break
	
	allnewoutput <- paste(output[readupto:length(output)],collapse="\n")
	readupto <- length(output)
	foundtextmatch <- FALSE
	for(t in stop.text){
		if(grepl(t, allnewoutput)) foundtextmatch <- TRUE
	}
	if(foundtextmatch) break

	}

	done <- TRUE

	}, finally={
	
		retval <- list(text=text, lines=length(output), interrupt=!done)
		if(return) return(retval)
	
		})
}

prettifytable <- function(x, digits=5, colsequal=FALSE, nastring="", colsep="  "){
	# Stops ridiculously small things:
	x <- round(x, digits=10)
	formatted <- formatC(x, format="fg", digits=digits, width=-1)
	formatted <- gsub("NA",nastring,formatted)
	formatted <- rbind(dimnames(x)[[2]], formatted)
	dimnames(formatted) <- NULL
	
	if(colsequal){
		retval <- format(formatted, justify="right")
	}else{
		retval <- apply(formatted, 2, format, justify="right")
	}	
	
	return(apply(cbind(format(c("",dimnames(x)[[1]]), justify="left"), retval), 1, paste, collapse=colsep))

}

checkvalidrunjagsobject <- function(runjags.object){
	if(class(runjags.object)!="runjags") stop("The output of a runjags function (with class 'runjags') must be supplied", call.=FALSE)

		# More strict:
#	if(!identical(names(runjags.object), c('mcmc', 'pd', 'popt', 'pd.i', 'end.state', 'burnin', 'sample', 'thin', 'summary', 'HPD', 'hpd', 'mcse', 'psrf', 'autocorr', 'crosscorr', 'stochastic', 'dic', 'trace', 'density', 'model', 'data', 'monitor', 'modules', 'factories', 'method', 'method.options', 'timetaken'))) stop("Invalid runjags.object provided; the output of a runjags function (with class 'runjags') must be supplied", call.=FALSE)

# Less strict:
	if(!all(c('mcmc', 'end.state', 'burnin', 'sample', 'thin', 'model', 'data', 'monitor', 'modules', 'factories', 'method', 'method.options', 'timetaken') %in% names(runjags.object))) stop("Invalid runjags.object provided; the output of a runjags function (with class 'runjags') must be supplied", call.=FALSE)

	invisible(TRUE)
}

matchvars <- function(vars, names){
	
	vars <- as.character(na.omit(vars))
	
	if(length(vars)>0){
	#	matched <- vapply(vars, function(m) return(grepl(paste("^",m,sep=""),names)), logical(length(names)))
		matched <- vapply(vars, function(m) return(grepl(m,paste("^",names,sep=""),fixed=TRUE)), logical(length(names)))
	
		exact <- vapply(vars, function(m) return(tolower(gsub("'","",gsub('"','',m,fixed=TRUE),fixed=TRUE)) == names), logical(length(names)))
	
		exactneeded <- t(matrix((grepl("'",vars,fixed=TRUE) | grepl('"',vars,fixed=TRUE)), ncol=length(names), nrow=length(vars)))
	
		selected <- which(apply( (matched & !exactneeded) | (exact & exactneeded) , 1, any))

		if(length(selected)==0) stop("No matches for the variable names supplied", call.=FALSE)

	}else{
			
		selected <- 1:length(names)
	}
	
	return(selected)	

}

checkvalidforjags <- function(object){
	
	if(length(object)==1 && is.na(object)) return(list(valid=TRUE, probstring=""))
	
	if(class(object)=="runjags.data" || class(object)=="runjags.inits") class(object) <- "character"
	if(class(object)!="list" && class(object)!="character") return(list(valid=FALSE, probstring="object must be either a named list or a character vector in the R dump format"))
	
	if(!is.list(object)) object <- list.format(object, checkvalid=FALSE)
	
	if(any(names(object) == "")){
		return(list(valid=FALSE, probstring="missing variable name(s)"))
	}
	if(!length(unique(names(object))) == length(object)){
		return(list(valid=FALSE, probstring="duplicated variable name(s)"))
	}
		
	problems <- sapply(object, function(x){
		
		# NA values are permitted....
		#if(any(is.nan(x))){
		#	return("NaN")
		#}
		#if(any(is.na(x))){
		#	return("NA")
		#}
		x <- na.omit(x)
		if(length(x)==0){
			return("")
		}
		if(any(is.null(x))){
			return("NULL")
		}
		if(class(x)=="data.frame"){
			return("class 'data.frame' - try converting it to a valid type using as.matrix")
		}
		if(length(x)==0){
			return("length zero")
		}
		if(class(x)=="logical"){
			return("TRUE/FALSE")
		}
		if(class(x)=="character"){
			return("character")
		}
		if(class(x)=="factor"){
			return("factor")
		}
		if(any(x==Inf)){
			return("Inf")
		}
		if(any(x==-Inf)){
			return("Inf")
		}
		return("")
	})	
	
	problems[names(problems)==".RNG.name"] <- ""
	
	if(all(problems=="")){
		return(list(valid=TRUE, probstring=""))
	}else{
		problems <- problems[problems!=""]
		probstring <- paste("invalid variable value(s) - ", paste(names(problems), " (", problems, ")", sep=""), collapse=", ", sep="")
		return(list(valid=FALSE, probstring=probstring))	
	}
	
}

getrunjagsmethod <- function(method){
	methodmatch <- pmatch(tolower(method), c('rjags', 'simple', 'interruptible', 'parallel', 'snow', 'background', 'bgparallel', 'xgrid'))
	if(is.na(methodmatch)){
		stop(paste("Unsupported or ambiguous method '", method, "'; choose one of 'rjags', 'simple', 'interruptible', 'parallel', 'snow', 'background' or 'bgparallel'", sep=""), call.=FALSE)
	}else{
		method <- c('rjags', 'simple', 'interruptible', 'parallel', 'snow', 'background', 'bgparallel', 'xgrid')[methodmatch]
	}
	if(method=="rjags"){
		if(!require('rjags')) stop("The rjags package is not installed - please install this package to use the 'rjags' method for runjags", call.=FALSE)
		if(packageVersion('rjags') < 3.9) stop("Please update the rjags package to version 3-9 or later", call.=FALSE)				
	}
	if(.Platform$OS.type=='unix' && (.Platform$GUI!="AQUA" & Sys.info()['user']=='nobody' && !(method %in% c('rjags','simple')))){
		warning("You may be trying to use a runjags method on Xgrid which won't work - choose either rjags or simple methods for using run.jags functions on Xgrid (or see the xgrid.jags functions for an alternative)")
	}
	
	return(method)
}