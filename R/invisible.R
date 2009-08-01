print.gelman.with.target <- function (x, digits = 3, ...) 
{
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


print.plotindpages <- function (x, ...) 
{
	dev.new()
	class(x) <- "trellis"
	print(x, ...)
}


winbugs.extract.big <- function(find, string){
	
split <- strsplit(string, "")[[1]]

newstring <- ""

newlinelast = found = started <- FALSE
openbracket = closebracket = find.no <- 0

newlinelast <- TRUE

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
for(i in 1:length(newstring)){
	temp <- strsplit(newstring[i], "")[[1]]
	
	#  Because you can't have .Dim <- structure or variable = value:
	numbers <- which(temp=="=")
	if(length(numbers)>0){
		for(k in 1:length(numbers)){
			string <- character(length=10)
			for(j in 1:10){
				string[j] <- paste(temp[pmax((numbers[k]-3-j):(numbers[k]-j), 1)], collapse="")
			}	
			if(all(string!=".Dim")) temp[numbers[k]] <- "<-"
		}
	}
	
	newstring[i] <- paste(temp, collapse="")
}
return(newstring)
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

	value <- vector('list', length=length(names))
	
	for(i in 1:length(parameter)){	

		if(any(names==parameter[i])){
			temp <- inputlist[names==parameter[i]]
			
			if(class(temp)=="function"){
				success <- suppressWarnings(try(temp <- temp(chain), silent=TRUE))
				if(class(success)=="try-error") temp <- temp()
			}
			
			value[[i]] <- temp
			
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
					temp <- NULL
				}
			}
					
			if(is.null(temp)) stop(paste(parameter[i], "not found (or has/returns value NULL)")) 

			value[[i]] <- temp
			
		}

		names(value)[[i]] <- parameter[i]

	}


	return(value)

}

normalise.mcmc <- function(mcmc.list, normalise = TRUE, warn = TRUE, check.stochastic = TRUE){

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
			if(warn==TRUE) cat("\n")
		}
		usevec[i] <- NA
		if(warn==TRUE) cat(paste("*WARNING* The monitored variable '", parnames[i], "' appears to be non-stochastic.  It will not be included in the convergence diagnostic\n", sep=""))
		if(warn=="warning") warning(paste("The monitored variable '", parnames[i], "' appears to be non-stochastic.  It will not be included in the convergence diagnostic", sep=""))
	}}
}
if(anydone & warn==TRUE) cat("\n")
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
	for(parameter in 1:nvar(mcmc)){

		data <- unlist(mcmc[,parameter])

		if(!all(data > 0)) next

	
		if(all(data > 0) & all(data < 1)){
			logdat <- log(data/(1-data))
			logit <- TRUE
		}else{
			logdat <- log(data)
			logit <- FALSE
		}
		
		# having problems with some probabilites coming out at Inf, although this should be fixed now I am checking all values not just the thinned ones, but it doesn't do any harm to leave so:
	
		if(any(is.na(logdat))) next
		if(any(logdat==Inf) | any(logdat==-Inf)) next


		# sample for max length of shapiro.test:
		
		done <- 0
		
		alldata <- data
		alllogdat <- logdat
		
		# sometimes shapiro.test returns an error for the normal data (if very skewed?)
		# repeating it up to 100 times should be OK
		
		while(done < 100){
		
			if(length(alldata) > 1000) data <- sample(alldata, 1000)
			if(length(alllogdat) > 1000) logdat <- sample(alllogdat, 1000)
		
			norm.s <- shapiro.test(data)$statistic
			log.s <- shapiro.test(logdat)$statistic
			
			done <- done + 1
			
			if(!is.na(norm.s) & !is.na(log.s)) break
		}
		
		if(done==100){
			# in case repeating up to 100 times isn't OK
			if(warn) warning(paste("Attempt to normalise mcmc chain failed for the parameter '", varnames(mcmc)[parameter], "'", sep=""))
			next
		}
		
		if(norm.s >= log.s){
			use <- 1	
		}else{
			use <- if(logit) 3 else 2
		}
		
		# for bug testing log transformations:
		#use <- if(logit) 3 else 2
		
		if(use!=1){
			for(chain in 1:nchain(mcmc)){
				newvalues <- unlist(mcmc[[chain]][,parameter])
				if(logit) newvalues <- log(newvalues/(1-newvalues)) else newvalues <- log(newvalues)
				mcmc[[chain]][,parameter] <- newvalues
				
				
			}
		}
	}
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
		
		if(warn) cat("Note:  Unable to calculate the multivariate psrf due to an error calculating the Gelman-Rubin statistic\n")
		
		
		
		y <- list(psrf=psrfs, mpsrf="Unable to calculate multivariate psrf")
		
		class(y) <- "gelman.diag"
		return(y)
	}else{
		return(gelman)
	}
	
}
