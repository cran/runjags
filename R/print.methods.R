print.runjags <- function(x, vars=NA, digits = 5, ...){
	
	# First check that we have matching variables:
	selected <- matchvars(vars, varnames(x$mcmc))
	
	numbers <- NULL
	success <- try({
		if(!is.list(x$method) && is.na(x$method)){
			cat("\nJAGS model summary:  Model not yet updated\n\n")
			m <- NA
		}else{
		
			chainstring <- if(nchain(x$mcmc)==1) "(single chain)" else paste("(", if(x$thin>1) "thin = 1 in ", if(x$thin>1) x$thin, if(x$thin>1) "; ", "chains = ", nchain(x$mcmc), "; burnin = ", x$burnin, ")",sep="")
			if(is.character(x$summary)){
				cat("\nJAGS model with ", niter(x$mcmc)*nchain(x$mcmc), " samples ", chainstring, "\n", sep="")
				cat("\nFull summary statistics aren't available as this model was last extended using summarise=FALSE argument - you can use eg summary(as.mcmc.list(results)) or extend.jags(results, sample=0, summarise=TRUE) to append summary statistics\n\n", sep="")
				m <- NA
			}else{
				cat("\nJAGS model summary statistics from ", niter(x$mcmc)*nchain(x$mcmc), " samples ", chainstring, ":\n\n", sep="")

				if(any(c(is.character(x$hpd),is.character(x$summary),is.character(x$mcse),is.character(x$autocorr)))){
					m <- "There was an error calculating summary statistics - use $summary, $hpd etc to access individual elements"
				}else{				

					# Add 'NA' for non-stochastic variables:
					autocorrs=mcse=pos=sse=psrfs <- replicate(length(x$stochastic),NA)
					autocorrs[x$stochastic] <- x$autocorr[4,]
					mcse[x$stochastic] <- x$mcse$mcse
					sse[x$stochastic] <- round(x$mcse$sse)
					pos[x$stochastic] <- round((x$mcse$mcse/x$summary$statistics[x$stochastic,2])*100,1)
					
					x$summary$statistics[,1:2,drop=FALSE]
					
					# Catch if only 1 chain
					if(is.character(x$psrf)){
						psrfs <- replicate(length(x$mcse$mcse), NA)
					} else {
						psrfs[x$stochastic] <- x$psrf$psrf[,1]
					}

					numbers <- cbind(x$hpd[,1:3,drop=FALSE], x$summary$statistics[,1:2,drop=FALSE], mcse, pos, sse, autocorrs, psrfs)
					dimnames(numbers) <- list(dimnames(x$hpd)[[1]], c(dimnames(x$hpd)[[2]],"Mean","SD","MCerr","MC%ofSD","SSeff",gsub("Lag ", "AC.", dimnames(x$autocorr)[[1]][4]),"psrf"))
				
					selected <- matchvars(vars, dimnames(numbers)[[1]])
					numbers <- numbers[selected,,drop=FALSE]
				
					m <- prettifytable(numbers, digits=digits, colsequal=FALSE, nastring="", colsep="  ")
				
				}
			
				cat(m, "", sep="\n")
	
				if(class(x$dic)!="character"){
					cat("Model fit assessment (DIC):  ", format(round(x$dic$dic, digits=digits), scientific=FALSE), "  (range between chains: ", format(round(min(x$dic$dic.chains), digits=digits), scientific=FALSE), " - ", format(round(max(x$dic$dic.chains), digits=digits), scientific=FALSE), ")\n", sep="")
					cat("Estimated effective number of parameters (pD):  ", format(round(x$dic$meanpd, digits=digits), scientific=FALSE), "\n\n", sep="")
				}
				
			    cat("Total time taken: ", timestring(as.numeric(x$timetaken, units="secs")), "\n\n", sep="")
			}
		}
		})
	if(class(success)=="try-error") stop("An unexpected error occured in the print method for runjags class")
	invisible(numbers)
}

summary.runjags <- function(object, ...){
	
	if(!identical(list(), list(...))) warning("Additional arguments supplied to summary.runjags were ignored")
	return(object$summary)
}


print.runjags.model <- function(x, linenumbers=TRUE, ...){
	if(linenumbers){
		split <- strsplit(x,"\n",fixed=TRUE)[[1]]
		lines <- length(split)
		x <- paste(paste(format(as.character(1:lines), justify='left'),"  |  ", split, sep=""),collapse="\n")
	}
	cat(c("\nJAGS model syntax:\n\n", x, if(linenumbers) "\n\n" else "\n"),sep="")
	invisible(x)
}
print.runjags.data <- function(x, linenumbers=TRUE, ...){
	if(x==""){
		cat("\nNo data supplied\n")
	}else{
		if(linenumbers){
			split <- strsplit(x,"\n",fixed=TRUE)[[1]]
			lines <- length(split)
			x <- paste(paste(format(as.character(1:lines), justify='left'),"  |  ", split, sep=""),collapse="\n")
		}
		cat(c("\nJAGS data:\n\n", x, if(linenumbers) "\n\n" else "\n"),sep="")
	}
	invisible(x)
}
print.runjags.inits <- function(x, linenumbers=TRUE, ...){
	cat(c("","JAGS chains initial values / end states:"),sep="\n")
	if(linenumbers){
		for(i in 1:length(x)){
			split <- strsplit(x[i],"\n",fixed=TRUE)[[1]]
			lines <- length(split)
			if(lines>0) x[i] <- paste(paste(format(as.character(1:lines), justify='left'),"  |  ", split, sep=""),collapse="\n")
			cat(c("\nChain ", i, ":\n\n", "", x[i],if(linenumbers) "\n"),sep="")
		}	
	}else{
		for(i in 1:length(x)){
			cat(c("\nChain ", i, ":\n", "", x[i],""),sep="")
		}
	
	}
	cat("\n")
	invisible(x)
}

print.runjags.plots <- function(x,vars=NA,layout=c(1,1),newwindows=!.Platform$GUI%in%c("AQUA","Rgui"),file="",add.crosscorr=FALSE,...){
	
	if(!length(layout)==2) stop("The layout option must be a numeric vector of length 2")
	if(!all(layout>0)) stop("All dimensions in the layout vector must be >=1")
	
	if(class(x)=="runjags"){
		# Interleave trace and density for the same variable:
		toplot <- c(x$trace, x$density)
		if(class(toplot)=="character"){
			cat(toplot)
			invisible(toplot)
		}
		plotnums <- 2

		orignames <- names(toplot)
		for(i in 1:length(x$trace)){
			toplot[[((i-1)*2)+1]] <- x$trace[[i]]
			toplot[[((i-1)*2)+2]] <- x$density[[i]]
		}
		# Fix the names:
		names(toplot) <- orignames[((1:(length(x$trace)*2))/2) + c(0.5,length(x$trace))]
	}else{
		toplot <- x
		if(class(toplot)=="character"){
			cat(toplot)
			invisible(toplot)
		}
		plotnums <- 1
	}	
	
	selected <- matchvars(vars, names(toplot))
	toplot <- toplot[selected]
	
	if(file==""){
		if(!newwindows){
			cat("Producing ", length(selected)+!identical(add.crosscorr,FALSE), " plots for ", length(selected)/plotnums," variables to the active graphics device\n(see ?runjagsclass for options to this S3 method)\n",sep="")
		}else{
			cat("Producing ", length(selected)+!identical(add.crosscorr,FALSE), " plots for ", length(selected)/plotnums," variables to separate graphics devices\n",sep="")
		}
	}else{
		cat("Producing ", length(selected)+!identical(add.crosscorr,FALSE), " plots for ", length(selected)/plotnums," variables to file\n",sep="")
	}			
	
	# Make layout consistent with ordering of dimensions of arrays:
	layout <- layout[2:1]
	
	output <- capture.output({
		
		if(file!="") pdf(file,...)
		if(!exists("dev.new")) dev.new <- x11

		if(!identical(add.crosscorr,FALSE)){
			crosscorr.plot(add.crosscorr)
			title(sub="Cross-correlation")
		}
	
		N <- length(toplot)
		numperpage <- layout[1] * layout[2]
		numpages <- ceiling(N/numperpage)
	
		mores <- numeric(N)	
		mores[] <- TRUE
		mores[(0:numpages)*numperpage] <- FALSE
		mores[length(mores)] <- FALSE
		
		newpage <- TRUE
		p1 <- 1
		p2 <- 1
		
		# This stops the xyplots being overlaid onto the crosscorr.plot:
		plot.new()		
		
		for(i in 1:N){
			if(newpage && file=="" && newwindows){
				dev.new()
			}
			class(toplot[[i]]) <- "trellis"
			print(toplot[[i]], split=c(p1,p2,layout[1],layout[2]), more=mores[i], newpage=newpage, ...)
	
			newpage <- FALSE
			p1 <- p1+1
			if(p1 > layout[1]){
				p1 <- 1
				p2 <- p2+1
				if(p2 > layout[2]){
					p2 <- 1
					newpage <- TRUE
				}
			}
		}
		
		if(file!="") dev.off()
		
	})
	invisible(toplot)
}


print.crosscorr.stats <- function(x, vars=NA, digits=5, ...){
    cat("Cross-correlation matrix:\n\n")

	selected <- matchvars(vars, dimnames(x)[[1]])
	x <- x[selected,selected,drop=FALSE]
		
	m <- prettifytable(x, digits=digits, colsequal=FALSE, nastring="", colsep="  ")
	
	cat(m, sep="\n")
	
	cat("\n[See also 'crosscorr.plot(as.mcmc(runjags.object))' for a prettier output]\n")
	
	invisible(x)
}

print.mcse.stats <- function(x, vars=NA, digits = 5, ...){
	
	x <- unlist(x)
	varnames <- names(x)
	varnames <- varnames[grepl("sseff.",varnames)]
	varnames <- gsub("sseff.","",varnames)

	selected <- matchvars(vars, varnames)
	x <- x[c(selected, selected+length(varnames), selected+(2*length(varnames)))]
	varnames <- varnames[selected]
	
    cat("Monte Carlo standard error:\n\n")
	numbers <- matrix(nrow=length(varnames),ncol=4,dimnames=list(varnames,c("SSeff", "SD", "MCerr", "% of SD")))	
	numbers[,1] <- as.integer(x[1:(length(x)/3)])
	numbers[,2] <- x[((length(x)/3)+1):(length(x)*2/3)]
	numbers[,3] <- x[((length(x)*2/3)+1):length(x)]
	numbers[,4] <- round(numbers[,3]/numbers[,2]*100,1)

	m <- prettifytable(numbers, digits=digits, colsequal=FALSE, nastring="", colsep="  ")
		
	cat(m, sep="\n")
	
    cat("\n[A rule of thumb is that the Monte Carlo error should be less than 5% of the standard deviation of the sample]\n")

	invisible(numbers)
}



print.gelman.with.target <- function(x, vars=NA, digits = 3, ...){

	selected <- matchvars(vars, dimnames(x))
	x <- x[selected,drop=FALSE]

    cat("Potential scale reduction factors:\n\n")
    print.default(x$psrf, digits = digits, ...)
    if (!is.null(x$mpsrf)) {
        cat("\nMultivariate psrf (for all monitored variables):\n\n")
        cat(format(x$mpsrf, digits = digits))
    }
    
    cat("\n\nTarget psrf\n\n")
    cat(format(x$psrf.target, digits = digits))
    cat("\n")
	invisible(x)
}

print.dic.stats <- function(x, digits=3, ...){
	if(class(x)=="character"){
		cat(x)
	}else{
		string <- paste("Model fit statistics:\n\nDeviance information criterion [mean(deviance)+mean(pd)]:  ", round(x$dic, digits=digits), "\n", sep="")
		string <- paste(string, "(Individual chains: ", paste(round(x$dic.chains, digits=digits), collapse=", "), ")\n", sep='')
		if(!is.na(x$ped)){
			string <- paste(string, "\nPenalized Expected Deviance [mean(deviance)+sum(popt)]):  ", round(x$ped, digits=digits), "\n", sep="")
			string <- paste(string, "(Individual chains: ", paste(round(x$ped.chains, digits=digits), collapse=", "), ")\n", sep='')
		}
		swcat(string)
	}
	invisible(x)
}

print.runjags.study <- function(x,...){
	
	if(!identical(x$means, NA)){
		cat("\nAverage values obtained from a JAGS study with a total of ", x$simulations-sum(x$crashed), " simulations", if(sum(x$crashed)>0) paste(" (excluding ", sum(x$crashed), " crashed simulations)", sep=""), ":\n\n", sep="")
		print.default(x$means,...)
		if(!identical(x$singles, NA)){
			cat("\nValues obtained for variables that were stochastic for only 1 simulation:\n\n", sep="")
			print.default(x$singles,...)
		}		
	}else{
		if(!identical(x$singles, NA)){
			cat("\nValues obtained from a JAGS study with a total of ", x$simulations-sum(x$crashed), " simulations", if(sum(x$crashed)>0) paste(" (excluding ", sum(x$crashed), " crashed simulations)", sep=""), ":\n\n", sep="")
			print.default(x$singles,...)
		}			
	}
	cat("\n")
	
	
	cat("Average time taken:  ", timestring(mean(as.numeric(x$timetaken, units="secs"))), " (range: ", timestring(min(as.numeric(x$timetaken, units="secs"))), " - ", timestring(max(as.numeric(x$timetaken, units="secs"))), ")\n", sep="")
	cat("Average burnin required:  ", round(mean(x$burnin)), " (range: ", round(min(x$burnin)), " - ", round(max(x$burnin)), ")\n", sep="")
	cat("Average samples required:  ", round(mean(x$sample)), " (range: ", round(min(x$sample)), " - ", round(max(x$sample)), ")\n", sep="")
	cat("\n")
	invisible(x$summary)
}

as.mcmc.runjags <- function(x){
	
	m <- x$mcmc
	
	# as.mcmc doesn't have ... in it's arguments so I can't add anything to it here:
	vars <- NA
	collapse.chains <- TRUE
	
	if(collapse.chains){
		# Probably better to be semi-consistent with as.mcmc on an mcmc list and at least throw a warning here:
		if(class(m)=="mcmc.list"){
			if(length(m)>1) warning(paste("Combining the ", length(m), " mcmc chains together", sep=""))
		}		
		m <- combine.mcmc(m, collapse.chains=TRUE)
	}
	
	m <- coda::as.mcmc(m)

	selected <- matchvars(vars, varnames(m))
	if(class(m)=="mcmc.list") thevarnames <- dimnames(m[[1]]) else thevarnames <- dimnames(m)
	m <- m[,varnames(m)[selected],drop=FALSE]	
	dimnames(m) <- list(thevarnames[[1]], thevarnames[[2]][selected])
	
	return(m)
	
}


as.mcmc.list.runjags <- function(x, vars=NA, ...){
	
	m <- as.mcmc.list(x$mcmc)
	
	selected <- matchvars(vars, varnames(m))
	thevarnames <- dimnames(m[[1]])
	m <- m[,varnames(m)[selected],drop=FALSE]	
	for(i in 1:length(m)){
		dimnames(m[[i]]) <- list(thevarnames[[1]], thevarnames[[2]][selected])
	}
	
	return(as.mcmc.list(m))
	
}

plot.runjags <- function(x,vars=NA,layout=NA, newwindows=NA,file="",type="all", ...){
	
	if(is.na(newwindows)) newwindows <- !.Platform$GUI%in%c("AQUA","Rgui")
		
	type <- c("trace","density","crosscorr","all")[pmatch(tolower(type), c("trace","density","crosscorr","all"))]

	if(length(type)==0 || any(is.na(type))) stop("The type options supplied must be in 'trace', 'density', 'crosscorr' or 'all'")
	type <- unique(type)
	
	if(any(type=="all")){
		type <- c("trace","density","crosscorr")
	}
		
	toplot <- x
	tlayout <- c(1,2)
	
	if(any("trace"%in%type) && !any("density"%in%type)){
		toplot <- x$trace
		tlayout <- c(1,1)
	}
	if(any("density"%in%type) && !any("trace"%in%type)){
		toplot <- x$density
		tlayout <- c(1,1)
	}
	
	crosscorr <- FALSE
	if(any("crosscorr"%in%type)){
		crosscorr <- combine.mcmc(x, vars=vars, return.samples=1000)
	}
	
	if(length(layout)==1 && is.na(layout)) layout <- tlayout
	
	if(!any("density"%in%type) && !any("trace"%in%type)){
		if(file!="") pdf(file,...)
		crosscorr.plot(crosscorr,sub="Cross-correlation")
		if(file!="") dev.off()
		invisible(crosscorr)
	}else{	
		invisible(print.runjags.plots(toplot,vars=vars,layout=layout,newwindows=newwindows,file=file,add.crosscorr=crosscorr))
	}
}

print.runjags.bginfo <- function(x, ...){

	cat("\nJAGS model summary:  Model currently running in the background ... use 'results.jags(background.runjags.object)' to retrieve the results.\nStarted on ", as.character(x$startedon), " in the following directory: '", x$directory, "'\n\n", sep="")
	invisible(x)
		
}

plot.runjags.bginfo <- function(x, ...){

	stop("Nothing to plot - retrieve the results for this suspended JAGS call using 'results.jags(background.runjags.object)' first")
	invisible(x)
		
}

plot.runjags.plots <- print.runjags.plots

as.jags <- function(x, ...){
	UseMethod("as.jags")
}
as.jags.default <- function(x, ...){
	stop("Conversion to jags objects is only possible for specific classes of object (such as objects of class 'runjags')")
}

as.runjags <- function(x, ...){
	UseMethod("as.runjags")
}
as.runjags.default <- function(x, ...){
	stop("Conversion to runjags objects is only possible for specific classes of object (such as objects of class 'jags')")
}

failedjags <- new.env()
assign("model", "", envir=failedjags)
assign("data", "", envir=failedjags)
assign("inits", "", envir=failedjags)
