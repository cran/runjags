testjags <- function(jags=findjags(), silent=FALSE){

	tempfile <- new_unique(name=c("test.", NA), suffix=".cmd")
	write("exit", file=tempfile)
	s.info <- Sys.info()
	p.info <- .Platform
	
	if(as.numeric(paste(R.version$major, R.version$minor, sep="")) < 26){
    	stop("Please update your version of R to the latest available (> 2.6.0 required)")
	}
	
	os <- p.info$OS.type
	username <- as.character(s.info["user"])
	rversion <- R.version$version
	gui <- p.info$GUI
	p.type <- p.info$pkgType
	
	if(os=="windows"){
		
		if(file.exists(paste(jags, 'bin/jags-terminal.exe', sep=''))) jags <- paste(jags, 'bin/jags-terminal.exe', sep='')
		if(file.exists(paste(jags, '/bin/jags-terminal.exe', sep=''))) jags <- paste(jags, '/bin/jags-terminal.exe', sep='')
		if(file.exists(paste(jags, 'jags-terminal.exe', sep=''))) jags <- paste(jags, 'jags-terminal.exe', sep='')
		if(file.exists(paste(jags, '/jags-terminal.exe', sep=''))) jags <- paste(jags, '/jags-terminal.exe', sep='')

		path <- strsplit(jags, split='/bin', fixed=TRUE)

		firstpath <- path[[1]][1:length(path[[1]])-1]

		binpath <- paste(firstpath, '/bin;%PATH%', sep='')
		libpath <- paste(firstpath, '/modules', sep='')

		Sys.setenv(PATH=binpath)
		Sys.setenv(LTDL_LIBRARY_PATH=libpath)

		jags <- paste("\"", jags, "\"", sep="")
		
		popen.support = popen <- FALSE
		suppressWarnings(popen.support <- try(system(paste(jags, " ", tempfile, sep=""), intern=TRUE, wait=TRUE, show.output.on.console = FALSE), silent=TRUE))
	    if(class(popen.support)=="try-error"){
	    	popen <- FALSE
	    	suppressWarnings(success <- system(paste(jags, " ", tempfile, sep=""), ignore.stderr = TRUE, wait=TRUE, show.output.on.console = FALSE))
	    }else{
	    	popen <- TRUE
	    	suppressWarnings(try({if(strsplit(popen.support[[1]], split=" ")[[1]][1]=="Welcome"){
	    		success <- 0
	    	}else{
		    	success <- system(paste(jags, " ", tempfile, sep=""), intern = TRUE, ignore.stderr = TRUE, wait=TRUE, show.output.on.console = FALSE)
		    }}, silent=TRUE))
	    }
	}
	if(os=="unix"){
		jags <- paste("\"", jags, "\"", sep="")
		popen.support = popen <- FALSE
		suppressWarnings(popen.support <- try(system(paste(jags, " ", tempfile, sep=""), intern=TRUE, wait=TRUE, show.output.on.console = FALSE), silent=TRUE))
		suppressWarnings(success <- system(paste(jags, " ", tempfile, " > /dev/null", sep=""), ignore.stderr = TRUE, wait=TRUE, show.output.on.console = FALSE))
		if(class(popen.support)=="try-error"){
			popen <- FALSE
		}else{
			popen <- TRUE
		}
	}

	if(os != "unix" && os != "windows"){
		stop("Error analysing operating system")
	}
	if(length(success)==0) success <- 666 else length(success) <- 1
	if(popen==TRUE && success==0){
    	rightstring <- logical(length=length(popen.support))
  		for(i in 1:length(popen.support)){
    		rightstring[i] <- strsplit(popen.support[[i]], split=" ")[[1]][1]=="Welcome"
    	}
    	rightstring <- which(rightstring==TRUE)
    	if(is.na(rightstring)){
    		version <- 'unknown'
    		num.version <- c(0,0,0)
    	}else{
    		version <- strsplit(popen.support[rightstring], split=" ", fixed=TRUE)[[1]][4]
    		num.version <- strsplit(version, split=".", fixed=TRUE)
    	}
    }
    
	if(silent==FALSE){
		cat("You are currently logged on as ", username, ", on a ", os, " machine\n", sep="")
		cat("You are using ", rversion, ", with the ", gui, " GUI", "\n", sep="")
		if(os=="windows"){
			cat("WARNING:  JAGS will run more slowly under windows than unix, and suppression of JAGS output may not be available\n")
		}
		if(success==0){
			if(popen == TRUE){
				cat("JAGS version ", version, " found successfully using the command ", jags, "\n", sep="")
				if(as.numeric(num.version[[1]][1] == 0) && as.numeric(num.version[[1]][2] < 97)){
					cat("The version of JAGS currently installed on your system is no longer supported.  Please update JAGS from http://www-fis.iarc.fr/~martyn/software/jags/\n")
					jags.avail <- FALSE
				}else{
					jags.avail <- TRUE
				}
			}else{
				cat("JAGS was found on your system, but the version cannot be verified due to the absence of popen support.  Please ensure that the latest version of JAGS is installed by visiting http://www-fis.iarc.fr/~martyn/software/jags/\n")
				jags.avail <- TRUE
				num.version <- list("version unknown")
			}
		}else{
			cat("JAGS was not found on your system using the command ", jags, ".  Please ensure that the command is correct and that the latest version of JAGS from http://www-fis.iarc.fr/~martyn/software/jags/ is installed\n", sep="")
			jags.avail <- FALSE
			num.version <- list("none found")
		}
	}else{
		if(success==0){
			if(popen == TRUE){
				if(as.numeric(num.version[[1]][1] == 0) && as.numeric(num.version[[1]][2] < 97)){
					cat("The version of JAGS currently installed on your system is no longer supported.  Please update JAGS from http://www-fis.iarc.fr/~martyn/software/jags/\n")
					jags.avail <- FALSE
				}else{
					jags.avail <- TRUE
				}
			}else{
				jags.avail <- TRUE
				num.version <- list("version unknown")
			}
		}else{
			jags.avail <- FALSE
			popen <- FALSE
			num.version <- list("none found")
		}
	}
	unlink(tempfile)
	return(c("os"=os, "JAGS.available"=jags.avail, "JAGS.path"=jags, "popen.support"=popen, "JAGS.version"=num.version, "R.version"=rversion, "R.GUI"=gui, "R.package.type"=p.type, "username"=username))
}

testJAGS <- testjags
