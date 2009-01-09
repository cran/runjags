findjags <- function(ostype = .Platform$OS.type){

	if(ostype=="unix"){
		return("jags")
	}
	
	if(ostype=="windows"){
		
		versions <- paste(as.integer(599:200/200), as.integer(199:0/20), as.integer(19:0), sep='.')
    
  		paths <- character(length=(length(versions)*2)+2)
		
		paths[1:length(versions)] <- paste("C:/Program Files/JAGS/JAGS-", versions, "/bin", sep="")
		paths[(length(versions)+1):(length(versions)*2)] <- paste("C:/JAGS/JAGS-", versions, "/bin", sep="")
		
		paths[(length(versions)*2)+2] <- "C:/Program Files/JAGS/bin"
		paths[(length(versions)*2)+1] <- "C:/JAGS/bin"
		
		paths <- c("C:/Program Files/JAGS/bin", "C:/JAGS/bin", paths)
		
		##  include paths for JAGS 0.97.x?
  
		exists <- file.exists(paths)
		exists[length(exists)] <- TRUE
		
		##  JAGS/bin is a folder, and therefore exists.  JAGS/bin/jags.bat and JAGS/bin/jags-terminal.exe should also exist
		
		paths <- paste(paths, '/jags-terminal.exe', sep='')
		
		##  jags.bat spawns lots of horrible windows.  jags-terminal.exe requires environmental variables - set by testjags()
		
		return(paths[exists==TRUE][1])
	}
	
	stop(paste("Unrecognised OS type '", ostype, "'.  Use either 'unix' or 'windows'", sep=""))
}

findJAGS <- findjags