findjags <- function(ostype = .Platform$OS.type, look_in = if(ostype=="windows") c("/Program Files/","/") else NULL){

	if(ostype=="unix"){
		
		# First test popen support and then give up if we don't have it:
		testpopen <- system('printf " \r"', intern=TRUE)
		if(testpopen!=' \r') return('jags')
		
		# Then test standard 'jags':
		jagspath <- system('which jags 2>&1', intern=TRUE)
		# If that doesn't work try some paths and if one exists return it:
		if(length(jagspath)==0){
			paths <- c("/opt/local/bin/jags", "/opt/local/sbin/jags", "/usr/texbin/jags", "/usr/bin/jags", "/bin/jags", "/usr/sbin/jags", "/sbin/jags", "/usr/local/bin/jags", "/usr/X11/bin/jags")
			for(i in 1:length(paths)){
				jagspath <- system(paste('which ', paths[i], ' 2>&1', sep=""), intern=TRUE)
				if(length(jagspath)!=0) return(jagspath)
			}
		}
		
		# Or just return 'jags' (will do this if 'jags' works or if nothing works):
		return("jags")
	}
	
	if(ostype=="windows"){

		if(class(look_in)!="character") stop("The look_in argument supplied to findjags must be a character vector of paths")
		look_in <- paste(look_in,"/",sep="")
		look_in <- gsub("//","/",look_in,fixed=TRUE)
		paths <- paste(unlist(lapply(look_in, function(x) return(paste(x,list.files(x),sep="")))),"/",sep="")
		possible <- unique(paths[grepl("JAGS",paths)|grepl("jags",paths)])

		posspaths <- character(0)

		for(i in 1:length(possible)){
		binpaths <- list.files(possible[1],recursive=TRUE)
		posspaths <- c(posspaths, paste(possible[i],binpaths[grepl("jags-terminal.exe",binpaths)],sep=""))
		}

		posspaths <- unique(posspaths)

		versions <- sapply(posspaths, function(x){
			pathsegs <- strsplit(x,"/")[[1]]
			withnum <- grepl("[0-9]",pathsegs)
			return(getversion(pathsegs[withnum][1]))
		})
		# If versions doesn't contain a number, getversion will return 0

		x64s <- grepl('x86',posspaths)

		if(any(x64s)){
			versions <- versions[x64s]
			posspaths <- posspaths[x64s]
		}
		usepath <- posspaths[which(versions==max(versions))[1]]
		
		##  jags.bat spawns lots of horrible windows.  jags-terminal.exe requires environmental variables - set by testjags()
		
		return(usepath)
	}
	
	stop(paste("Unrecognised OS type '", ostype, "'.  Use either 'unix' or 'windows'", sep=""))
}

findJAGS <- findjags