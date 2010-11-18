new_unique <- function(name=NA, suffix="", ask=FALSE, prompt="A file or directory with this name already exists.  Overwrite?", touch=FALSE, type='file'){
	
	if(suffix!=""){
		if(strsplit(suffix, split="")[[1]][1]!=".") suffix <- paste(".", suffix, sep="")
	}
	
	if(!any(type==c('file', 'folder', 'directory', 'dir', 'f', 'd'))) stop('The type specified must be either file or directory')
	
	if(type=='f') type <- 'file'
	if(any(type==c('folder', 'dir', 'd'))) type <- 'directory'
	
	for(i in 1:length(name)){
		if(is.na(name[i])){
			alphanumeric <- c(as.numeric(0:9), "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", toupper(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")))
			name[i] <- paste(sample(alphanumeric, 9, replace=TRUE), sep="", collapse="")
		}
	}
	
	temp <- paste(name, sep="", collapse="")
	exists <- file.exists(paste(temp, suffix, sep=""))
	if(exists==TRUE){
		path.ok <- FALSE
		counter <- 1
		if(ask==TRUE){
			path.ok <- ask(paste("\'", temp, suffix, "\'.  ", prompt, "  ", sep=""), type="logical")
			if(path.ok==TRUE){
				unlink(paste(temp, suffix, sep=""), recursive = TRUE)
			}
		}
		while(path.ok == FALSE){
			temptemp <- paste(temp, "_", counter, "", sep="")
			exists <- file.exists(paste(temptemp, suffix, sep=""))
			if(exists==TRUE){
				counter <- counter + 1
			}else{
				path.ok <- TRUE
				temp <- temptemp
				break
			}
		}
	}
	
	if(type=='file'){
		suppressWarnings(try(file.create(paste(temp, suffix, sep="")), silent=TRUE))
	}else{
		suppressWarnings(try(dir.create(paste(temp, suffix, sep="")), silent=TRUE))
	}
	permissions <- file.exists(paste(temp, suffix, sep=""))
	if(permissions==FALSE){
		swcat("Error:  Directory not writable\n")
		return("Directory not writable")
	}else{
		if(!touch) unlink(paste(temp, suffix, sep=""), recursive = TRUE)
	}
	if(!touch) suppressWarnings(try(backupforspaces <- file.remove(paste(temp, suffix, sep="")), silent=TRUE))
	return(paste(temp, suffix, sep=""))
}