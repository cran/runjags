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

tailf <- function(file, start=1, refresh=0.1, min.static=1, max.static=Inf, stop.when=function() return(FALSE), print=TRUE, return=!print){

done <- FALSE

# Allow the simulation to start:
Sys.sleep(0.5)

tryCatch({
if(!file.exists(file)) stop("The named file does not exist")

linesto <- start+1
going <- TRUE
static <- 0
lastline <- ""

text <- ""

repeat{
Sys.sleep(refresh)

suppressWarnings(output <- readLines(file))
# Catch occasional error with na being introduced:
if(is.na(lastline)) lastline <- ""
if(length(output)==0){
	static <- static+1
	if(static > max.static) break
}

if(length(output)==(linesto-1)){
	if(output[length(output)]==lastline){
		static <- static+1
		if(static > max.static) break
	}else{
		static <- 0
		new <- output[length(output)]
		new <- strsplit(new, '')[[1]]
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
if(static > min.static & stop.when()) break
}

done <- TRUE

}, finally={
	
	retval <- list(text=text, lines=length(output), interrupt=!done)
	if(return) return(retval)
	
	})
}
