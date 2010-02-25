tailf <- function(file, start=1, refresh=0.1, min.static=1, max.static=Inf, stop.when=function() return(FALSE), print=TRUE, return=!print){

done <- FALSE

tryCatch({
if(!file.exists(file)) stop("The named file does not exist")
suppressWarnings(output <- readLines(file))

linesto <- start+1
going <- TRUE
static <- 0
lastline <- ""

text <- ""

repeat{
Sys.sleep(refresh)

suppressWarnings(output <- readLines(file))
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
