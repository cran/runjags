
read.winbugs <- function(path){

exists = likelystring <- logical(length(path))

for(i in 1:length(path)){
	exists[i] <- suppressWarnings(try(file.exists(path[i]), silent=TRUE))
	if(class(exists[i])=="try-error") exists[i] <- FALSE
	likelystring[i] <- any(strsplit(path[i], "")[[1]]=="{") | any(strsplit(path[i], "")[[1]]=="=") | any(strsplit(path[i], "")[[1]]=="<") | any(strsplit(path[i], "")[[1]]=="#")
}

if(all(!likelystring) & all(!exists)) stop("Path(s) or modelstring(s) not valid")

if(length(path)==1){
	if(exists[1]) string <- paste(readLines(path, warn=FALSE), collapse="\n") else string <- path
	
}else{
	string <- ""
	for(i in 1:length(path)){
	
		if(likelystring[i]==FALSE & exists[i]==FALSE) warning(paste("Specified file '", path[i], "' does not exist", sep=""))
		if(exists[i]) string <- paste(string, paste(readLines(path[i], warn=FALSE), collapse="\n"), sep="\n") else string <- paste(string, path[i], sep="\n")
	}
}

string <- paste(string, "\n", sep="")

find <- c("\n\t", "\n ", ",\n", ",\r", ";\n", ";\r", "\r\t", "\r ", "\n\n", "\r\r")
for(i in 1:length(find)){
	repeat{
	
		splits <- strsplit(string, split=find[i])
		string <- paste(splits[[1]], collapse="\n")
		if(length(splits[[1]])==1) break
	
	}
}


model <- paste("model{\n", winbugs.extract.big("model", string), "\n}\n", sep="")

if(length(model)==0) stop("No model block was found")
if(length(model)>1){
	warning("More than 1 model block was found in the file.  The first model was used and other(s) ignored.")
	model <- model[1]
}

maindata <- winbugs.extract.big("data", string)

if(length(maindata) > 1){
	warning("More than 1 data block was found in the file.  Blocks were combined.")
	maindata <- paste(maindata, collapse="")
}

if(all(maindata=="\n")) maindata <- ""

autodata <- winbugs.extract.small("#data#", string)

maininits <- winbugs.extract.big("inits", string)

autoinits <- winbugs.extract.small("#inits#", string)

monitors <- ""
temp <- winbugs.extract.big("monitor", string)
for(i in 1:length(temp)){
	tempy <- strsplit(temp[i], "")[[1]]
	tempy <- paste(tempy[tempy!=" "], collapse="")
	#tempy <- paste(strsplit(temp[i], "")[[1]][strsplit(temp[i], "")!=""][[1]], collapse="")
	for(str in c("\n", "\t", ",", ":", ";")){
		tempy <- paste(strsplit(tempy, str, fixed=TRUE)[[1]], collapse="*")
	}
	tempy <- strsplit(tempy, "*", fixed=TRUE)[[1]]
	monitors <- c(monitors, tempy[tempy!=""])
}
monitors <- c(monitors, winbugs.extract.small("#monitor#", string))
monitors <- monitors[monitors!=""]

#if(length(monitors)==0) warning("No monitor blocks or tags were found")

#if(length(maindata)==0) warning("No data blocks or tags were found")

if(is.null(model)) model <- NA
if(is.null(maindata)) maindata <- NA
if(is.null(autodata)) autodata <- NA
if(is.null(maininits)) maininits <- NA
if(is.null(autoinits)) autoinits <- NA
if(is.null(monitors)) monitors <- NA

model[model==''] <- NA
maindata[maindata==''] <- NA
autodata[autodata==''] <- NA
maininits[maininits==''] <- NA
autoinits[autoinits==''] <- NA
monitors[monitors==''] <- NA

model[length(model)==0] <- NA
maindata[length(maindata)==0] <- NA
autodata[length(autodata)==0] <- NA
maininits[length(maininits)==0] <- NA
autoinits[length(autoinits)==0] <- NA
monitors[length(monitors)==0] <- NA

if(identical(model, as.character(NA))) model <- NA
if(identical(maindata, as.character(NA))) maindata <- NA
if(identical(autodata, as.character(NA))) autodata <- NA
if(identical(maininits, as.character(NA))) maininits <- NA
if(identical(autoinits, as.character(NA))) autoinits <- NA
if(identical(monitors, as.character(NA))) monitors <- NA

output <- list(model=model, data=maindata, autodata=autodata, inits=maininits, autoinits=autoinits, monitor=monitors)

return(output)

}

read.WinBUGS <- read.winbugs