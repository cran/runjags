
read.winbugs <- function(path){

exists <- suppressWarnings(try(file.exists(path), silent=TRUE))
if(class(exists)=="try-error") exists <- FALSE

likelystring <- any(strsplit(path, "")[[1]]=="{")

if(likelystring==FALSE & exists==FALSE) stop("Path or modelstring not valid")

if(exists) string <- paste(readLines(path, warn=FALSE), collapse="\n") else string <- path

string <- paste(string, "\n", sep="")

find <- c("\n\t", "\n ", ",\n", ",\r", ";\n", ";\r", "\r\t", "\r ", "\n\n", "\r\r")
for(i in 1:length(find)){
	repeat{
	
		splits <- strsplit(string, split=find[i])
		string <- paste(splits[[1]], collapse="\n")
		if(length(splits[[1]])==1) break
	
	}
}


extract.big <- function(find, string){
	
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





extract.small <- function(find, string){
	
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


model <- paste("model{\n", extract.big("model", string), "\n}\n", sep="")

if(length(model)==0) stop("No model block was found")
if(length(model)>1){
	warning("More than 1 model block was found in the file.  The first model was used and other(s) ignored.")
	model <- model[1]
}

maindata <- extract.big("data", string)

if(length(maindata) > 1){
	warning("More than 1 data block was found in the file.  Blocks were combined.")
	maindata <- paste(maindata, collapse="")
}

if(all(maindata=="\n")) maindata <- ""

autodata <- extract.small("#data#", string)

maininits <- extract.big("inits", string)

autoinits <- extract.small("#inits#", string)

monitors <- ""
temp <- extract.big("monitor", string)
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
monitors <- c(monitors, extract.small("#monitor#", string))
monitors <- monitors[monitors!=""]

#if(length(monitors)==0) warning("No monitor blocks or tags were found")

#if(length(maindata)==0) warning("No data blocks or tags were found")

output <- list(model=model, data=maindata)
if(!all(autodata=="")) output <- c(output, list(autodata=autodata))
output <- c(output, list(inits=maininits))
if(!all(autoinits=="")) output <- c(output, list(autoinits=autoinits))
if(!all(monitors=="")) output <- c(output, list(monitor=monitors))
return(output)

}

read.WinBUGS <- read.winbugs