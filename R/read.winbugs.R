
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

# Rubbish old code that didn't use gsub and was slow:
#find <- c("\n\t", "\n ", "\r\t", "\r ", "\n\n", "\r\r") # ",\n", ",\r", ";\n", ";\r", - removed these since we need ',\n' for some winbugs data but they are added again later on after spaces between () are removed
#for(i in 1:length(find)){
#	repeat{
	
#		splits <- strsplit(string, split=find[i])
#		string <- paste(splits[[1]], collapse="\n")
#		if(length(splits[[1]])==1) break
	
#	}
#}

# New gsub based code:
# Remove all tabs
string <- gsub("\t", "", string, fixed=TRUE)
# Convert carriage returns and form feeds to \n
string <- gsub("\f", "\n", string, fixed=TRUE)
string <- gsub("\r", "\n", string, fixed=TRUE)
# Remove commas and semi-colons from end of lines (allowing for as many spaces as you like between, and \n)
string <- gsub(",[[:space:]]*\n", "\n", string)
string <- gsub(";[[:space:]]*\n", "\n", string)
# Remove excess white space at the start of lines:
string <- gsub("\n[[:space:]]*", "\n", string)

model <- paste("model{\n", winbugs.extract.big("model", string)[[1]], "\n}\n", sep="")

# No helpful conversion of = to <- any more (was it doing this beforehand?)

if(length(model)==0) stop("No model block was found")
if(length(model)>1){
	warning("More than 1 model block was found in the file.  The first model was used and other(s) ignored.")
	model <- model[1]
}

maindata <- winbugs.extract.big("data", string)
datalistfound <- maindata[[2]]
maindata <- maindata[[1]]

jagsdata = listdata <- character()
# Removes any  character between '#' and a new line
# For regexp matching - '.' means any character, * means match the preceeding character any number of times

for(i in 1:length(maindata)){
# This would be greedy:
#tdata <- gsub("#.*\n", "\n", maindata[i])
# This is non-greedy matching:
tdata <- gsub("#(.*?)\n", "\n", maindata[i], perl=TRUE)

# This code will remove any "\n" from between "(" and ")" and replace it with a comma (all commas removed beforehand):
# [^\\)] means any character except ) - it's double escaped for some reason
greps <- gregexpr("\\([^\\)]*\n[^\\)]*\\)", tdata, extended=TRUE)
s <- greps[[1]]
e <- (s-1)+ attr(greps[[1]], "match.length")
if(s[1]!=-1){ # If it doesn't find anything, s=-1
finalstring <- strsplit(tdata, "", fixed=TRUE)[[1]]
for(j in 1:length(s)){
	
	tstring <- finalstring[s[j]:e[j]]
	newstring <- gsub("\n", ", ", tstring)
	finalstring[s[j]:e[j]] <- newstring
	
}
tdata <- paste(finalstring, collapse="")
# Remove extra spaces from previously indented things:
tdata <- gsub(",[[:space:]]*", ", ", tdata)
}

likelylist <- c(grepl(".Dim", maindata[i]), datalistfound[i], grepl("c\\(", maindata[i]))
likelyjags <- c(grepl("\\[", maindata[i]), grepl("for\\(", maindata[i]))
# Since I'm removing comments I think it's safe to assume anything with = is listdata?, anything with .Dim is listdata, anything with list( is listdata, anything with c( is listdata, anything with [ is jagsdata, anything with for( is jagsdata.  Check all these and if we get a disparity then a warning?

# A manual override mechanism:
definitelist <- grepl("#bugsdata#", maindata[i])
definitejags <- grepl("#jagsdata#", maindata[i])
#browser()

if(!definitelist & !definitejags){
if(! (any(likelylist)&!all(likelyjags) | any(likelyjags)&!all(likelylist))){
	#print("DATA TYPE UNKNOWN")
	#print("try parsing here?  Have an option to default data type if unkown?")
	#for(i in 1:length(maindata)){
	#	err <- try(eval(parse(text=maindata[i])))
	#}
	# For now assume that default is bugs type data:
	definitelist <- TRUE
}else{
	if(any(likelylist)) definitelist <- TRUE else definitejags <- TRUE
}
}

if(definitelist & definitejags) stop("Each data block can contain only one of #jagsdata# or #bugsdata# not both.")

if(definitelist) listdata <- c(listdata, tdata) else jagsdata <- c(jagsdata, tdata)

}


# Was previously in extract.big but now here as I want to see the = before converting them.  Maybe put this after the next bit of code and decide on whether to convert based on the same decision.

if(length(listdata) > 0){
newstring <- listdata
for(i in 1:length(newstring)){
	temp <- strsplit(newstring[i], "")[[1]]
	
	#  Because you can't have .Dim <- structure or variable = value:
	numbers <- which(temp=="=")
	if(length(numbers)>0){
		for(k in 1:length(numbers)){
			tstring <- character(length=10)
			for(j in 1:10){
				tstring[j] <- paste(temp[pmax((numbers[k]-3-j):(numbers[k]-j), 1)], collapse="")
			}	
			if(all(tstring!=".Dim")) temp[numbers[k]] <- "<-"
		}
	}
	
	newstring[i] <- paste(temp, collapse="")
}
maindata <- newstring
}else{
	maindata <- ""
}
#####################

if(length(maindata) > 1){
	warning("More than 1 WinBugs type data block was found in the file.  Blocks were combined.")
	maindata <- paste(maindata, collapse="")
}

if(length(jagsdata) > 1){
	warning("More than 1 JAGS type data block was found in the file.  Blocks were combined.")
	jagsdata <- paste(jagsdata, collapse="\n")
}

### Also need to check for variable=value, variable=value without new line:

# This code will change any "," to "\n" from between "<-" and "<-" unless a "(" or ")" are present:
# [^\\)^\\(] means any character except ) or (
# *? and perl makes it non-greedy matching
# Need a while loop as the same character can't be used as the end point of one search and the start of another, so variable=value, variabile=value, variable=value would only remove the first (although variable=value, variable=value\nvariable=value, variable=value would be fine)

s <- 0
while(s!=-1){
greps <- gregexpr("<-([^\\)^\\(]*?),([^\\)^\\(]*?)<-", maindata, perl=TRUE)
s <- greps[[1]]
e <- (s-1)+ attr(greps[[1]], "match.length")
if(s[1]!=-1){ # If it doesn't find anything, s=-1
finalstring <- strsplit(maindata, "", fixed=TRUE)[[1]]
for(j in 1:length(s)){
	
	tstring <- finalstring[s[j]:e[j]]
	newstring <- gsub(",", "\n", tstring)
	finalstring[s[j]:e[j]] <- newstring
	
}
maindata <- paste(finalstring, collapse="")
}
}
### Then make sure any new leading white space is removed (gsub is vectorised):
maindata <- gsub("\n[[:space:]]*", "\n", maindata)
####


if(all(maindata=="\n" | maindata==" " | maindata=="")) maindata <- ""# else maindata <- paste("data{\n", maindata, "\n}\n", sep="")
if(!all(jagsdata=="\n" | jagsdata==" " | jagsdata=="")) model <- paste("data{\n", jagsdata, "\n}\n\n", model, sep="")


autodata <- winbugs.extract.small("#data#", string)

maininits <- winbugs.extract.big("inits", string)[[1]]

# Removes any  character between '#' and a new line
# For regexp matching - '.' means any character, * means match the preceeding character any number of times
for(i in 1:length(maininits)){
# This would be greedy:
#tdata <- gsub("#.*\n", "\n", maindata[i])
# This is non-greedy matching:
tdata <- gsub("#(.*?)\n", "\n", maininits[i], perl=TRUE)

# This code will remove any "\n" from between "(" and ")" and replace it with a comma (all commas removed beforehand):
# [^\\)] means any character except ) - it's double escaped for some reason
greps <- gregexpr("\\([^\\)]*\n[^\\)]*\\)", tdata, extended=TRUE)
s <- greps[[1]]
e <- (s-1)+ attr(greps[[1]], "match.length")
if(s[1]!=-1){ # If it doesn't find anything, s=-1
finalstring <- strsplit(tdata, "", fixed=TRUE)[[1]]
for(j in 1:length(s)){
	
	tstring <- finalstring[s[j]:e[j]]
	newstring <- gsub("\n", ", ", tstring)
	finalstring[s[j]:e[j]] <- newstring
	
}
maininits[i] <- paste(finalstring, collapse="")
}
}

####### Was previously in extract.big but now here as I want to see the = before converting the data.  Still always want to convert inits though.
newstring <- maininits
for(i in 1:length(newstring)){
	temp <- strsplit(newstring[i], "")[[1]]
	
	#  Because you can't have .Dim <- structure or variable = value:
	numbers <- which(temp=="=")
	if(length(numbers)>0){
		for(k in 1:length(numbers)){
			tstring <- character(length=10)
			for(j in 1:10){
				tstring[j] <- paste(temp[pmax((numbers[k]-3-j):(numbers[k]-j), 1)], collapse="")
			}	
			if(all(tstring!=".Dim")) temp[numbers[k]] <- "<-"
		}
	}
	
	newstring[i] <- paste(temp, collapse="")
}
maininits <- newstring
######

### Also need to check for variable=value, variable=value without new line:
for(i in 1:length(maininits)){

# This code will change any "," to "\n" from between "<-" and "<-" unless a "(" or ")" are present:
# [^\\)^\\(] means any character except ) or (
# *? and perl makes it non-greedy matching
# Need a while loop as the same character can't be used as the end point of one search and the start of another, so variable=value, variabile=value, variable=value would only remove the first (although variable=value, variable=value\nvariable=value, variable=value would be fine)
s <- 0
while(s!=-1){
greps <- gregexpr("<-([^\\)^\\(]*?),([^\\)^\\(]*?)<-", maininits[i], perl=TRUE)
s <- greps[[1]]
e <- (s-1)+ attr(greps[[1]], "match.length")
if(s[1]!=-1){ # If it doesn't find anything, s=-1
finalstring <- strsplit(maininits[i], "", fixed=TRUE)[[1]]
for(j in 1:length(s)){
	
	tstring <- finalstring[s[j]:e[j]]
	newstring <- gsub(",", "\n", tstring)
	finalstring[s[j]:e[j]] <- newstring
	
}
maininits[i] <- paste(finalstring, collapse="")
}
}
}
### Then make sure any new leading white space is removed (gsub is vectorised):
maininits <- gsub("\n[[:space:]]*", "\n", maininits)
####


autoinits <- winbugs.extract.small("#inits#", string)

monitors <- ""
temp <- winbugs.extract.big("monitor", string)[[1]]
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