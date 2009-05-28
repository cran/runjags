dump.format <- function(data=list()){
	
	if(length(data)==2 & is.null(names(data)) & class(data[[1]])=="character" & length(data[[1]])==1){  # allows old style dump.format (length = 1)
		names <- data
		data <- list(data[[2]])
		names(data) <- names[[1]]
	}
	
	if(class(data)!="list" | length(data)==0) stop("Data must be provided as a named list")
	if(any(names(data)=="") | is.null(names(data))) stop("Data must be provided as a named list")
	
	variable = names(data)
	value <- data
	
	if(any(variable==".RNG.name")){
		n <- which(variable==".RNG.name")
		split <- strsplit(value[[n]], split="")[[1]]
		if(split[1]!="\"" & split[length(split)]!="\""){
			split <- c("\"", split, "\"")
			value[[n]] <- paste(split, collapse="")
		}
	}
	
	output.string <- ""
	for(i in 1:length(variable)){
		if(length(value[[i]])==1 && length(dim(value[[i]]))==0){
			value.string <- as.character(value[[i]])
		}else{
			dims <- dim(value[[i]])
			if(length(dims) > 1){
				value.string <- "structure(c("			
			}else{
				value.string <- "c("
			}
			value.string <- paste(value.string, paste(value[[i]], collapse=", "), ")", sep="")
			if(length(dims) > 1){
				value.string <- paste(value.string, ", .Dim = c(", paste(dims, collapse=", "), "))", sep="")
			}
		}
		output.string <- paste(output.string, "\"", variable[[i]], "\" <- ", value.string, "\n", sep="")
	}
		
	return(output.string)
}
