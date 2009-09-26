ask <- function (prompt="?", type="logical", bounds=c(-Inf, Inf), na.allow=FALSE){
	prompt <- paste(prompt, '  ', sep='')
	
	if(((type == "logical") | (type == "numeric") | (type == "character") | (type == "integer")) != TRUE){
		stop(paste("Unrecognised return type \'", type, "\'", sep=""))
	}
	
	if(type=="logical"){
		repeat{
			result <- readline(prompt = prompt)
			if(na.allow==TRUE && (result=="NA" | result=="na" | result=="Na")){
				return(NA)
			}
			if((result == "T") | (result == "TRUE") | (result == "True") | (result == "true") | (result == "t") | (result == "y") | (result == "YES") | (result == "yes") | (result == "Yes") | (result == "Y")){
				return(TRUE)
			}
			if((result == "F") | (result == "FALSE") | (result == "False") | (result == "false") | (result == "f") | (result == "n") | (result == "NO") | (result == "no") | (result == "No") | (result == "N")){
				return(FALSE)
			}
			cat("ERROR:  Please enter 'y' or 'n'\n")
		}
	}
	
	if(type=="numeric"){
		repeat{
			result <- readline(prompt = prompt)
			if(na.allow==TRUE && (result=="NA" | result=="na" | result=="Na")){
				return(NA)
			}
			suppressWarnings(result <- as.numeric(result))
			if(is.na(result)){
				cat("ERROR:  Please enter a number\n")
			}else{
				if((result > bounds[2]) | (result < bounds[1])){
					cat("ERROR:  Please enter a number between ", bounds[1], " and ", bounds[2], "\n", sep="")
				}else{
					return(result)
				}
			}
		}
	}
	
	if(type=="character"){
		return(as.character(readline(prompt=prompt)))
	}
	
	if(type=="integer"){
		repeat{
			result <- readline(prompt=prompt)
			if(na.allow==TRUE && (result=="NA" | result=="na" | result=="Na")){
				return(NA)
			}
			suppressWarnings(result <- as.numeric(result))
			if(is.na(result) | (as.integer(result) != result)){
				cat("ERROR:  Please enter a whole number\n")
			}else{
				if((result > bounds[2]) | (result < bounds[1])){
					cat("ERROR:  Please enter a whole number between ", bounds[1], " and ", bounds[2], "\n", sep="")
				}else{
					return(result)
				}
			}
		}
	}			
}