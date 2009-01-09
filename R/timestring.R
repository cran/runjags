timestring <- function(time1, time2=NA, units=NA, show.units=TRUE){
	
	time <- na.omit(c(time1, time2))
	
	if(length(time)==2){
		time <- as.integer(difftime(time[2], time[1], units="secs"))
	}else{	
		if(length(time)==1){
			time <- as.integer(time)
		}else{
			stop("Error:  input variables incorrect")
		}
	}
	
	secs <- time
	mins <- round(time / (60), digits=1)
	hours <- round(time / (60*60), digits=1)
	days <- round(time / (60*60*24), digits=1)
	weeks <- round(time / (60*60*24*7), digits=1)	
	years <- round(time / (60*60*24*7*52), digits=1)
	
	if(!is.na(units)){
		if(units=="s" | units=="secs" | units=="seconds"){
			if(show.units==FALSE){
				return(secs)
			}else{
				return(paste(secs, " seconds", sep=""))
			}
		}
		if(units=="m" | units=="minutes" | units=="mins"){
			if(show.units==FALSE){
				return(mins)
			}else{
				return(paste(mins, " minutes", sep=""))
			}
		}
		if(units=="h" | units=="hours"){
			if(show.units==FALSE){
				return(hours)
			}else{
				return(paste(hours, " hours", sep=""))
			}
		}
		if(units=="d" | units=="days"){
			if(show.units==FALSE){
				return(days)
			}else{
				return(paste(days, " days", sep=""))
			}
		}
		if(units=="w" | units=="weeks"){
			if(show.units==FALSE){
				return(weeks)
			}else{
				return(paste(weeks, " weeks", sep=""))
			}
		}
		if(units=="y" | units=="years"){
			if(show.units==FALSE){
				return(years)
			}else{
				return(paste(years, " years", sep=""))
			}
		}
		cat("Error:  Unrecognised unit type '", units, "'\n", sep="")
	}
		
	if(secs < 60){
		if(show.units==FALSE){
			return(secs)
		}else{
			return(paste(secs, " seconds", sep=""))
		}
	}
	if(mins < 60){
		if(show.units==FALSE){
			return(mins)
		}else{
			return(paste(mins, " minutes", sep=""))
		}
	}
	if(hours < 24){
		if(show.units==FALSE){
			return(hours)
		}else{
			return(paste(hours, " hours", sep=""))
		}
	}
	if(days < 7){
		if(show.units==FALSE){
			return(days)
		}else{
			return(paste(days, " days", sep=""))
		}
	}
	if(weeks < 52){
		if(show.units==FALSE){
			return(weeks)
		}else{
			return(paste(weeks, " weeks", sep=""))
		}
	}
	
	if(show.units==FALSE){
		return(years)
	}else{
		return(paste(years, " years", sep=""))
	}
	
}