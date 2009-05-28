combine.mcmc <- function(mcmc.objects=list(), thin=1, return.samples=NA, collapse.chains=FALSE){
	
	if(class(mcmc.objects)!="list"){
		if(any(class(mcmc.objects)==c("mcmc.list", "mcmc"))){
			mcmc.objects <- list(mcmc.objects)
		}else{
			stop("Data must be provided as a list of or single mcmc object(s), or a list of or single mcmc.list(s) (for multiple chains)")
		}
	}
	
	no.objects <- length(mcmc.objects)
	
	if(length(mcmc.objects)==0) stop("The list provided cannot be empty")
	
	n.chains <- integer(length=no.objects)
	n.params = rowlengths <- vector("list", length=no.objects)
	
	if(no.objects > 1){
		for(i in 1:no.objects){
		
			if(class(mcmc.objects[[i]])=="mcmc.list"){
				n.chains[i] <- length(mcmc.objects[[i]])
				returnlist <- TRUE
			}else{
				if(class(mcmc.objects[[i]])=="mcmc"){
					n.chains[i] <- 1
					mcmc.objects[[i]] <- mcmc.list(mcmc.objects[[i]])
					returnlist <- FALSE
				}else{
					stop("Data must be provided as a list of mcmc objects, or a list of mcmc.lists (for multiple chains).")	
				}
			}
		
			n.params[[i]] <- integer(length=n.chains[i])
			rowlengths[[i]] <- integer(length=n.chains[i])
		
			for(j in 1:n.chains[i]){
				n.params[[i]][j] <- nvar(mcmc.objects[[i]][[j]])
				rowlengths[[i]][j] <- niter(mcmc.objects[[i]][[j]])
			}
		
			if(!all(rowlengths[[i]] == rowlengths[[i]][1])) stop(paste("The chain lengths were not equal for object ", i, ".", sep=""))
			rowlengths[[i]] <- rowlengths[[i]][1]
			
		}
	
		rowlengths <- unlist(rowlengths)

		paramsequal <- all(unlist(lapply(n.params, function(x) if(all(x==n.params[[1]][1])) return(TRUE) else return(FALSE))))
	
		if(!(all(n.chains==n.chains[1]))) ("There was an unequal number of chains between mcmc objects")
		if(!paramsequal) stop("There was an unequal number of monitored variables (columns) between chains / mcmc objects")
	
		n.chains <- n.chains[1]
		n.params <- n.params[[1]][1]
	
		newobjects <- vector("list", length=n.chains)
		
		start.points <- numeric(no.objects)
		start.points[1] <- 1
		if(no.objects>2){
		for(i in 2:no.objects){
			start.points[i] <- start.points[i-1] + rowlengths[i-1]
		}
		}
		
		for(i in 1:n.chains){
		
			newobjects[[i]] <- matrix(NA, nrow=0, ncol=n.params, dimnames=list(NULL, dimnames(mcmc.objects[[1]][[1]])[[2]]))
		
			for(j in 1:no.objects){
			
				newobjects[[i]] <- rbind(newobjects[[i]], mcmc.objects[[j]][[i]])
				#newobjects[[i]][start.points[j]:(sum(rowlengths[1:j])),] <- as.matrix(window(mcmc.objects[[j]][[i]]))
			
			}
			
			dimnames(newobjects[[i]])[[1]] <-  1:nrow(newobjects[[i]])
			
			newobjects[[i]] <- as.mcmc(newobjects[[i]])

		}
		
		if(returnlist) newobjects <- as.mcmc.list(newobjects) else newobjects <- newobjects[[1]]
		
	}else{
		
		newobjects <- mcmc.objects[[1]]
	}
	
	if(collapse.chains & class(newobjects)=="mcmc.list"){
		class(newobjects) <- "list"
		newobjects <- combine.mcmc(newobjects)
	}

	rowlengths <- niter(newobjects)
		
	if(!is.na(return.samples)){
		if(return.samples > rowlengths){
			thin <- 1
		}else{
			thin <- rowlengths / (return.samples-1)
		}
	}
	
	currentthin <- thin(newobjects)
	thin <- floor(thin)*currentthin

	suppressWarnings(newobjects <- window(newobjects, thin=thin))

	
	return(newobjects)
	
}

combine.MCMC <- combine.mcmc