load.runjagsmodule <- function(){
	
	if(!require("rjags")) stop("The rjags package is required to use the internal runjags module - alternatively you can download a standalone version of the JAGS module from the sourceforge page at http://sourceforge.net/projects/runjags/")
		
	success <- try(load.module('runjags',system.file("libs", if(.Platform$r_arch!="") .Platform$r_arch else "", package="runjags")))
	
	if(class(success)=="try-error"){
		warning("The internal runjags module could not be found - if you installed this package from CRAN, please file a bug report to the package author")
		invisible(FALSE)
	}else{
		invisible(TRUE)
	}
}

load.runJAGSmodule <- load.runjagsmodule
