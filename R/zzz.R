.onLoad <- function(libname, pkgname){
	
	runjagsprivate$runjagsversion <- utils::packageDescription(pkgname, fields='Version')
	setopts <- mget('.runjags.options', envir=.GlobalEnv, ifnotfound=list(.runjags.options=NULL))[[1]]
	if(!is.null(setopts)){
		if(!is.list(setopts)){
			warning('Ignoring invalid (non-list) specification for .runjags.options on loading the runjags package', call.=FALSE)
		}else{
			newopts <- do.call('runjags.options', args=setopts)
		}
	}
}

.onAttach <- function(libname, pkgname){
	
	# This will be run after load if the package is attached:
	setopts <- mget('.runjags.options', envir=.GlobalEnv, ifnotfound=list(.runjags.options=NULL))[[1]]
	if(!is.null(setopts) && !runjags.getOption('silent.runjags')){
		packageStartupMessage(paste('Attaching runjags (version ', runjagsprivate$runjagsversion, ') and setting user-specified options', sep=''))
	}
}

