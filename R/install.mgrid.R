install.mgrid <- function(libpath=.Library, ask=TRUE){

if(.Platform$OS.type=='windows'){
	stop('xgrid functions are only available on machines running Mac OS X and with access to an Xgrid controller')
}

xgridavail <- system('xgrid 2>&1', intern=TRUE)
if(length(xgridavail)==1){
	stop('xgrid is not available on this machine.  mgrid was not installed')
}

versionstring <- try(system('mgrid -? 2>&1', intern=TRUE)[2])
if(class(versionstring)=='try-error'){
	oldversion <- '1.x'
}else{
	if(is.na(versionstring)){
		oldversion <- 0
	}else{
		if(versionstring=='Usage:'){
			oldversion <- '2.x'
		}else{
			oldversion <- (strsplit(gsub('mgrid -- version ', '', versionstring), ',')[[1]][1])
		}
	}
}

sudoa <- length(grep('-A', system('sudo -? 2>&1', intern=TRUE)))>0

if(.Platform$GUI=='AQUA'){
	cat("An administrator's password is required to ensure the mgrid script is executable and to copy it to /usr/local/bin/ where it can be accessed.  Since you are currently using the GUI version of R with no tty interface, you will have to either enter your password in clear text below or type 'exit' and re-run this function from the command line version of R.  Alternatively, if you would prefer to install mgrid yourself type 'exit' and then copy the script at 'runjags/inst/xgrid/mgrid.sh' to '/usr/local/bin/mgrid', ensuring that the file is executable.  Otherwise, please enter your password below.\n")
	pass <- readline('sudo (super-user do) password:  ')
	pass <- paste(pass, '\n', sep='')
	if(tolower(pass)=='exit') stop('The process was aborted by the user')
	if(sudoa) out1 <- system(paste('sudo -S chmod 755 ', libpath, '/runjags/xgrid/mgrid.sh', sep=''), input=pass, intern=TRUE) else out1 <- system(paste('sudo chmod 755 ', libpath, '/runjags/xgrid/mgrid.sh', sep=''), input=pass, intern=TRUE)
	versionstring <- system(paste(libpath, '/runjags/xgrid/mgrid.sh -? 2>&1', sep=""), intern=TRUE)[2]
}else{
	cat("An administrator's password is required to ensure the mgrid script is executable
and to copy it to /usr/local/bin/ where it can be accessed.  If you would
prefer to install mgrid yourself type control+c and then copy the script at
'runjags/inst/xgrid/mgrid.sh' to '/usr/local/bin/mgrid', ensuring that the
file is executable.  Otherwise, please enter your password at the sudo prompt.\n")
	versionstring <- system(paste('sudo chmod 755 ', libpath, '/runjags/xgrid/mgrid.sh; ', libpath, '/runjags/xgrid/mgrid.sh -? 2>&1', sep=""), intern=TRUE)[2]
}

cat('\n')
newversion <- (strsplit(gsub('mgrid -- version ', '', versionstring), ',')[[1]][1])

if(ask){
	if(oldversion==0) cat('mgrid is not currently installed\n') else cat('Version ', oldversion, ' of mgrid is currently installed\n', sep='')
	cat('Version ', newversion, ' available at ', libpath, '/runjags/xgrid/mgrid.sh\n', sep='')
	instok <- ask(prompt=paste('Install version ', newversion, ' to "/usr/local/bin/mgrid" ?\n', sep=''), type='logical', na.allow=FALSE)
}else{
	instok <- TRUE
}

if(instok){
	if(.Platform$GUI=='AQUA'){
		if(sudoa) out <- system(paste('sudo -S cp ', libpath, '/runjags/xgrid/mgrid.sh /usr/local/bin/mgrid', sep=""), input=pass, intern=TRUE) else out <- system(paste('sudo cp ', libpath, '/runjags/xgrid/mgrid.sh /usr/local/bin/mgrid', sep=""), input=pass, intern=TRUE)
	}else{
		out <- system(paste('sudo cp ', libpath, '/runjags/xgrid/mgrid.sh /usr/local/bin/mgrid', sep=""), intern=TRUE)
	}
	cat('mgrid version ', newversion, ' installed successfully.  Type "mgrid" from the command line\nor "system(\'mgrid\')" from R to see the manual page\n', sep='')
	invisible(paste('Version ', newversion, ' installed', sep=''))
}else{
	cat('Installation aborted\n')
	invisible('Installation aborted')
}

}
