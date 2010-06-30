\name{xgrid.run}
\alias{xgrid.run}
\alias{xgrid.submit}
\alias{xgrid.results}

\title{Remote execution of user-specified R functions on Apple Xgrid distributed computing clusters}
\description{
   Allows arbitrary R code to be executed on Apple Xgrid distributed
   computing clusters and the results returned to the R session of the
   user.  Jobs can either be run synchronously (the process will wait
   for the model to complete before returning the results) or
   asynchronously (the process will terminate on submission of the job
   and results are retrieved at a later time).  Access to an Xgrid
   cluster with R (along with all packages required by the function)
   installed is required.  Due to the dependance on Xgrid software to
   perform the underlying submission and retrieval of jobs, these
   functions can only be used on machines running Mac OS X.  Further
   details of required environmental variables and the optional mgrid
   script to enable multi-task jobs can be found in the details section.

   'xgrid.run' submits jobs to Xgrid that execute the function provided
   over the number of iterations specified, then intermittently
   retrieves the status of the job(s) and, if finished, retrieving and
   returning the results as an R list object.

   'xgrid.submit' submits the job to xgrid, and returns the name of the
   started job.

   'xgrid.results' returns the results of a job started using
   'xgrid.submit' in the current working directory.
}
\usage{

xgrid.run(f=function(iteration){}, niters, 
   object.list=list(), file.list=character(0), 
   threads=min(niters,100), jobname=NA, wait.interval="10 min", 
   xgrid.method=if(threads==1) 'simple' else if(Sys.which('mgrid')=="") 
   'separatejobs' else 'separatetasks', Rpath='/usr/bin/R', 
   cleanup = TRUE, submitandstop = FALSE, tempdir=!submitandstop, 
   keep.files = FALSE, show.output = TRUE, max.filesize="1GB", 
   sub.app=if(Sys.which('mgrid')=="") 'xgrid -job submit -in "$indir"' 
   else 'mgrid -t $ntasks -i "$indir"', sub.options="", 
   sub.command=paste(sub.app, sub.options, '"$cmd"', sep=' '), ...)

xgrid.submit(f=function(iteration){}, niters, 
   object.list=list(), file.list=character(0), 
   threads=min(niters,100), jobname=NA, 
   xgrid.method=if(threads==1) 'simple' else if(Sys.which('mgrid')=="") 
   'separatejobs' else 'separatetasks', Rpath='/usr/bin/R', 
   show.output = TRUE, max.filesize="1GB", 
   sub.app=if(Sys.which('mgrid')=="") 'xgrid -job submit -in "$indir"'
   else 'mgrid -t $ntasks -i "$indir"', sub.options="", 
   sub.command=paste(sub.app, sub.options, '"$cmd"', sep=' '), ...)

xgrid.results(jobname, cleanup=TRUE, keep.files=FALSE, show.output=TRUE)
}

\arguments{

   \item{f}{the function to be iterated over on Xgrid.  This must take
   exactly 1 argument, which represents the iteration number.  The
   value(s) of interest should be returned by this function (an object
   of any class is permissable).  No default.}

   \item{niters}{the total number of iterations over which to evaluate
   the function f.  This can be less than the number of threads, in
   which case multiple iterations are evaluated serially as part of the
   same task.  No default.}

\item{object.list}{a named list of objects that will be copied to the
global environment on Xgrid and so will be visible inside the function. 
Alternatively, this can be a character vector of objects, that will be
looked for in the global environment, rather than a named list.  All
other objects in the current working directory will not be visible when
the function is evaluated. THIS INCLUDES LIBRARIES WHICH MUST BE
RE-CALLED WITHIN THE FUNCTION BEFORE USE.  In order to use functions
within an R library it is therefore necessary for the required library
to be installed on the Xgrid nodes on which the job will be run.  If not
all nodes have the required libraries installed, you can use an ART
script to ensure the job is sent only to machines that do (see the
example provided below), or you can use mgrid to manually request
certain nodes using the '-f -h <nodename>' options. Alternatively, text
files containing R code can be included in the 'file.list' argument and
source()d within the function. Default blank list (no objects copied).}

   \item{file.list}{a vector of filenames representing files in the
   current working directory that will be copied to the working
   directory of the executed function.  This allows R code to be
   source()d, datasets to be loaded, and compiled code to be dynamically
   linked within the function, among other things.  Default none.}

   \item{threads}{the number of threads to generate for the job. 
   Threads is taken to mean jobs if xgrid.method is 'separatejobs' or
   tasks if xgrid.method is 'separatetasks'.  Each thread is sent to a
   separate node for execution, so the more threads there are the faster
   the job will finish (unless the number of threads exceeds the number
   of available nodes).  A very large number of threads may cause
   problems with the Xgrid controller, hence the ability to set fewer
   threads than iterations.  Functions that return objects of a very
   large size should use a large number of threads and use the
   xgrid.method 'separatejobs' to minimise the total size of objects
   returned by each xgrid job.  Default is equal to the number of
   iterations if this is less than 100, or 100 otherwise.}

   \item{jobname}{ for all functions except xgrid.results.jags, the
   jobname can be provided to make identification of the job using Xgrid
   Admin easier.  If none is provided, then one is generated using a
   combination of the username and hostname of the submitting machine.
   If the provided jobname is already used by a file/folder in the
   working directory, then the name is altered to be unique using
   new_unique().  For xgrid.results.jags, the jobname must be supplied
   to match the jobname value returned by xgrid.submit.jags(file) during
   job submission.}

   \item{wait.interval}{when running xgrid jobs synchronously, the
   waiting time between retrieving the status of the job.  If the job is
   found to be finished on retrieving the status then results are
   returned, otherwise the function waits for 'wait.interval' before
   repeating the process.  Time units of seconds, minutes, hours, days
   or weeks can be specified.  If no units are given the number is
   assumed to represent minutes.  Default "10 min".}

   \item{xgrid.method}{the method of submitting the work to Xgrid - one
   of 'simple', 'separatejobs' or 'separatetasks'.  The former runs all
   chains on a single node, whereas 'separatejobs' runs all chains as
   individual xgrid jobs and 'separatetasks' runs all chains as
   individual tasks within the same job (this makes the job information
   in Xgrid Admin easier to read).  The latter method requires a
   submission script that is capable of supporting multi-task jobs, such
   as the mgrid script included with the runjags package (see the
   details section for more details and installation instructions).  If
   each chain is likely to return a large amount of information then
   'separatejobs' should be used; this is because jobs are retrieved individually
   which reduces the chances of overloading the Xgrid controller.
   Default 'simple' if threads==1; otherwise 'separatetasks' if mgrid is
   available or 'separatejobs' if not.}

   \item{Rpath}{the path to the R executable on the xgrid machines. If
   not all machines on the xgrid cluster have R (or a required package)
   installed then it is possible to use an ART script to ensure the job
   is sent to only machines that do - see the examples section for
   details.  Default '/usr/bin/R' (this is the default install location
   for R).}

   \item{cleanup}{option to delete the job(s) from Xgrid after
   retrieving result.  Default FALSE.}

   \item{submitandstop}{controls whether job should be run synchronously
   (submitandstop=FALSE), in which case the process will wait for the
   model to complete before returning the results, or asynchronously
   (submitandstop=TRUE), in which case the process will terminate on
   submission of the job and results are retrieved at a later time. 
   Default for xgrid.run is FALSE.  xgrid.submit is a wrapper to
   xgrid.run with submitandstop=TRUE.}

   \item{tempdir}{for xgrid.run, option to use the temporary directory
   as specified by the system rather than creating files in the working
   directory.  Any files created in the temporary directory are removed
   when the function exits.  A temporary directory cannot be used for
   xgrid.submit.  Default TRUE when running the job synchronously.}

\item{keep.files}{option to keep the folder with files needed to run the
job rather than deleting it, or copy the folder to the working directory
before exiting if tempdir=TRUE.  This may be useful for attempting to
bug fix failing jobs.  Default FALSE.}

   \item{show.output}{option to print the output of the function
   (obtained using cat, writeLine or print for example) at each
   iteration after retrieving the job(s) from xgrid.  If FALSE, the
   output is suppressed.  Default TRUE.}
  
   \item{max.filesize}{the maximum total size of the objects produced by
   the function for each thread if xgrid.method=separatejobs, or for the
   entire job if xgrid.method=separatetasks.  This is a failsafe
   designed to prevent attempted transfer of huge files bringing the
   xgrid controller down.  If the maximum size is exceeded for a thread
   or job then the results are erased for all iterations within that
   thread or job, and the job will likely have to be re-submitted.  If
   each chain is likely to return a large amount of information, then
   'separatejobs' should be used because jobs are retrieved individually
   which reduces the chances of overloading the Xgrid controller.  The
   object.list is also checked to ensure it complies with the maximum
   size, but the file.list and any objects saved to the working
   directory by the function are NOT automatically cheked.  Units can be
   provided as either "MB" or "GB".  Default "1GB".}

\item{sub.app}{the submission application or script to use for job
running/submission.  The inbuilt Xgrid application supports most
options, but greater functionality is provided by the mgrid script (see
the details section for more information and installation instructions).
 Any other custom script can be used with the requirements that it
submit the job provided and print the Xgrid job ID to screen before
exiting (as the only numerical value printed), or alternatively the
script may submit the job and create a 'jobid.txt' file in the working
directory containing the job id.  If xgrid.method is 'separatejobs' then
the argument may be of length equal to the number of chains, in which
case each job is submitted using a different application/script. Paths
with spaces in them must be quoted when the command is passed to the
shell (this may mean escaping quotes if necessary).  Default uses mgrid
if installed, otherwise 'xgrid -job submit'.}

\item{sub.options}{one or more option flags to be passed through to the
submission application (as a character string).  Examples include ART
scripts, email on job completion, and when using the mgrid script many
other possibilities (see the details section).  When providing links to
files as part of the command, all links must be absolute (ie start with
/ or ~) as xgrid/mgrid will be will not be called in the working
directory, and paths with spaces must be quoted.  If xgrid.method is
'separatejobs' then the argument may be of length equal to the number of
chains, in which case each job receives a different set of options. 
Some options require the Xgrid controller to be running OS X Leopard
(10.5) or later.  Default none.}

\item{sub.command}{the actual command to be executed using system() to
submit the job.  Changing this results in sub.app and sub.options being
ignored, and is probably the best option to use for custom submission
scripts (see the sub.app argument for the requirements for custom
scripts).  The environmental variables $cmd (the name of the BASH script
to be run), $ntasks (the number of tasks), $job (the job number for
multiple jobs), and $indir (the input directory) will be available to
the script.  For multiple tasks, the custom script should ensure that
the task number is supplied as the (only) argument to the BASH script
(requires xgrid.method="separatetasks" to function).  If xgrid.method is
'separatejobs' then the argument may be of length equal to the number of
chains, in which case each job receives a different command.  Paths with
spaces in them must be quoted when the command is passed to the shell
(this may mean escaping quotes if necessary).  Default uses the values
of sub.app and sub.options.}

   \item{...}{objects to be passed to the function provided (equivalent
   to specifying the objects in the object.list).} 

}



\details{

These functions allow JAGS models to be run on Xgrid distributed
computing clusters from within R using the same syntax as required to
run the models locally.  All the functionality could be replicated by
saving all necessary objects to files and using the Xgrid command line
utility to submit and retrieve the job manually; these functions merely
provide the convenience of not having to do this manually.  Xgrid
support is only available on Mac OS X machines.  

The xgrid controller hostname and password must be set as environmental
variables. The command line version of R knows about environmental
variables set in the .profile file, but unfortunately the GUI version
does not and requires them to be set from within R using:


Sys.setenv(XGRID_CONTROLLER_HOSTNAME="<hostname>")

Sys.setenv(XGRID_CONTROLLER_PASSWORD="<password>")

(These lines could be copied into your .Rprofile file for a 'set and
forget' solution)


All functions can be run using the built-in xgrid commands, however some
added functionality (including multi-tasks jobs to enable the
'separatetasks' method) is provided by the 'mgrid.sh' BASH shell script
which is included with the runjags package (in the 'inst/xgrid' folder
for the package source or the 'xgrid' folder for the installed package).
More details about this script is given at the top of the mgrid.sh file.
 To install (optional), see the \code{\link{install.mgrid}} function.

}



\value{For xgrid.submit, a list containing the jobname (which will be
required by xgrid.results to retrieve the job) and the job ID(s) for use
with the xgrid command line facilities.  For xgrid.run and
xgrid.results, the output of the function over all iterations is
returned as a list, with each element of the list representing the
results at each iteration.  If the function returned an error, then the
error will be held in the list as the return value at the iteration that
returned the error.  If the function returns an object that exceeds the
'max.filesize' when combined with the results for other iterations in
that job (or greater than max.filesize/threads for multi-task jobs), the
results for that thread are replaced with an error message (this is to
prevent the xgrid controller crashing due to transferring large files).

}



\seealso{

\code{\link{xgrid.run.jags}} for functions to run JAGS models on Xgrid,
or \code{\link{run.jags}} to do so locally.

\code{\link{install.mgrid}} to install the mgrid script.

mclapply and mcparallel in the multicore package for parallel execution
of code over multiple local cores.

}



\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk}}



\examples{

# A basic example of synchronous running of code over 100 iterations, 
# split up between 10 tasks (or 10 jobs if mgrid is not installed):

\dontrun{

# The function to evaluate:
f <- function(iteration){
	# All objects supplied to object.list will be visible here, but
	# remember to call all necessary libraries within the function
	
	cat("Running iteration", iteration, "\n")
	# Some lengthy code evaluation....
	
	output <- rpois(10, iteration)
	return(output)
}

# Run the function on xgrid for 100 iterations split between 10 machines:
results <- xgrid.run(f, niters=100, threads=10)

}


# Any packages required by the function need to be installed on the
# nodes the function is run on.  This function retrieves information
# about the available packages on each of the node names provided:

\dontrun{

# The name of one or more nodes to get information about:
nodenames <- c("mynode", "guestnode", "othernode")

# Run the job:
results <- xgrid.run(function(i){
		return(installed.packages()[,'Version'])
	}, 
	niters=length(nodenames), threads=length(nodenames), 
	wait.interval="10 seconds", xgrid.method='separatejobs',
	sub.options=paste("-f -h '", nodenames, "'", sep=""),
	show.output=FALSE)
# Make the names match up to the statistics:
names(results) <- nodenames

# Show the available packages and their versions for each node:
results

}


# An example of running an Xgrid job within another Xgrid job, using 
# xgrid.submit to submit a job that runs a JAGS model to convergence 
# using xgrid.autorun.jags:

\dontrun{

# Create an ART script to make sure that (a) R is installed, 
# (b) JAGS is installed, and (c) the runjags package is installed 
# on the node:
cat('#!/bin/bash
#!/bin/bash
if [ ! -f /usr/bin/R ]; then 
echo 0
exit 0
fi
if [ ! -f /usr/local/bin/jags ]; then 
echo 0
exit 0
fi
/usr/bin/R --slave -e "suppressMessages(rjsuccess <- require(runjags, quietly=TRUE)); writeLines(as.character(rjsuccess*1))"
exit 0
', file='runjagsART.sh')

# Some data etc we will need for the model:
library(runjags)

X <- 1:100
Y <- rnorm(length(X), 2*X + 10, 1)
data <- dump.format(list(X=X, Y=Y, N=length(X)))

# Model in the JAGS format
model <- "model {
for(i in 1 : N){
Y[i] ~ dnorm(true.y[i], precision);
true.y[i] <- (m * X[i]) + c;
}
m ~ dunif(-1000,1000);
c ~ dunif(-1000,1000);
precision ~ dexp(1);
}"

# Get the Xgrid controller hostname and password to be passed 
# to the slave job:
hostname <- Sys.getenv('XGRID_CONTROLLER_HOSTNAME')
password <- Sys.getenv('XGRID_CONTROLLER_PASSWORD')

# The function we are going to call on xgrid:
f <- function(iteration){
	# Make sure the necessary environmental variables are set:
	Sys.setenv(XGRID_CONTROLLER_HOSTNAME=hostname)
	Sys.setenv(XGRID_CONTROLLER_PASSWORD=password)
	
	# Call the library on the node:
	library(runjags)
	
	# Use xgrid.autorun.jags to run 2 chains until convergence:
	results <- xgrid.autorun.jags(model=model, 
		monitor=c("m", "c", "precision"), data=data, n.chains=2, 
		plots = FALSE, xgrid.method='separatejobs', 
		wait.interval='1 min', jobname='xgridslavejob')
	
	return(results)
}

# Submit the function to xgrid using our ART script to ensure the 
# node can handle the job (the ART script path must be specified as 
# an absolute link as xgrid won't be called in the current working 
# directory, and all paths must be enclosed in quotes to preserve spaces):
name <- xgrid.submit(f, object.list=list(X=X, Y=Y, model=model, 
	data=data, hostname=hostname, password=password), threads=1, 
	niters=1, sub.options=if(Sys.which('mgrid')=="") paste('-art "', 
	getwd(), '/runjagsART.sh"', sep='') else paste('-a "', getwd(), 
	'/runjagsART.sh"', sep=''), xgrid.method='simple')
# Cleanup (remove runjagsART file):
unlink('runjagsART.sh')

# Get the results once it is finished:
results <- xgrid.results(name)$iteration.1

}


}

\keyword{methods}