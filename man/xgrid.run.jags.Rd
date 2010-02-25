\name{xgrid.run.jags}
\alias{xgrid.autorun.jags}
\alias{xgrid.autorun.JAGS}
\alias{xgrid.autorun.jagsfile}
\alias{xgrid.autorun.JAGSfile}
\alias{xgrid.run.jags}
\alias{xgrid.run.JAGS}
\alias{xgrid.run.jagsfile}
\alias{xgrid.run.JAGSfile}
\alias{xgrid.submit.jags}
\alias{xgrid.submit.JAGS}
\alias{xgrid.submit.jagsfile}
\alias{xgrid.submit.JAGSfile}
\alias{xgrid.results.jags}
\alias{xgrid.results.JAGS}


\title{Run a JAGS Model using an Xgrid distributed computing cluster
from Within R}

\description{

Extends the functionality of the (auto)run.jags(file) family of
functions to use with Apple Xgrid distributed computing clusters.  Jobs
can either be run synchronously using xgrid.(auto)run.jags(file) in
which case the process will wait for the model to complete before
returning the results, or asynchronously using xgrid.submit.jags(file)
in which case the process will terminate on submission of the job and
results are retrieved at a later time.  Access to an Xgrid cluster with
JAGS (although not necessarily R) installed is required.  Due to the
dependance on Xgrid software to perform the underlying submission and
retrieval of jobs, these functions can only be used on machines running
Mac OS X.  Further details of required environmental variables and the
optional mgrid script to enable multi-task jobs can be found in the
details section.

}

\usage{

xgrid.run.jags(wait.interval="10 min", xgrid.method='simple',
jagspath='/usr/local/bin/jags', jobname=NA, cleanup=TRUE,
sub.app=if(Sys.which('mgrid')=="") 'xgrid -job submit'
else 'mgrid -t $ntasks', sub.options="",
sub.command=paste(sub.app, sub.options, '-i $indir $cmd', sep=' '),
...)

xgrid.run.jagsfile(wait.interval="10 min", xgrid.method='simple',
jagspath='/usr/local/bin/jags', jobname=NA, cleanup=TRUE,
sub.app=if(Sys.which('mgrid')=="") 'xgrid -job submit'
else 'mgrid -t $ntasks', sub.options="",
sub.command=paste(sub.app, sub.options, '-i $indir $cmd', sep=' '),
...)

xgrid.autorun.jags(wait.interval="10 min", xgrid.method='simple',
jagspath='/usr/local/bin/jags', jobname=NA, cleanup=TRUE,
sub.app=if(Sys.which('mgrid')=="") 'xgrid -job submit'
else 'mgrid -t $ntasks', sub.options="",
sub.command=paste(sub.app, sub.options, '-i $indir $cmd', sep=' '),
...)

xgrid.autorun.jagsfile(wait.interval="10 min", xgrid.method='simple',
jagspath='/usr/local/bin/jags', jobname=NA, cleanup=TRUE,
sub.app=if(Sys.which('mgrid')=="") 'xgrid -job submit'
else 'mgrid -t $ntasks', sub.options="",
sub.command=paste(sub.app, sub.options, '-i $indir $cmd', sep=' '),
...)

xgrid.submit.jags(xgrid.method='simple', jagspath='/usr/local/bin/jags',
jobname=NA, cleanup=TRUE, sub.app=if(Sys.which('mgrid')=="")
'xgrid -job submit' else 'mgrid -t $ntasks', sub.options="",
sub.command=paste(sub.app, sub.options, '-i $indir $cmd', sep=' '),
...)

xgrid.submit.jagsfile(xgrid.method='simple',
jagspath='/usr/local/bin/jags',
jobname=NA, cleanup=TRUE, sub.app=if(Sys.which('mgrid')=="")
'xgrid -job submit' else 'mgrid -t $ntasks', sub.options="",
sub.command=paste(sub.app, sub.options, '-i $indir $cmd', sep=' '),
...)

xgrid.results.jags(jobname)

}



\arguments{

\item{wait.interval}{when running xgrid jobs synchronously, the waiting
time between retrieving the status of the job.  If the job is found to
be finished on retrieving the status then results are returned,
otherwise the function waits for 'wait.interval' before repeating the
process.  Time units of seconds, minutes, hours, days or weeks can be
specified.  If no units are given the number is assumed to represent
minutes.  Default "10 min".}

\item{xgrid.method}{the method of submitting the work to Xgrid - one of
'simple', 'separatejobs' or 'separatetasks'.  The former runs all chains
on a single node, whereas 'separatejobs' runs all chains as individual
xgrid jobs and 'separatetasks' runs all chains as individual tasks
within the same job (this makes the job information in Xgrid Admin
easier to read).  The latter method requires a submission script that is
capable of supporting multi-task jobs, such as the mgrid script included
with the runjags package (see the details section for more details and
installation instructions).  If each chain is likely to return a large
amount of information then 'separatejobs' should be used; this is because jobs
are retrieved individually which reduces the chances of overloading the
Xgrid controller.  Default 'simple'.}

\item{jagspath}{the path to the JAGS executable on the xgrid machines.
Note that /usr/local/bin is not included in the path when running Xgrid
jobs, so it is safer to provide the full path.  If not all machines on
the xgrid cluster have JAGS installed then it is possible to use an ART
script to ensure the job is sent to only machines that do - see the
examples section for details.  Default '/usr/local/bin/jags' (this is
the default install location for JAGS).}

\item{jobname}{for all functions except xgrid.results.jags, the jobname
can be provided to make identification of the job using Xgrid Admin
easier.  If none is provided, then one is generated using a combination
of the username and hostname of the submitting machine.  If the provided
jobname is already used by a file/folder in the working directory, then
the name is altered to be unique using new_unique().  For
xgrid.results.jags, the jobname must be supplied to match the jobname
value returned by xgrid.submit.jags(file) during job submission.}

\item{cleanup}{option to delete the job(s) from Xgrid after retrieving
result.  Default TRUE.}

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
case each job is submitted using a different application/script. Default
uses mgrid if installed, otherwise 'xgrid -job submit'.}

\item{sub.options}{one or more option flags to be passed through to the
submission application (as a character string).  Examples include ART
scripts, email on job completion, and when using the mgrid script many
other possibilities (see the details section).  When providing links to
files as part of the command, all links must be absolute (ie start with
/ or ~) as xgrid/mgrid will be will not be called in th working
directory.  If xgrid.method is 'separatejobs' then the argument may be
of length equal to the number of chains, in which case each job receives
a different set of options.  Some options require the Xgrid controller
to be running OS X Leopard (10.5) or later.  Default none.}


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
chains, in which case each job receives a different command.  Default
uses the values of sub.app and sub.options.}

\item{...}{other options to be passed to the (auto)run.jags(file)
functions as if the model were being run locally.}

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
 To install (optional), open the Terminal and change directory to the
'xgrid' folder inside the package.  Then run the following commands to
install mgrid in /usr/local/bin and create the support folder in
'Application Support' (an Administrator's password will be required):

sudo chmod 755 mgrid.sh

sudo cp mgrid.sh /usr/local/bin/mgrid

sudo mkdir "/Library/Application Support/mgrid/"

Alternatively, to do this automatically (and assuming runjags is
installed in the default library), run the following code in R, then
paste the contents of the clipboard into the opened Terminal window and
hit enter followed by an Administrator's password:

clip <- pipe("pbcopy", "w"); cat(paste('cd ', .Library, '/runjags/xgrid; sudo chmod 755 mgrid.sh; sudo cp mgrid.sh /usr/local/bin/mgrid; if [ ! -d "/Library/Application Support/mgrid" ]; then sudo mkdir "/Library/Application Support/mgrid"; fi;', sep=""), file=clip); close(clip); system('open -n -a /Applications/Utilities/Terminal.app')

You can then look at the possible arguments to mgrid by typing 'mgrid'
in the Terminal or system('mgrid') within R.

}



\value{For xgrid.submit.jags and xgrid.submit.jagsfile, a list
containing the jobname (which will be required by xgrid.results.jags to
retrieve the job) and the job ID(s) for use with the xgrid command line
facilities.  For all other functions, the results of the simulation are
returned as with the respective (auto)run.jags(file) functions.

}



\seealso{

\code{\link{run.jags}}, \code{\link{autorun.jags}} and
\code{\link{run.jagsfile}} for more information on JAGS models.

\code{\link{xgrid.run}} for functions to execute user-specified
functions on Xgrid.

}



\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk}}


\examples{

# run a simple model on Xgrid using a single job:

\dontrun{

# Ensure the required environmental variables are set:
Sys.setenv(XGRID_CONTROLLER_HOSTNAME="<hostname>")
Sys.setenv(XGRID_CONTROLLER_PASSWORD="<password>")

# Simulate the data
X <- 1:100
Y <- rnorm(length(X), 2*X + 10, 1)

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

# Run the model synchronously using the 'simple' method and a wait interval of 1 minute:
results <- xgrid.run.jags(xgrid.method='simple', wait.interval='1 min', model=model, monitor=c("m", "c", "precision"), data=data, n.chains=2, plots = FALSE)

# Analyse the results:
results$summary

}


# Submit a job to xgrid and (later) retrieve the results.  Use an ART script to ensure the job is only sent to nodes with JAGS installed:

\dontrun{

# Ensure the required environmental variables are set:
Sys.setenv(XGRID_CONTROLLER_HOSTNAME="<hostname>")
Sys.setenv(XGRID_CONTROLLER_PASSWORD="<password>")

# Create the ART script we need to ensure JAGS is installed:
cat('#!/bin/bash\nif [ -f /usr/local/bin/jags ]; then echo 1; else echo 0; fi\n', file='jagsART.sh')

# Simulate the data
X <- 1:100
Y <- rnorm(length(X), 2*X + 10, 1)

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

# Run the model asynchronously:
name <- xgrid.submit.jags(xgrid.method='separatejobs', sub.options=paste('-a ' , getwd(), '/jagsART.sh', sep=''), model=model, monitor=c("m", "c", "precision"), data=data, n.chains=2, plots = FALSE)

# Cleanup (remove jagsART file):
unlink('jagsART.sh')

# Retrieve the results:
results <- xgrid.results(name)
}



# Autorun a model to convergence using separate tasks on xgrid.  Ensure the tasks are sent to the 2 fastest nodes (called 'Bugati' and 'McLaren') in our (fictional) cluster using arguments to mgrid.

\dontrun{

# Ensure the required environmental variables are set:
Sys.setenv(XGRID_CONTROLLER_HOSTNAME="<hostname>")
Sys.setenv(XGRID_CONTROLLER_PASSWORD="<password>")

# Ensure mgrid is installed:
stopifnot(Sys.which('mgrid')!="")

# Simulate the data
X <- 1:100
Y <- rnorm(length(X), 2*X + 10, 1)

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

# Run the model synchronously using the 'separatetasks' method and a wait interval of 1 minute:
results <- xgrid.autorun.jags(xgrid.method='separatetasks', wait.interval='1 min', sub.options='-h "Bugati:McLaren" -f', model=model, monitor=c("m", "c", "precision"), data=data, n.chains=2, plots = FALSE)

}





}

\keyword{models}