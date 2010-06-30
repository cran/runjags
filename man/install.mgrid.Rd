\name{install.mgrid}
\alias{install.mgrid}

\title{Install the mgrid script to extend the functionality of the Apple Xgrid related functions}
\description{
   This function installs the mgrid script included in the runjags
   package to /usr/local/bin/mgrid to allow the multiple task features
   of the xgrid related functions to be used.  The mgrid script also
   provides other useful options, see the details section.  This
   function will only run on Mac OS X, and is only useful for those
   wishing to access an Xgrid distributed computing cluster.

   Please note that installation of the mgrid script will require an
   administrators password.  If you are uncomfortable with entering this
   into an R function, then you can manually install the mgrid script by
   opening a Terminal at the runjags/xgrid folder and typing 'sudo chmod
   755 mgrid.sh' followed by 'sudo cp mgrid.sh /usr/local/bin/mgrid'
   
}
\usage{
install.mgrid(libpath=.Library, ask=TRUE)
}

\arguments{

\item{libpath}{the path to the library where runjags is installed. 
Unless specified when installing the package, this will be the default
provided by .Library.}

\item{ask}{option to confirm the old and new versions of mgrid and ask
to continue before overwriting the old version with the new version. 
Default TRUE.}

}



\details{

The xgrid related functions within runjags allow arbitrary R code to be run on Xgrid distributed
computing clusters from within R.  All the functionality could be
replicated by saving all necessary objects to files and using the Xgrid
command line utility to submit and retrieve the job manually; these
functions merely provide the convenience of not having to do this
manually.  Xgrid support is only available on Mac OS X machines.  

All functions can be run using the built-in xgrid commands, however some
added functionality (including multi-tasks jobs to enable the
'separatetasks' method) is provided by the 'mgrid.sh' BASH shell script
which is included with the runjags package (in the 'inst/xgrid' folder
for the package source or the 'xgrid' folder for the installed package).
This function automatically installs the mgrid script from this location.

The xgrid controller hostname and password must be set as environmental
variables. The command line version of R knows about environmental
variables set in the .profile file, but unfortunately the GUI version
does not and requires them to be set from within R using:


Sys.setenv(XGRID_CONTROLLER_HOSTNAME="<hostname>")

Sys.setenv(XGRID_CONTROLLER_PASSWORD="<password>")

(These lines could be copied into your .Rprofile file for a 'set and
forget' solution)

You can look at the possible arguments to mgrid by typing 'mgrid'
in the Terminal or system('mgrid') within R.  The output is also shown 
below for convinience:


mgrid -- version 3.01, June 2010

by Matthew Denwood (matthewdenwood@mac.com)

SYNOPSIS

A bash script that provides a replacement for xgrid -job submit using batch
file submission with support for multi-task jobs, scheduler hinting,
environmental variables, and built-in ART scripts for selection of intel or
ppc machines.

USAGE

mgrid [-s stdin] [-i indir] [-d jobid] [-e email-address] 
   [-a art-path | -z art-path] [-b batchname] [-c arch]
   [-v "env_variable=env_value [...]" ] [-q] [-h node_name] [-f] 
   [-r ram_required_(MB)] [-n name] [-t number_of_tasks] 
   cmd [arg1 [...] ['$task']]

mgrid -l [-m] [-u]

mgrid -?

OPTIONS

The following options are equivaent to those given for xgrid -job submit:

-s  Use supplied file as standard input (equivalent to -si)

-i  Input supplied directory (equivalent to -in)

-d  Wait for the dependant job ID to finish (equivalent to -dids, with the 
    limitation that only 1 dependant job can be specified)

-e  Use supplied email address to report status changes (equivalent to -email)

-a  Use supplied file as an ART script (equivalent to -art)

Other options for xgrid -job submit are not supported.  The following options 
are unique for this script:

-z  Use supplied file as an ART script, and also calculate the node ranking 
    scores using the supplied script rather than the inbuilt script or a script 
    in 'Application Support/mgrid/scoring.rb'.  The disadvantage of using an 
    ART script to calculate node ranking in this way is that for multi-task 
    jobs, the score is not altered to take into account previous task 
    allocations.  For an alternative method which preserves this feature see:
    'runjags/inst/xgrid/node_scoring_example.rb'.  For users outside the
    University of Glasgow or not using node ranking, the -r option is identical 
    to the -a option.  Supplying " " or "none" as the argument.
    disables node ranking and the ranking ART script for submission of this job.

-b  Produce a batch file with the specified file name and stop (does not 
    submit to xgrid).  This option also prints the (ranked) scores and nodes to
    screen.  If the batchname specified is "profile" (or "/dev/null") then
    more detailed information about the scoring is shown, no batch file
    is produced and cmd is ignored (and may be omitted).

-c  Ensure that jobs are run only on intel or ppc machines - should be 
    'intel' or 'ppc'.

-v  A space seperated string of environmental variable(s) in the form of 
    "var_one=value_one var_two=value_two" to be set locally before running the
    command.

-q  Wait for nodes running OS X server to become available rather than running 
    jobs on desktop nodes.

-h  Don't run the ranking script, and use the provided node name(s) instead.  
    For multiple tasks, separate node names with a colon (no space).  Entire 
    string must be quoted if any node name contains a space.  Colons in node
    names will produce either an error or unexpected results.  If the node name 
    does not match any of the available nodes then the first available node 
    will be chosen (unless the -f option is also specified in which case the 
    job/task will hang).  Supplying a blank node name (-h ) produces a job or 
    batch file with no schedule hinting (this also effectively turns off node 
    ranking for University of Glasgow users, although the ranking script is 
    still passed as an ART script to the job - see the -r option for a way
    to disable both).

-f  Force the controller to assign the job to the highest ranked (or specified 
    using -t) node by using an ART script rather than schedule hinting.  
    Note that ALL tasks will be run on the same node.

-r  The minimum amount of free RAM in MB that is required for the job.  If not 
    supplied, 10MB is used as a default amount to prevent machines that have 
    free processors but no free RAM from accepting jobs.

-n  The name to give the job (appears on xgrid admin etc).  If none is supplied,
    the command is used as the name.

-t  The number of tasks being run.  The arguments should include one containing
    '$task' which denotes the task number (this MUST be enclosed in single
    quotes or the $ sign escaped), otherwise the task number will be passed as 
    the last argument to the command.                                    

-l  Display a list of jobs currently on xgrid and exit.  The following two
    arguments can also be given:

    -m  Include the current status of the jobs.

    -u  Include the username and hostame of whoever submitted the job
        (provided the jobs were submitted using mgrid).

    All other arguments are ignored. 

-?  Print a help/usage message and exit.  All other arguments are ignored.

ARGUMENTS

cmd is the command to be run on xgrid, and the remaining arguments are passed
as arguments to this command.  The special argument '$task' is used to denote 
the task number, and is appended to (any) other commands if ntasks is specified 
(even if it is 1) and '$task' is not found among the other arguments.  The 
'$task' variable can be embedded in other text to form an argument that changes 
with the task number, for example mgrid -t 2 /usr/bin/cal -y '200$task' would 
print 2001 for task 1 and 2002 for task 2.  *NB* If using the '$task' special
variable in this way, ensure that the argument is enclosed in single quotes
(NOT double quotes), or use backslash to escape it (as in "\$"), as the 
shell will otherwise evaluate "$task" to "" on passing the argument to mgrid.

REQUIREMENTS

Requires XGRID_CONTROLLER_HOSTNAME and XGRID_CONTROLLER_PASSWORD to be set as 
environmental variables (these cannot currently be specified as arguments).

ABOUT

This script is a replacement for xgrid -job submit that provides some
extra features, including support for jobs with multiple tasks, use of 
environmental variables and easier targeting of the job to named nodes or 
ppc vs intel machines.  For use within the University of Glasgow only, the 
script also uses schedule hints to automatically target nodes with higher ART 
scores.  This is basically an ugly workaround for the fact that ART doesn't 
(currently) rank nodes correctly when prioritising jobs.  It is possible to 
implement this feature for use with other xgrid setups; please email me if you 
are interested in doing so. Please also feel free to email me if you would be 
willing to clean up and add better documention to my woefully cobbled together 
code so that it might be more useful to others....

For use with hydra.vet.gla.ac.uk only:

By default, an inbuilt script is used to rank the available nodes for job/task
allocation.  This scoring is performed based on the number of processors per
node, the number of jobs already running, and whether or not the node is a 
server (running Mac OS X server).  The first task is assigned to the node with 
the highest score before incrementing the number of jobs running on that node 
to account for the extra task, and re-calculating the score for that node.  
This process is then repeated for all remaining tasks.  Additional information 
detailing the amount of RAM, processor speed and 32/64 bit availability on the 
nodes is collected but not used by default, except for the amount of RAM free 
which is used to score a node 0 if it is less than the given minimum.  To change 
the way that tasks are distributed among the available nodes, modify the code as 
desired in the file included with the runjags package (found in:
'runjags/inst/xgrid/node_scoring_example.rb'), rename the file either
'scoring.rb' or 'scoring.sh', and save either in the working directory or in 
'/Library/Application Support/mgrid/'.  Alternatively, a simple scoring script
can be specified using the -r option, however this score will not be updated 
after assigning each task to account for the extra task, so all tasks will be 
assigned to the node with the highest score at the start.  If the number of 
tasks exceeds the number of available processors on that node, the remaining 
tasks will be allocated to nodes chosen by the xgrid controller.  For jobs with
only a single task this option should produce the same results as the more 
complex method described above.

NOTES

This script is distributed 'as is', both FREELY and WITHOUT CHARGE, under the
GNU general public license (see http://www.gnu.org/copyleft/gpl.html).  I am
therefore not liable for any damage to your computer, xgrid cluster, or sanity
caused by using it.  

If you find this script useful, or find any bugs, then feel free to email me
at matthewdenwood@mac.com.  Paypal donations to the same address are also
gratefully received and may encourage further development of the software....



}



\value{The version of mgrid installed is returned invisibly.
}



\seealso{

\code{\link{xgrid.run.jags}} for functions to run JAGS models on Xgrid,
and \code{\link{xgrid.run}} for running arbitrary functions on Xgrid.

}



\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk}}


\keyword{methods}