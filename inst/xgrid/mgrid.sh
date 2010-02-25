#!/bin/bash

##############################################################################################################
#                                                                                                            #
#     mgrid                                                                                                  #
#     Copyright 2010 Matthew Denwood - matthewdenwood@mac.com - Version 2.01 - February 2010                 #
#                                                                                                            #
#     This script is a replacement for xgrid -job submit that provides some extra features, including        #
#     support for jobs with multiple tasks, use of environmental variables and easier targeting of the job   #
#     to named nodes or ppc vs intel machines.  For use within the University of Glasgow only, the script    #
#     also uses schedule hints to automatically target nodes with higher ART scores.  This is basically an   #
#     ugly workaround for the fact that ART doesn't (currently) rank nodes correctly when prioritising jobs. #
#     It is possible to implement this feature for use with other xgrid setups; please email me if you are   #
#     interested in doing so. Please also feel free to email me if you would be willing to clean up and add  #
#     better documention to my woefully cobbled together code so that it might be more useful to others....  #
#                                                                                                            #
#     Requires XGRID_CONTROLLER_HOSTNAME and XGRID_CONTROLLER_PASSWORD to be set as environmental variables. #
#     You should also put this script in eg /usr/local/bin/ and create a folder for the local copy of the    #
#     node list (University of Glasgow only) by issuing the following commands in the Terminal:              #
#                                                                                                            #
#     [cd <path_to_download_folder/>runjags/inst/xgrid/]                                                     #
#     sudo cp mgrid.sh /usr/local/bin/mgrid                                                                  #
#     [sudo mkdir "/Library/Application Support/mgrid/"]                                                     #
#                                                                                                            #
#     The script will then be accessible to all functions in the runjags package for R, and invokable from   #
#     the command line by using the command 'mgrid' (see the options by typing 'mgrid -?').  This is NOT     #
#     done as part of the default package install process, and will require an administrator's password.     #
#                                                                                                            #
#     For use with hydra.vet.gla.ac.uk only:                                                                 #
#     By default, an inbuilt script is used to rank the available nodes for job/task allocation.  This       #
#     scoring is performed based on the number of processors per node, the number of jobs already running,   #
#     and whether or not the node is a server (running Mac OS X server).  The first task is assigned to the  #
#     node with the highest score before incrementing the number of jobs running on that node to account for #
#     the extra task, and re-calculating the score for that node.  This process is then repeated for all     #
#     remaining tasks.  Additional information detailing the amount of RAM, processor speed and 32/64 bit    #
#     availability on the nodes is collected but not used by default.  To change the way in which the        #
#     tasks are distributed among the available nodes, modify the code as desired in the file included with  #
#     the runjags package (found in 'runjags/inst/xgrid/node_scoring_example.rb'), rename the file           #
#     'scoring.rb/sh', and save either in the working directory or in "/Library/Application Support/mgrid/". #
#     Alternatively, a simple scoring script can be specified using the -r option, however this score will   #
#     not be updated after assigning each task to account for the extra task, so all tasks will be assigned  #
#     to the node with the highest score at the start.  If the number of tasks exceeds the number of         #
#     available processors on that node, the remaining tasks will be allocated to nodes chosen by the xgrid  #
#     controller.  For jobs with only a single task this option should produce the same results as the more  #
#     complex method described above.                                                                        #
#                                                                                                            #
#     USAGE                                                                                                  #
#     mgrid [-s stdin] [-i indir] [-d jobid] [-e email-address] [-a art-path | -r art-path] [-b batchname]   #
#     [-c arch] [-v "env_variable=env_value [...]" ] [-q] [-h "node_name_task1[:node_name_task2...]"] [-f]   #
#     [-n name] [-t number_of_tasks] cmd [arg1 [...] ['$task']]                                              #
#     mgrid -l [-m] [-u]                                                                                     #
#     mgrid -?                                                                                               #
#     (Use 'mgrid' with no arguments to see the manual page)                                                 #
#                                                                                                            #
#     OPTIONS                                                                                                #
#     The following options are equivaent to those given for xgrid -job submit:                              #
#     -s  Use supplied file as standard input (equivalent to -si)                                            #
#     -i  Input supplied directory (equivalent to -in)                                                       #
#     -d  Wait for the dependant job ID to finish (equivalent to -dids, with the limitation that only 1      #
#         dependant job can be specified)                                                                    #
#     -e  Use supplied email address to report status changes (equivalent to -email)                         #
#     -a  Use supplied file as an ART script (equivalent to -art)                                            #
#     Other options for xgrid -job submit are not supported.  The following options are new for this script: #
#     -r  Use supplied file as an ART script, and also calculate the node ranking scores using the supplied  #
#         script rather than the inbuilt script or a script in "Application Support/mgrid/scoring.rb".  The  #
#         disadvantage of using an ART script to calculate node ranking in this way is that for multi-task   #
#         jobs, the score is not altered to take into account previous task allocations.  For an alternative #
#         method which preserves this see 'runjags/inst/xgrid/node_scoring_example.rb'.  For users outside   #
#         the University of Glasgow or not using node ranking, the -r option is identical to the -a option.  #
#         Supplying " " or "none" as the argument disables node ranking and the ranking ART script for       #
#         submission of this job.                                                                            #
#     -b  Produce a batch file with the specified file name and stop (does not submit to xgrid).  This       #
#         option also prints the (ranked) scores and nodes to screen.  If the batchname specified is         #
#         "/dev/null" then more detailed information about the scoring is shown and no batch file is         #
#         produced (and cmd may be omitted)                                                                  #
#     -c  Ensure that jobs are run only on intel or ppc machines - arch should be 'intel' or 'ppc'           #
#     -v  A space seperated string of environmental variable(s) in the form of "var_one=value_one            #
#         var_two=value_two" to be set locally before running the command                                    #
#     -q  Wait for nodes running OS X server to become available rather than running jobs on desktop nodes   #
#     -h  Don't run the ranking script, and use the provided node name(s) instead.  For multiple tasks,      #
#         separate node names with a colon (no space).  Entire string must be quoted if any node name        #
#         contains a space.  Colons in node names will produce either an error or unexpected results.  If    #
#         the node name does not match any of the available nodes then the first available node will be      #
#         chosen (unless the -f option is also specified in which case the job/task will hang). Supplying a  #
#         blank node name (-h "") produces a job or batch file with no schedule hinting (this also           #
#         effectively turns off node ranking for University of Glasgow users, although the ranking script is #
#         still passed as an ART script to the job - see the -r option for a way to disable both).           #
#     -f  Force the controller to assign the job to the highest ranked (or specified using -t) node by using #
#         an ART script rather than schedule hinting.  Note that ALL tasks will be run on the same node.     #
#     -n  The name to give the job (appears on xgrid admin etc).  If none is supplied, the command is used   #
#         as the name                                                                                        #
#     -t  The number of tasks being run.  The arguments should include one containing '$task' which denotes  #
#         the task number (this MUST be enclosed in single quotes or '\' used to escape '$'), otherwise the  #
#         task number will be passed as the last argument to the command.                                    #
#                                                                                                            #
#     -l  Display a list of jobs currently on xgrid and exit.  The following arguments can also be given:    #
#         -m  Include the current status of the jobs.                                                        #
#         -u  Include the username and hostame of whoever submitted the job (provided the jobs were          #
#             submitted using mgrid).                                                                        #
#         All other arguments are ignored.                                                                   #
#                                                                                                            #
#     -?  Print a help/usage message and exit.  All other arguments are ignored.                             #
#                                                                                                            #
#     ARGUMENTS                                                                                              #
#     'cmd' is the command to be run on xgrid, and the remaining arguments are passed as arguments to this   #
#     command.  The special argument '$task' is used to denote the task number, and is appended to (any)     #
#     other commands if ntasks is specified (even if it is 1) and '$task' is not found among the other       #
#     arguments.  The '$task' variable can be embedded in other text to form an argument that changes with   #
#     the task number, for example mgrid -t 2 /usr/bin/cal -y '200$task' would print 2001 for task 1 and     #
#     2002 for task 2.  *NB* If using the '$task' special variable in this way, ensure that the argument is  #
#     enclosed in single quotes (NOT double quotes), or use backslash to escape it (as in "\$"), as the      #
#     shell will otherwise evaluate "$task" to "" on passing the argument to mgrid.                          #
#                                                                                                            #
#     NOTES                                                                                                  #
#     This script is distributed 'as is', both FREELY and WITHOUT CHARGE, under the GNU general public       #
#     license (see http://www.gnu.org/copyleft/gpl.html).  I am therefore not liable for any damage to your  #
#     computer, xgrid cluster, or sanity caused by using it.                                                 #
#                                                                                                            #
#     If you find this script useful, or find any bugs, then feel free to email me at matthewdenwood@mac.com #
#     Paypal donations to the same address are also gratefully received and may encourage further            #
#     development of the software....                                                                        #
#                                                                                                            #
##############################################################################################################


# The node ranking stuff is currently only available for the xgrid controller based at the University of Glasgow; the following option turns it on or off in the script (supplying the option -h "" to mgrid effectively also turns off node ranking, although the node ranking script is still sent as an ART script with the job):
if [ "$XGRID_CONTROLLER_HOSTNAME" == "hydra.vet.gla.ac.uk" ]; then
	uog=1
else
	uog=0
fi

if [ $uog == 1 ]; then
	subid='mgrid_UoG_v2.01'
else
	subid='mgrid_v2.01'
fi


##### Some exit codes from /usr/include/sysexits.h
EX_OK=0
EX_USAGE=64
EX_UNAVAILABLE=69
EX_SOFTWARE=70
EX_CANTCREAT=73
#####


queue=0
tasks=0
thejobname=""
manhint=0
manhints=""
force=0
artrank=0
artscore=0
list=0
listcomment=0
listuser=0
printhelp=0
printerror=0
rankingdisabled=0

while getopts "s:i:d:e:a:r:c:v:b:qh:fn:t:lmu?" flag; do
if [ $flag == "s" ]; then
	stdin=$OPTARG
elif [ $flag == "i" ]; then
	indir=$OPTARG
elif [ $flag == "d" ]; then
	# Can only have 1 dependant job using this, but xgrid.start allows more in the details.txt file
	depjobs=$OPTARG
elif [ $flag == "e" ]; then
	email=$OPTARG
elif [ $flag == "a" ]; then
	artscore=1
	artpath=$OPTARG
elif [ $flag == "r" ]; then
	if [ "$OPTARG" == "" -o "$OPTARG" == " " -o "$OPTARG" == "none" -o "$OPTARG" == "None" -o "$OPTARG" == "NONE" ]; then
		uog=0
		rankingdisabled=1
	else
		artpath=$OPTARG
		artrank=1
	fi
elif [ $flag == "c" ]; then
	arch=$OPTARG
	if [ "$arch" == "i386" -o "$arch" == "intel" -o "$arch" == "Intel" -o "$arch" == "INTEL" ]; then
		arch='intel'
	elif [ "$arch" == 'ppc' -o "$arch" == 'PPC' -o "$arch" == 'Ppc' ]; then
		arch='ppc'
	else
		echo "Unsupported cpu type '"$arch"'.  Specify one of 'intel' or 'ppc'." >&2
		exit $EX_USAGE
	fi
elif [ $flag == "v" ]; then
	env_var=$OPTARG
elif [ $flag == "b" ]; then
	batch=$OPTARG
elif [ $flag == "q" ]; then
	queue=1
elif [ $flag == "h" ]; then
	manhint=1
	manhints=$OPTARG
elif [ $flag == "f" ]; then
	force=1
elif [ $flag == "n" ]; then
	thejobname=$OPTARG
elif [ $flag == "t" ]; then
	tasks=$OPTARG
elif [ $flag == "l" ]; then
	list=1
elif [ $flag == "m" ]; then
	listcomment=1
elif [ $flag == "u" ]; then
	listuser=1
elif [ $flag == "?" ]; then
	printerror=1
fi
done


shift $(( $OPTIND-1 ))

nenv_var=`echo $env_var | awk -v p=2 {'print NF'}`
if [ $nenv_var -gt 0 ];then
	for (( i=1; i<=$nenv_var; i++ )); do
		env_vars[$i]=`echo $env_var | awk -v j=$i {'print $j'}`
	done
fi

statusonly=0
if [ "$batch" == "/dev/null" -o "$batch" == "/dev/null/" ]; then
	if [ $uog == 1 ]; then
		statusonly=1
	fi
fi

#for (( i=1; i<=$nenv_var; i++ )); do
#	echo ${env_vars[$i]} | awk -F "=" {'print $1'}
#	echo ${env_vars[$i]} | awk -F "=" {'print $2'}
#done

if [ $printerror == 1 ]; then
	printf "\nUsage:\nmgrid [-s stdin] [-i indir] [-d jobid] [-e email-address] \n      [-a art-path | -r art-path] [-b batchname] [-c arch] \n      [-v \"env_variable=env_value [...]\" ] [-q] [-h node_name] \n      [-f] [-n name] [-t number_of_tasks] cmd [arg1 [...]]\nmgrid -l [-m] [-u]\nmgrid -?\n(Use 'mgrid' with no arguments to see the manual page)\n\n" >&2
	exit $EX_USAGE	
fi

if [ $# == 0 -a $statusonly == 0 -a $list == 0 ]; then
	printhelp=1
fi

if [ $printhelp == 1 ]; then
	# Horrible hack of a function to allow overstriking (bold) for less (tput doesn't work...):
	embold ()
	{
	str=`echo $1`
	out=""
	for (( i=0; i<${#str}; i++ )); do
	out="$out${str:$i:1}\b${str:$i:1}"
	done

	echo "$out"
	return 0
	}


	printf "
`embold "mgrid -- version 2.01, February 2010"`
by Matthew Denwood (matthewdenwood@mac.com)

`embold SYNOPSIS`
A bash script that provides a replacement for `embold 'xgrid -job submit'` using batch
file submission with support for multi-task jobs, scheduler hinting,
environmental variables, and built-in ART scripts for selection of intel or
ppc machines.

`embold USAGE`
`embold mgrid` [`embold -s` stdin] [`embold -i` indir] [`embold -d` jobid] [-\b-e\be email-address] 
   [`embold -a` art-path | `embold -r` art-path] [`embold -b` batchname] [`embold -c` arch]
   [`embold -v` \"env_variable=env_value [...]\" ] [`embold -q`] [`embold -h` node_name] [`embold -f`] 
   [-\b-n\bn name] [`embold -t` number_of_tasks] `embold cmd` [arg1 [...] ['\$task']]
`embold 'mgrid -l'` [`embold -m`] [`embold -u`]
`embold 'mgrid -?'`

`embold OPTIONS`
The following options are equivaent to those given for xgrid -job submit:
`embold -s`  Use supplied file as standard input (equivalent to `embold -si`)
`embold -i`  Input supplied directory (equivalent to `embold -in`)
`embold -d`  Wait for the dependant job ID to finish (equivalent to `embold -dids`, with the 
    limitation that only 1 dependant job can be specified)
-\b-e\be  Use supplied email address to report status changes (equivalent to `embold -email`)
`embold -a`  Use supplied file as an ART script (equivalent to `embold -art`)

Other options for xgrid -job submit are not supported.  The following options 
are unique for this script:
`embold -r`  Use supplied file as an ART script, and also calculate the node ranking 
    scores using the supplied script rather than the inbuilt script or a script 
    in 'Application Support/mgrid/scoring.rb'.  The disadvantage of using an 
    ART script to calculate node ranking in this way is that for multi-task 
    jobs, the score is not altered to take into account previous task 
    allocations.  For an alternative method which preserves this feature see:
    'runjags/inst/xgrid/node_scoring_example.rb'.  For users outside the
    University of Glasgow or not using node ranking, the `embold -r` option is identical 
    to the `embold -a` option.  Supplying \" \" or \"none\" as the argument 
    disables node ranking and the ranking ART script for submission of this job.
`embold -b`  Produce a batch file with the specified file name and stop (does not 
    submit to xgrid).  This option also prints the (ranked) scores and nodes to
    screen.  If the batchname specified is /dev/null then more detailed 
    information about the scoring is shown and no batch file is produced (and 
    `embold cmd` may be omitted)
`embold -c`  Ensure that jobs are run only on intel or ppc machines - should be 
    'intel' or 'ppc'
`embold -v`  A space seperated string of environmental variable(s) in the form of 
    \"var_one=value_one var_two=value_two\" to be set locally before running the
    command
`embold -q`  Wait for nodes running OS X server to become available rather than running 
    jobs on desktop nodes
`embold -h`  Don't run the ranking script, and use the provided node name(s) instead.  
    For multiple tasks, separate node names with a colon (no space).  Entire 
    string must be quoted if any node name contains a space.  Colons in node
    names will produce either an error or unexpected results.  If the node name 
    does not match any of the available nodes then the first available node 
    will be chosen (unless the -f option is also specified in which case the 
    job/task will hang).  Supplying a blank node name (-h ) produces a job or 
    batch file with no schedule hinting (this also effectively turns off node 
    ranking for University of Glasgow users, although the ranking script is 
    still passed as an ART script to the job - see the `embold -r` option for a way
    to disable both).
`embold -f`  Force the controller to assign the job to the highest ranked (or specified 
    using `embold -t`) node by using an ART script rather than schedule hinting.  
    Note that ALL tasks will be run on the same node
-\b-n\bn  The name to give the job (appears on xgrid admin etc).  If none is supplied,
    the command is used as the name
`embold -t`  The number of tasks being run.  The arguments should include one containing
    '\$task' which denotes the task number (this MUST be enclosed in single
    quotes or the \$ sign escaped), otherwise the task number will be passed as 
    the last argument to the command.                                    

`embold -l`  Display a list of jobs currently on xgrid and exit.  The following two
    arguments can also be given:
    `embold -m`  Include the current status of the jobs.
    `embold -u`  Include the username and hostame of whoever submitted the job
        (provided the jobs were submitted using mgrid).
    All other arguments are ignored. 

`embold -?`  Print a help/usage message and exit.  All other arguments are ignored.

`embold ARGUMENTS`
`embold cmd` is the command to be run on xgrid, and the remaining arguments are passed
as arguments to this command.  The special argument '\$task' is used to denote 
the task number, and is appended to (any) other commands if ntasks is specified 
(even if it is 1) and '\$task' is not found among the other arguments.  The 
'\$task' variable can be embedded in other text to form an argument that changes 
with the task number, for example `embold \"mgrid -t 2 /usr/bin/cal -y '200\\\$task'\"` would 
print 2001 for task 1 and 2002 for task 2.  `embold '*NB*'` If using the '\$task' special
variable in this way, ensure that the argument is enclosed in single quotes
(NOT double quotes), or use backslash to escape it (as in \"\\\$\"), as the 
shell will otherwise evaluate \"\$task\" to \"\" on passing the argument to mgrid.

`embold REQUIREMENTS`
Requires XGRID_CONTROLLER_HOSTNAME and XGRID_CONTROLLER_PASSWORD to be set as 
environmental variables (these cannot currently be specified as arguments).

`embold ABOUT`
This script is a replacement for `embold 'xgrid -job submit'` that provides some
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

`embold 'For use with hydra.vet.gla.ac.uk only'`:
By default, an inbuilt script is used to rank the available nodes for job/task
allocation.  This scoring is performed based on the number of processors per
node, the number of jobs already running, and whether or not the node is a 
server (running Mac OS X server).  The first task is assigned to the node with 
the highest score before incrementing the number of jobs running on that node 
to account for the extra task, and re-calculating the score for that node.  
This process is then repeated for all remaining tasks.  Additional information 
detailing the amount of RAM, processor speed and 32/64 bit availability on the 
nodes is collected but not used by default.  To change the way in which the 
tasks are distributed among the available nodes, modify the code as desired in 
the file included with the runjags package (found in:
'runjags/inst/xgrid/node_scoring_example.rb'), rename the file either
'scoring.rb' or 'scoring.sh', and save either in the working directory or in 
'/Library/Application Support/mgrid/'.  Alternatively, a simple scoring script
can be specified using the `embold -r` option, however this score will not be updated 
after assigning each task to account for the extra task, so all tasks will be 
assigned to the node with the highest score at the start.  If the number of 
tasks exceeds the number of available processors on that node, the remaining 
tasks will be allocated to nodes chosen by the xgrid controller.  For jobs with
only a single task this option should produce the same results as the more 
complex method described above.

`embold NOTES`
This script is distributed 'as is', both FREELY and WITHOUT CHARGE, under the
GNU general public license (see http://www.gnu.org/copyleft/gpl.html).  I am
therefore not liable for any damage to your computer, xgrid cluster, or sanity
caused by using it.  

If you find this script useful, or find any bugs, then feel free to email me
at matthewdenwood@mac.com.  Paypal donations to the same address are also
gratefully received and may encourage further development of the software....

" | less

	exit $EX_OK
fi


if [ $artscore == 1 -a $artrank == 1 ];then
	echo "Cannot specify both -a and -r options" >&2
	exit $EX_USAGE
fi


# I use quite a few (probably more than necessary) temporary files, so they should at least be secure:
tmpdir=`mktemp -d -t temp` && success=1 || success=0

if [ $success == 0 ]; then
	echo "Unable to create temporary working directory" >&2
	exit $EX_CANTCREAT
fi

# Assure the file is removed at program termination or after we received a signal:
trap 'rm -rf "$tmpdir" >/dev/null 2>&1' 0
trap "exit 2" 1 2 3 15

# Set up a temporary file I use:
tfile1=${tmpdir}/temp1

# Check that hostname and password are set as environmental variables:
if [ "$XGRID_CONTROLLER_HOSTNAME" == "" ]; then
	echo "Error:  The environmental variables XGRID_CONTROLLER_HOSTNAME (and, if required, XGRID_CONTROLLER_PASSWORD) are not set.  Use the export command to do this (probably in your .profile)." >&2
	exit $EX_USAGE
fi






##################   This section is for the -l options only

if [ $list == 1 ]; then
	printf "Retrieving current job list from xgrid..."
	xgrid -job list >& $tfile1 && success=1 || success=0
	if [ $success == 1 ]; then
	
		ORIGIFS=$IFS
		#save the current value of ifs

		IFS=$(echo -en "\n\b\r")
		#reset ifs to end of line stuff

		exec 3<&0
		#save current value of stdin

		exec 0<$tfile1
		#set stdin to read from the temporary file
		
		jobs=0
		string=""
		while read line
		do
			linenospace=`echo $line | awk '{print $1}' | sed -e "s/,//g"`
			if [ ! "$linenospace" == "{" -a ! "$linenospace" == "}" -a ! "$linenospace" == ");" -a ! "$linenospace" == "jobList" ]; then
				string=`echo $string $linenospace`
				jobs=$(( $jobs+1 ))
				pid[$jobs]=$linenospace
			fi
		done
		
		exec 0<&3 3<&-
		# restore stdin and free up fd#6 for other processes to use

		IFS=$ORIGIFS
		# restore $IFS which was used to determine what the field separators are

		rm $tfile1
		tempstatusfile=${tmpdir}/tempstatusfile
		
		if [ $jobs == 0 ]; then
			printf "\rThere are no jobs currently listed on xgrid\n"
		else
			
			if [ $listuser == 1 ]; then
				if [ $listcomment == 1 ]; then
					echo "Name;ID;Status;Comment;User" > $tempstatusfile
				else
					echo "Name;ID;Status;User" > $tempstatusfile
				fi
			else
				if [ $listcomment == 1 ]; then
					echo "Name;ID;Status;Comment" > $tempstatusfile
				else
					echo "Name;ID;Status" > $tempstatusfile
				fi
			fi
			
			for (( i=1; i<=$jobs; i++ )); do
				
				xgrid -job attributes -id ${pid[$i]} >& $tfile1 && success=1 || success=0
				if [ $success == 1 ]; then
					
					grepstring=`cat $tfile1 | grep "jobStatus"`
					status=`echo $grepstring | awk '{print $3}'`
					
					grepstring=`cat $tfile1 | grep "applicationIdentifier"`
					usern=`echo $grepstring | awk '{print $3}' | sed 's/;//g' | sed 's/"//g' | sed "s/'//g"`
					
					if [ "$usern" == "com.apple.xgrid.cli" ]; then
						usern="Unknown"
					fi
					if [ "$usern" == "igrid_MD" ]; then
						usern="Unknown"
					fi
					if [ "$usern" == "mgrid_MD" ]; then
						usern="Unknown"
					fi
					
					grepstring=`cat $tfile1 | grep "percentDone"`
					percent=`echo $grepstring | awk '{print $3}' | awk '{print int(($1*10)+0.5)/10}'`
					
					grepstring=`cat $tfile1 | grep "dateSubmitted"`
					subon=`echo $grepstring | awk '{print $3 " at " $4}'`

					grepstring=`cat $tfile1 | grep "dateStarted"`
					startedon=`echo $grepstring | awk '{print $3 " at " $4}'`

					grepstring=`cat $tfile1 | grep "dateStopped"`
					stoppedon=`echo $grepstring | awk '{print $3 " at " $4}'`
					
					grepstring=`cat $tfile1 | grep "name"`
					jobname=`echo $grepstring | awk '{print $3}' | sed -e "s/[;\"]//g"`
					
					string="$jobname;${pid[$i]}"
					
					if [ "$status" == "Finished;" ]; then
						if [ $listcomment == 1 ]; then
							string=`echo "$string;Complete;Finised on: $stoppedon"`
						else
							string=`echo "$string;Complete"`
						fi
					elif [ "$status" == "Pending;" ]; then
						grepstring=`cat $tfile1 | grep "suspended"`

						if [ "`echo $grepstring`" == "" ]; then
							if [ $listcomment == 1 ]; then
								string=`echo "$string;Pending;Submitted on: $subon"`
							else
								string=`echo "$string;Pending;"`
							fi
						else
							if [ $listcomment == 1 ]; then
								string=`echo "$string;PAUSED;Submitted on: $subon"`
							else
								string=`echo "$string;PAUSED"`
							fi
						fi
					elif [ "$status" == "Canceled;" ]; then
						if [ $listcomment == 1 ]; then
							string=`echo "$string;Canceled;Submitted on: $subon"`
						else
							string=`echo "$string;Canceled"`
						fi
					elif [ "$status" == "Running;" ]; then
						grepstring=`cat $tfile1 | grep "activeCPUPower"`
						cpupower=`echo $grepstring | awk '{print $3}' | sed -e "s/;//g"`
						
						grepstring=`cat $tfile1 | grep "suspended"`

						if [ "`echo $grepstring`" == "" ]; then
							if [ $listcomment == 1 ]; then
								string=`echo "$string;RUNNING;${percent:0:4}% complete, started on: $startedon"`
							else
								string=`echo "$string;RUNNING"`
							fi
						else
							if [ $listcomment == 1 ]; then
								string=`echo "$string;PAUSED;${percent:0:4}% complete, started on: $startedon"`
							else
								string=`echo "$string;PAUSED"`
							fi
						fi
						
						#`", active CPU power: $cpupower"`
					elif [ "$status" == "Failed;" ]; then
						grepstring=`cat $tfile1 | grep "error"`
						error=`echo $grepstring | awk ' {
						for (i=3; i<=NF; i++)
						printf("%s ", $i)
						} ' | sed -e "s/;//g"`
						if [ $listcomment == 1 ]; then
							string=`echo "$string;FAILED;Returned error $error"`
						else
							string=`echo "$string;FAILED"`
						fi
					else
						if [ $listcomment == 1 ]; then
							string=`echo "$string;UNKNOWN;Xgrid returned an unrecognised status type for this job"`
						else
							string=`echo "$string;UNKNOWN"`
						fi
					fi
					
					if [ $listuser == 1 ]; then
						string=`echo "$string;$usern"`
					fi
					echo $string >> $tempstatusfile
				else
					echo "There was an error retrieving job information for process id ${pid[$i]}" >> $tempstatusfile
				fi
				rm $tfile1
			done
			#echo ""
			printf "\rThe following $jobs jobs are currently listed on xgrid:\n"
			#echo $string # echo ${pid[@]} would do the same thing anyway
			
			cat < $tempstatusfile | column -t -s ";" 
			rm $tempstatusfile
			#echo ""
			#echo "Use xgrid.results along with the job name to retrive results for a completed job"
		fi
		
		exit $EX_OK
		
	else
		echo "An error occured while contacting xgrid.  The following was returned:" >&2
		cat < $tfile1 >&2
		rm $tfile1
		exit $EX_UNAVAILABLE
	fi
	
	exit $EX_OK
fi

##################   End list option section



cmd=$1
shift
nargs=$#

if [ $nargs -ge 1 ]; then
	for (( i=1; i<=$nargs; i++ )); do
		args[$i]=$1
		shift
	done
fi

if [ $tasks == 0 ]; then
	usingtasks=0
	tasks=1
else
	# If ntasks < 0 (ie -t specified) then look for '$task' in any arguemt; if it isn't found then append an argument which is just $task
	dolltaskf=0
	for (( i=1; i<= $nargs; i++ )); do
		task=1
		grepr=`echo ${args[$i]} | grep '$task'`
		if [ ! "$grepr" == "" ]; then
			dolltaskf=1
		fi
	done
	if [ $dolltaskf == 0 ]; then
		nargs=$[$nargs+1]
		args[$nargs]='$task'
	fi
	usingtasks=1
fi

for (( i=1; i<=$tasks; i++ )); do
	hints[$i]=""
done

if [ $manhint == 1 ]; then
	nmanhints=`echo $manhints | awk -v p=2 'BEGIN { FS = ":" } ; { print NF }'`
	if [ "$manhints" == "" ]; then
		nmanhints=1
	fi
	if [ $nmanhints == 1 ]; then
	
		for (( i=1; i<=$tasks; i++ )); do
			hints[$i]=`echo $manhints | awk 'BEGIN { FS = ":" } ; { print $1 }'`
		done
	
	else
		
		if [ ! $nmanhints == $tasks ]; then
			echo "The number of manual schedule hints provided does not match the number of tasks - ensure the node names are separated by a colon">&2
			exit $EX_USAGE
		fi
	
		if [ $force == 1 ]; then
			echo "Only 1 manual schedule hint can be used if the -f option is specified">&2
			exit $EX_USAGE
		fi
	
		for (( i=1; i<=$nmanhints; i++ )); do
			hints[$i]=`echo $manhints | awk -v j=$i 'BEGIN { FS = ":" } ; { print $j }'`
		done
	
	fi
fi

if [ $uog == 0 ]; then
	if [ $force == 1 -a $manhint == 0 ]; then
		cat "You must supply a node name using the -h argument when using the -f argument">&2
		exit $EX_USAGE
	fi
	if [ $force == 1 -a "$manhints" == "" ]; then
		cat "You must supply a non-blank node name to the -h argument when using the -f argument">&2
		exit $EX_USAGE
	fi
fi

if [ ! -f $stdin ]; then
	cat "The file specified for standard-in does not exist">&2
	exit $EX_USAGE
fi
if [ ! -d $indir ]; then
	cat "The directory specified as the input directory does not exist">&2
	exit $EX_USAGE
fi
if [ ! -f $artpath ]; then
	cat "The file specified as art-path does not exist">&2
	exit $EX_USAGE
fi

if [ "$thejobname" == "" ]; then
	thejobname=$cmd
fi

echo " "
if [ $rankingdisabled == 1 ]; then
	echo "Node ranking disabled"
fi

if [ $manhint == 1 -o $uog == 0 ]; then
	echo "Generating xgrid job"
	updatelist=0
else
	
	##########################################################################################################################
	# The following code is the (space separated) names for the desktop machines, and will (at intervals) have to be updated.
	# Alternatively, we could access a list from a common resource so we only have to update one list.....

	# These names are correct as of 23.11.09, but are now only here as a last resort backup.  They should never need to be used or updated.  Leave first element of names blank (so index starts at 1):
	names=( "" "Dan’s Mac Pro" "Richard Orton’s Mac Pro" "Darran’s Mac Pro" "boydorr.ibls.gla.ac.uk" "boydorr-gkb.ibls.gla.ac.uk" "r-reeve.zoology.gla.ac.uk" "l mac pro" "PowerMac_MD" )
	l=$[${#names[@]}-1]
	ndesktops=$l

	# The code to generate the server names; this probably won't need to be altered except possibly to increase the size of the
	# loop when more intel nodes get added:

	l=$[$l + 1]
	names[$l]="hydra"

	l=$[$l + 1]
	names[$l]="boydorr"

	for (( i=3; i <= 5; i++ )); do
	l=$[$l + 1]
	names[$l]="intel"$i
	done

	for (( i=1; i <= 12; i++ )); do
	l=$[$l + 1]
	names[$l]="ppc"$i
	done

	lnames=$[${#names[@]}-1]

	if [ $lnames != $l ];then
		echo "There was an error generating the list of desktop and server names" >&2
		exit $EX_SOFTWARE
	fi

	##########################################################################################################################
	
	echo "Generating xgrid job using node ranking"

##########################################################################################################################
#  The following code is taken straight out of xgrid.start, so the following is required to make sure it is interchangable:
#  For mgrid, tasks is now specified as an option
#  For xgrid.start, artpath is always 'art.score.sh' if the file exists:
# artpath='art.score.sh'
# if [ ! -f $artpath ]; then
# artpath=""
# fi
#  And statusonly is always 0:
# statusonly=0


# Define getnodelist as a function (I use it at the end as well; saves copy/pasteing the code):
getnodelist ()
{

echo "Updating the local node list..."
# first see if a valid file exists:
validfile=0

if [ -f "/Library/Application Support/mgrid/nodelist.txt" ]; then
	listok=`find "/Library/Application Support/mgrid/nodelist.txt"`
	listok=`echo $listok`
else
	listok=""
fi

if [ "$listok" == "/Library/Application Support/mgrid/nodelist.txt" ]; then
	
	firstline=`head -n 1 "/Library/Application Support/mgrid/nodelist.txt"`
	lastline=`tail -n 1 "/Library/Application Support/mgrid/nodelist.txt"`

	if [ "$firstline" == "desktops:" ]; then
		if [ "$lastline" == "endlist" ]; then
			validfile=1
		fi
	fi
fi

# Then try to retrieve the job from xgrid:

listfound=1

ntemp=${tmpdir}/ntemp
xgrid -job list >& $ntemp && success=1 || success=0
if [ $success == 1 ]; then
	
	listfound=0
	
	ORIGIFS=$IFS
	#save the current value of ifs
	IFS=$(echo -en "\n\b\r")
	#reset ifs to end of line stuff
	exec 3<&0
	#save current value of stdin
	exec 0<$ntemp
	#set stdin to read from the temporary file
	
	jobs=0
	string=""
	while read line
	do
		linenospace=`echo $line | awk '{print $1}' | sed -e "s/,//g"`
		if [ ! "$linenospace" == "{" -a ! "$linenospace" == "}" -a ! "$linenospace" == ");" -a ! "$linenospace" == "jobList" ]; then
			string=`echo $string $linenospace`
			jobs=$(( $jobs+1 ))
			pid[$jobs]=$linenospace
		fi
	done
	
	exec 0<&3 3<&-
	# restore stdin and free up fd#6 for other processes to use
	IFS=$ORIGIFS
	# restore $IFS which was used to determine what the field separators are

	if [ $jobs == 0 ]; then
		echo "An error occured while retrieving the updated node list; there are no jobs currently listed on xgrid.">&2
	else
		
		# Loop through backwards since (once node lists are automatically done daily) the node_list should be one of the more recent jobs:
		for (( i=$jobs; i>0; i-- )); do
			
			xgrid -job attributes -id ${pid[$i]} >& $ntemp && success=1 || success=0
			if [ $success == 1 ]; then

				grepstring=`cat $ntemp | grep "name"`
				jobname=`echo $grepstring | awk '{print $3}' | sed -e "s/[;\"]//g"`
				
				if [ "$jobname" == "node_list" ]; then
					listfound=1
					
					grepstring=`cat $ntemp | grep "jobStatus"`
					status=`echo $grepstring | awk '{print $3}'`
					
					if [ ! "$status" == "Finished;" ]; then
						status=`echo $status | sed s/;//g`
						echo "An error occured while retrieving the updated node list; the 'node_list' job status was returned as '"$status"'" >&2
					else
						xgrid -job results -id ${pid[$i]} >& $ntemp && success=1 || success=0
						if [ $success == 1 ]; then
							firstline=`head -n 1 $ntemp`
							lastline=`tail -n 1 $ntemp`
							
							if [ "$firstline" == "desktops:" ]; then
								if [ "$lastline" == "endlist" ]; then
									cp $ntemp "/Library/Application Support/mgrid/nodelist.txt"
									echo "Node list updated successfully"
								else
									echo "An error occured while retrieving the updated node list; the file returned is in an unexpected format" >&2
								fi
							else
								echo "An error occured while retrieving the updated node list; the file returned is in an unexpected format" >&2
							fi
						else
							echo "An error occured while contacting xgrid to return the node list.  The following was returned:" >&2
							cat < $ntemp >&2
						fi
					fi
					break
				fi
			else
				echo "An error occured while contacting xgrid to return the node list.  The following was returned:" >&2
				cat < $ntemp >&2
			fi
			
			sleep 0.1
		done
	fi
	
else
	echo "An error occured while contacting xgrid to return the node list.  The following was returned:" >&2
	cat < $ntemp >&2
fi

rm $ntemp

# If this fails then if validfile=1 print a warning an old list will be used, or if validfile=0 use $names from the start of the script and echo a warning that it's likely to be very out of date (+ some idiot has deleted the node_list job).

if [ $listfound == 0 ]; then
	echo "The 'node_list' job was not found on xgrid (it may have been deleted)" >&2
	if [ $validfile == 1 ]; then
		echo "The (older) local version of the node list will be used" >&2
	else
		echo "The node list embedded in this script (updated November 2009) will be used" >&2
	fi
fi

}

#  Look for an 'mgrid.nodes.txt' file in /Library/Application Support/mgrid:
redolist=1
updatelist=1

if [ ! -d "/Library/Application Support/mgrid/" ]; then
	echo "Error:  Attempt to use node ranking features with no local directory setup.  Please follow the installation instructions and create a folder at \"/Library/Application Support/mgrid/\"">&2
	exit $EX_USAGE
fi

# If statusonly then always get a new list:
if [ -f "/Library/Application Support/mgrid/nodelist.txt" -a $statusonly == 0 ]; then
	
	# Get what the date was 28 days ago:
	
	date '+%Y %m %d' | 
	{
	read year month day

	# LIMITED TO 28 DAYS (as more than 1 month back breaks it)
	day=`expr "$day" - 28`
	if [ $day -le 0 ]; then
	month=`expr "$month" - 1`
	case "$month" in
	0)
	month=12
	year=`expr "$year" - 1`
	;;
	esac

	newday=`cal $month $year | grep . | fmt -1 | tail -1`
	day=$[$newday + $day]
	fi

	months=( "" "january" "february" "march" "april" "may" "june" "july" "august" "september" "october" "november" "december" )

	echo "$day ${months[$month]} $year 00:00:00"
	} > ${tmpdir}/tdate
	
	prevdate=`cat ${tmpdir}/tdate`
	rm ${tmpdir}/tdate
		
	# Make sure the local node list isn't ridiculously old:
	listok=`find "/Library/Application Support/mgrid/nodelist.txt" -newermt "$prevdate"`
	listok=`echo $listok`
	
	if [ "$listok" == "/Library/Application Support/mgrid/nodelist.txt" ]; then
		firstline=`head -n 1 "/Library/Application Support/mgrid/nodelist.txt"`
		lastline=`tail -n 1 "/Library/Application Support/mgrid/nodelist.txt"`

		if [ "$firstline" == "desktops:" ]; then
			if [ "$lastline" == "endlist" ]; then
				echo "Recent node list found locally"
				redolist=0
				
				# Check to see if the node list has been updated since yesterday:
				thedate=`date '+%d %B %Y' | sed y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/`
				listok=`find "/Library/Application Support/mgrid/nodelist.txt" -newermt "$thedate 00:00:00"`
				if [ "$listok" == "/Library/Application Support/mgrid/nodelist.txt" ]; then
					updatelist=0
				fi
			else
				echo "Local node list may be corrupt"
				rm "/Library/Application Support/mgrid/nodelist.txt"
			fi
		else
			echo "Local node list may be corrupt"
			rm "/Library/Application Support/mgrid/nodelist.txt"
		fi
	else
		echo "Local node list is old"
	fi
fi

if [ $redolist == 1 ]; then
	getnodelist
	updatelist=0
fi

# Update the node names and lnames etc ief the nodelist file exists (otherwise the ones at the top of the script will be kept):
if [ -f "/Library/Application Support/mgrid/nodelist.txt" ]; then
	
	lnames=$[${#names[@]}-1]
	# First remove all the backup names:
	for (( i=1; i<=$lnames; i++ )); do
		names[$i]=""
	done
	
	ORIGIFS=$IFS
	#save the current value of ifs
	IFS=$(echo -en "\n\b\r")
	#reset ifs to end of line stuff
	exec 3<&0
	#save current value of stdin
	exec 0<"/Library/Application Support/mgrid/nodelist.txt"
	#set stdin to read from the profile file
	
	l=1
	while read line
	do
		# Names come out of ruby script quoted and with an underscore appended so remove them:
		thename=`echo $line`
		if [ "$thename" == "desktops:" ]; then
			if [ $l -gt 1 ]; then
				echo "An error occured while reading the local nodelist.txt file">&2
				exit $SOFTWARE
			fi
		elif [ "$thename" == "servers:" ]; then
			ndesktops=$[$l-1]
		elif [ "$thename" == "endlist" ]; then
			lnames=$[${#names[@]}-1]

			if [ $lnames != $l ];then
				echo "There was an error reading the local nodelist.txt file">&2
				exit $EX_SOFTWARE
			fi
		else
			names[$l]="$thename"
			#echo ${names[$l]}
			l=$[$l + 1]
		fi

	done

	exec 0<&3 3<&-
	# restore stdin and free up fd#6 for other processes to use
	IFS=$ORIGIFS
	# restore $IFS which was used to determine what the field separators are
	
	
fi


fi

#  Look for a (shell or) ruby scoring script in the local directory, then the xgrid directory.  It must be run as a function/script taking arguments xjobs, physical cores, logical cores, server, ram, cpu speed, bits (64 or 32), (ART) cpu type, (ART) server, (ART) custom and echoing the score.  If none is there then use an internal script (half of the previous one).  The profiling script returns all this information.  The scores are then worked out by bash before every task is assigned and the top score given a task until all tasks done.

if [ $artrank == 1 ]; then

	cat > ${tmpdir}/scoring.rb <<-EOA
	#!/usr/bin/ruby -w

	# arguments xjobs, physical cores, logical cores, server, ram, cpu speed, bits (64 or 32), (ART) cpu type, (ART) server, (ART) custom

	puts ARGV[9].to_i
	exit 0
	EOA

else
	
	if [ -f scoring.rb ]; then
		if [ $uog == 1 -a $manhint == 0 ]; then
			echo "Using local ruby file for grid scoring"
		fi
		cp scoring.rb ${tmpdir}/scoring.rb
	elif [ -f scoring.sh ]; then
		if [ $uog == 1 -a $manhint == 0 ]; then
			echo "Using local shell script for grid scoring"
		fi
		cp scoring.sh ${tmpdir}/scoring.rb
	elif [ -f "/Library/Application Support/mgrid/scoring.rb" -a $manhint == 0 -a ! "`whoami`" == "nobody" ]; then
		cp "/Library/Application Support/mgrid/scoring.rb" ${tmpdir}/scoring.rb
	elif [ -f "/Library/Application Support/mgrid/scoring.sh" -a $manhint == 0 -a ! "`whoami`" == "nobody" ]; then
		cp "/Library/Application Support/mgrid/scoring.sh" ${tmpdir}/scoring.rb
	else
	
		# The ` and $ all need to be escape charactered:
	
		cat > ${tmpdir}/scoring.rb <<-EOA
		#!/usr/bin/ruby -w

		# arguments xjobs, physical cores, logical cores, server, ram, cpu speed, bits (64 or 32), (ART) cpu type, (ART) server, (ART) custom

		# Current jobs:
		xjobs = ARGV[0].to_i
		# Physical cores:
		ncpu = ARGV[1].to_i
		# Logical cores:
		nlogical = ARGV[2].to_i
		# 1 or 0:
		server = ARGV[3].to_i
		# RAM in MB:
		ram = ARGV[4].to_i
		# Clock speed in MHz:
		cpuspeed = ARGV[5].to_i
		# bits (64 or 32):
		bits = ARGV[6].to_i
		# Result of CPU type script (1 or 0; or 1 if not used):
		artcpu = ARGV[7].to_i
		# Result of server/desktop script (1 or 0; or 1 if not used):
		artserver = ARGV[8].to_i
		# Result of the custom art score (or 1 if not used):
		artcustom = ARGV[9].to_i

		serverbonus = 4

		base = 0
		if (xjobs < nlogical)
		  # If there's any space left on the processor, check whether there
		  # are any physical cores left (ignoring one desktop core for the user!)
		  if (xjobs < ncpu + server - 1)
		    # If there are then rank by number of cores, giving a 4 bonus to
		    # servers, so that 4 physical cores will be used on each server
		    # before any desktops are touched
		    base = base + (ncpu - xjobs + serverbonus * server) * 1000
		  else
		    # If not, then rank by number of logical cores, again with bonus
		    # for servers to push jobs to them
		    base = base + (nlogical - xjobs + serverbonus * server) * 100
		  end
		  # Randomise to vary order of usage
		  base = base + Kernel.rand(50)
		end

		# Take into account the ART scores (server/desktop * intel/ppc * custom.art - these will just be 1 if not used):
		base = base * artcustom * artserver * artcpu
	
		puts base.to_i
	
		EOA
	
	fi
fi

# scoring.rb needs to be executable so I can use it locally:
chmod 755 ${tmpdir}/scoring.rb
scoring=${tmpdir}/scoring.rb

# The getinfo file is not meant to be user modified:

cat > ${tmpdir}/getinfo.rb <<-EOA
#!/usr/bin/ruby -w

# To get all sysctl info:
# sysctl -a

# This is the best approximation I can get to the number of jobs the
# computer is currently running
#xjobs = \`/bin/ls -1 /var/xgrid/agent/tasks | /usr/bin/wc -l\`.strip.to_i - 1
xjobs = \`/usr/bin/find /var/xgrid/agent/tasks -newerat '1 month ago' -and -not -newerBt '10 seconds ago' -maxdepth 1 -type d -name '??????*' | /usr/bin/wc -l\`.strip.to_i
xjobs = 0 if (xjobs < 0)

# Can extract number of physical cpu cores from sysctl
ncpu = \`/usr/sbin/sysctl hw.physicalcpu | /usr/bin/sed 's/hw.physicalcpu: //'\`.strip.to_i

# And the number of logical cores
nlogical = \`/usr/sbin/sysctl hw.ncpu | /usr/bin/sed 's/hw.ncpu: //'\`.strip.to_i
os=\`/usr/sbin/system_profiler SPSoftwareDataType | /usr/bin/grep 'System Version'\`.strip

# Is it a server?
server = 0
if (os =~ /Server/)
  server = 1
end

# RAM (in MB):
ram = \`/usr/sbin/sysctl hw.physmem | /usr/bin/sed 's/hw.physmem: //'\`.strip.to_i
ram = ram / (1024**2)

# CPU speed (in MHz):
cpuspeed = \`/usr/sbin/sysctl hw.cpufrequency | /usr/bin/sed 's/hw.cpufrequency: //'\`.strip.to_i
cpuspeed = cpuspeed / (1024**2)

# bits (64 or 32):
bits = \`/usr/sbin/sysctl hw.optional 2> /dev/null |/usr/bin/awk -F': ' '/64/ {print $2}'\`.strip.to_i
bits = (bits*32) + 32

# This swaps the spaces in names for _ and leaves an _ at the end of the name, but I deal with this in the shell script:
comname=\`/usr/sbin/system_profiler SPSoftwareDataType | /usr/bin/grep 'Computer Name' | /usr/bin/awk '{
for (i=3; i<=NF; i++)
printf("%s ", \$i)
}' | /usr/bin/sed -e 's/ /_/g' -e "s/'//g" -e "s/’//g" -e 's/"//g'\`.strip

artcustom=1
artserver=1
artcpu=1
# If the serveronly.sh and/or cpu type and/or art.score.sh files exist then execute them and get the result:
if File.exist?('customart.sh')
  artcustom = \`./customart.sh\`.strip.to_i
end
if File.exist?('server.rb')
  artserver = \`./server.rb\`.strip.to_i
end
if File.exist?('arch.sh')
  artcpu = \`./arch.sh\`.strip.to_i
end

EOA

# Copy getinfo to prepare it for profiling job and ART scoring job:
cp ${tmpdir}/getinfo.rb ${tmpdir}/infoscoring.rb
# Append stuff to profiling job:
echo 'puts "\"#{comname}\" #{xjobs} #{ncpu} #{nlogical} #{server} #{ram} #{cpuspeed} #{bits} #{artcpu} #{artserver} #{artcustom}"' >> ${tmpdir}/getinfo.rb
# Hexdump in (possibly local) scoring script and put in code to create the file locally and then run it:
printf '`echo "' >> ${tmpdir}/infoscoring.rb
xxd $scoring >> ${tmpdir}/infoscoring.rb
echo '" | xxd -r - scoring.rb`' >> ${tmpdir}/infoscoring.rb
echo '`chmod 755 scoring.rb`' >> ${tmpdir}/infoscoring.rb
echo 'puts `./scoring.rb #{xjobs} #{ncpu} #{nlogical} #{server} #{ram} #{cpuspeed} #{bits} #{artcpu} #{artserver} #{artcustom}`.strip.to_i' >> ${tmpdir}/infoscoring.rb
echo '`rm scoring.rb`' >> ${tmpdir}/infoscoring.rb

batchname=${tmpdir}/batch

cat > $batchname <<-EOA
{
    jobSpecification =     {
EOA
echo "submissionIdentifier = \"$subid\";" >> $batchname
echo "applicationIdentifier = \"`whoami`"@"$HOSTNAME\";" >> $batchname

cat >> $batchname <<-EOA
        inputFiles =         {
            "getinfo.rb" =             {
                fileData = <
EOA

hexdump  -v -e ' "" 4/1 "%02x" " "' ${tmpdir}/getinfo.rb >> $batchname

cat >> $batchname <<-EOA
>;
                isExecutable = YES;
            };
			"scoring.rb" =             {
                fileData = <
EOA
# Shouldn't need the scoring file but may as well send it anyway:
hexdump  -v -e ' "" 4/1 "%02x" " "' ${tmpdir}/scoring.rb >> $batchname

cat >> $batchname <<-EOA
>;
                isExecutable = YES;
            };
EOA


if [ ! "$artpath" == "" ]; then
	cat >> $batchname <<-EOA
	"customart.sh" = {
	fileData = <
	EOA
	hexdump  -v -e ' "" 4/1 "%02x" " "' $artpath >> $batchname
	cat >> $batchname <<-EOA
	>;
	                isExecutable = YES;
	            };
	EOA
fi

if [ $queue == 1 ]; then
	
	cat > ${tmpdir}/server.rb <<-EOA
	#!/usr/bin/ruby -w

	os=\`/usr/sbin/system_profiler SPSoftwareDataType | /usr/bin/grep 'System Version'\`.strip

	# Is it a server?
	server = 0
	if (os =~ /Server/)
	  server = 1
	end

	puts server

	EOA
	cat >> $batchname <<-EOA
	"server.rb" = {
	fileData = <
	EOA
	hexdump  -v -e ' "" 4/1 "%02x" " "' ${tmpdir}/server.rb >> $batchname
	cat >> $batchname <<-EOA
	>;
	                isExecutable = YES;
	            };
	EOA
fi

if [ "$arch" != "" ]; then
	
	cat > ${tmpdir}/arch.sh <<-EOA
	#!/bin/bash

	if [ \`arch\` == "i386" ]; then
		intel=1
		ppc=0
	else
		intel=0
		ppc=1
	fi

	echo \$$arch

	exit 0

	EOA
	cat >> $batchname <<-EOA
	"arch.sh" = {
	fileData = <
	EOA
	hexdump  -v -e ' "" 4/1 "%02x" " "' ${tmpdir}/arch.sh >> $batchname
	cat >> $batchname <<-EOA
	>;
	                isExecutable = YES;
	            };
	EOA
fi

cat >> $batchname <<-EOA
        };
        name = "score.profiling";
        
		EOA

	
echo "schedulerHints = {" >> $batchname
lnames=$[${#names[@]}-1]
for (( i = 1; i <= $lnames; i++ ))
do
	echo $i"=\""${names[$i]}"\";" >> $batchname
done
echo "};" >> $batchname

echo "taskSpecifications = {" >> $batchname
for (( i = 1; i <= $lnames; i++ ))
do
	echo "$i = {" >> $batchname
	#if [ $i != 1 ];then
	#	echo "dependsOnTasks="$[$i-1]";" >> $batchname
	#fi
	echo "command = \"./getinfo.rb\";" >> $batchname
	echo "};" >> $batchname
done

cat >> $batchname <<-EOF
         };
    };
}
EOF

if [ $manhint == 0 -a $uog == 1 ]; then

echo "Submitting profiling job to xgrid..."

xgrid -job batch $batchname >& $tfile1 && success=1 || success=0
if [ $success == 1 ]; then
	echo "Job submission successful"
	
	jobstring=`tail +2 $tfile1 | head -n 1`
	cutstr=${jobstring:20}
	len=$(( ${#cutstr}-1 ))
	jobnum=${cutstr:0:len}
	
	rm $tfile1
else
	echo "An error occured while submitting the profiling job to xgrid.  The following was returned:" >&2
	cat < $tfile1 >&2
	exit 1
fi

rm $batchname

echo "Waiting for the job to complete..."
while true; do
	sleep 20

	xgrid -job attributes -id $jobnum >& $tfile1 && success=1 || success=0

	if [ $success == 1 ]; then
		grepstring=`cat $tfile1 | grep "jobStatus"`
		status=`echo $grepstring | awk '{print $3}'`
		
		rm $tfile1
	
		if [ "$status" == "Finished;" ]; then
			echo "Profiling job complete, reading results..."
			break
		else
			echo "Profiling job is not yet complete.  Waiting to try again..."
		fi
	else
		echo "An error was returned from xgrid when attempting to retrieve the job status.  Waiting to try again..."
	fi
done


profile=${tmpdir}/profile
	
xgrid -job results -id $jobnum -so $profile >& $tfile1 && success=1 || success=0
if [ $success == 1 ]; then
	rm $tfile1
	echo "Profiling job retrieved successfully"
else
	echo "An error occured while retrieving the profiling job from xgrid.  The following was returned:" >&2
	cat < $tfile1 >&2
	exit 1
fi

xgrid -job delete -id $jobnum >& $tfile1 && success=1 || success=0
if [ $success == 0 ]; then
	echo "An error occured while attempting to delete the profiling job.  The following was returned:" >&2
	cat < $tfile1 >&2
	echo "You will have to remove the job (ID $jobnum) manually" >&2
fi

#######  Having got the profiles, we now need to sort the jobs:

for (( i=1; i <= $lnames; i++ )); do
	namesfound[$i]=0
	snames[$i]=`echo ${names[$i]} | sed -e "s/'//g" -e "s/’//g" -e 's/"//g' -e 's/ /_/g'`
done

#snames are string names, names are what is sent to hints


ORIGIFS=$IFS
#save the current value of ifs

IFS=$(echo -en "\n\b\r")
#reset ifs to end of line stuff

exec 3<&0
#save current value of stdin

exec 0<$profile
#set stdin to read from the profile file

temp=${tmpdir}/temp
echo '' > $temp

while read line
do
	# Names come out of ruby script quoted and with an underscore appended so remove them:
	line=`echo $line | sed -e 's/_"//g' -e 's/"//g'`
	thename=`echo $line | awk '{print $1}'`
	desktop=0
	server=0
	
	for (( i=1; i<=$ndesktops; i++ )); do
		if [ $thename == ${snames[$i]} ]; then
			desktop=1
			namesfound[$i]=1
		fi
	done
	for (( i=$[$ndesktops+1]; i<=$lnames; i++ )); do
		if [ $thename == ${snames[$i]} ]; then
			server=1
			namesfound[$i]=1
		fi
	done
	
	echo $line | awk '{print $2" "$3" "$4" "$5" "$6" "$7" "$8" "$9" "$10" "$11" "$1}' >> $temp
		
done

exec 0<&3 3<&-
# restore stdin and free up fd#6 for other processes to use
 
IFS=$ORIGIFS
# restore $IFS which was used to determine what the field separators are

for (( i=1; i<=$ndesktops; i++ )); do
	if [ ${namesfound[$i]} == 0 ]; then
		echo "-1 0 0 0 0 0 0 0 0 0 "${snames[$i]} >> $temp
	fi
done
for (( i=$[$ndesktops+1]; i<=$lnames; i++ )); do
	if [ ${namesfound[$i]} == 0 ]; then
		echo "-1 0 0 0 0 0 0 0 0 0 "${snames[$i]} >> $temp
	fi
done

# First loop through to get the true number of free cores for each machine and all other information:

ORIGIFS=$IFS
#save the current value of ifs

IFS=$(echo -en "\n\b\r")
#reset ifs to end of line stuff

exec 3<&0
#save current value of stdin

exec 0<$temp
#set stdin to read from the temporary file

desktop=0

while read line
do
	if [ "$line" != "" ]; then
		thename=`echo $line | awk '{print $11}'`
		for (( i=1; i<=$lnames; i++ )); do
			if [ "$thename" == "${snames[$i]}" ]; then
				current=$i
			fi
		done
		if [ "${xjobs[$current]}" == "" ]; then
			xjobs[$current]=`echo $line | awk '{print $1}'`
			ncpu[$current]=`echo $line | awk '{print $2}'`
			nlogical[$current]=`echo $line | awk '{print $3}'`
			server[$current]=`echo $line | awk '{print $4}'`
			ram[$current]=`echo $line | awk '{print $5}'`
			cpuspeed[$current]=`echo $line | awk '{print $6}'`
			bits[$current]=`echo $line | awk '{print $7}'`
			artcpu[$current]=`echo $line | awk '{print $8}'`
			artserver[$current]=`echo $line | awk '{print $9}'`
			artcustom[$current]=`echo $line | awk '{print $10}'`
			origxjobs[$current]=${xjobs[$current]}
		else
			if [ `echo $line | awk '{print $1}'` -lt ${xjobs[$current]} ]; then
				xjobs[$current]=`echo $line | awk '{print $1}'`
				ncpu[$current]=`echo $line | awk '{print $2}'`
				nlogical[$current]=`echo $line | awk '{print $3}'`
				server[$current]=`echo $line | awk '{print $4}'`
				ram[$current]=`echo $line | awk '{print $5}'`
				cpuspeed[$current]=`echo $line | awk '{print $6}'`
				bits[$current]=`echo $line | awk '{print $7}'`
				artcpu[$current]=`echo $line | awk '{print $8}'`
				artserver[$current]=`echo $line | awk '{print $9}'`
				artcustom[$current]=`echo $line | awk '{print $10}'`
				origxjobs[$current]=${xjobs[$current]}
			fi
		fi
	fi
done

exec 0<&3 3<&-
# restore stdin and free up fd#6 for other processes to use
 
IFS=$ORIGIFS
# restore $IFS which was used to determine what the field separators are

#cp $temp nodelist.txt
rm $temp

tasksdone=0

temps=${tmpdir}/temps
temps1=${tmpdir}/temps1


echo "" > $temps

# First loop through all the scores:

for (( i=1; i<=$lnames; i++ )); do
	index[$i]=$i
	# Check to see if anything was returned (no response now means score 0 - I may change this but probably not):
	if [ ${xjobs[$i]} == -1 ]; then
		score[$i]=0
	else
		score[$i]=`$scoring ${xjobs[$i]} ${ncpu[$i]} ${nlogical[$i]} ${server[$i]} ${ram[$i]} ${cpuspeed[$i]} ${bits[$i]} ${artcpu[$i]} ${artserver[$i]} ${artcustom[$i]}`
	fi
	echo ${score[$i]}" "$i >> $temps
	firstscore[$i]=${score[$i]}
done


# Calculate the original order for feedback at the end:
cat $temps | sort -nr -o $temps1
mv $temps1 $temps

ORIGIFS=$IFS
#save the current value of ifs
IFS=$(echo -en "\n\b\r")
#reset ifs to end of line stuff
exec 3<&0
#save current value of stdin
exec 0<$temps
#set stdin to read from the temporary file

rank=1

while read line
do
	order[$rank]=`echo $line | awk '{print $2}'`
	rank=$[$rank+1]
done

exec 0<&3 3<&-
# restore stdin and free up fd#6 for other processes to use
IFS=$ORIGIFS
# restore $IFS which was used to determine what the field separators are


totalcores=0
totalmachines=0

for (( i=1; i<=$lnames; i++ )); do
	
	if [ ${firstscore[$i]} -gt 0 ]; then
		totalmachines=$[$totalmachines+1]
		availcores[$i]=$[${nlogical[$i]}-${origxjobs[$i]}]
		totalcores=$[$totalcores+${availcores[$i]}]
	fi
done


# Then select and update the changed score until tasks>tasksdone:

while true
do

	# Then get the highest score and index:
	first=`head -n 1 $temps`

	# Check the score > 0:
	firstscore=`echo $first | awk '{print $1}'`
	firstindex=`echo $first | awk '{print $2}'`
	if [ $firstscore -gt 0 ]; then
		tasksdone=$[$tasksdone+1]
		hints[$tasksdone]=${names[${firstindex}]}
		xjobs[$firstindex]=$[${xjobs[${firstindex}]}+1]
	else
		break
	fi

	if [ $tasksdone -ge $tasks -a $statusonly == 0 ]; then
		break
	fi
	
	# Then re-calculate the new score for the assigned node:
	score[$firstindex]=`$scoring ${xjobs[$firstindex]} ${ncpu[$firstindex]} ${nlogical[$firstindex]} ${server[$firstindex]} ${ram[$firstindex]} ${cpuspeed[$firstindex]} ${bits[$firstindex]} ${artcpu[$firstindex]} ${artserver[$firstindex]} ${artcustom[$firstindex]}`
	
	# To look at the updated scores:
	#echo ${names[$firstindex]}
	#echo ${score[$firstindex]}
	
	# And get the list back:
	echo "" > $temps

	for (( i=1; i<=$lnames; i++ )); do
		echo ${score[$i]}" "$i >> $temps
	done
	
	cat $temps | sort -nr -o $temps1
	mv $temps1 $temps
	
	if [ $tasksdone -gt $totalcores ]; then
		echo "Warning:  The ranking script supplied does not appear to be able to deal with multi-task jobs.  The schedule hints list has been truncated">&2
		break
	fi
	
done
	

if [ $statusonly == 0 ]; then
	echo ""
	echo "There are "$totalcores" cores on "$totalmachines" machines available for your job"
	if [ $tasks == 1 -o $force == 1 ]; then
		echo "Job assigned to "${hints[1]}
	else
		echo "The "$tasks" tasks have been allocated to the following nodes:"
		for (( i=1; i<$tasksdone; i++ )); do
			printf "${hints[$i]}, "
		done
		printf "${hints[$tasksdone]}."
		echo ""
	fi
	if [ $tasks -gt $tasksdone -a $force == 0 ]; then
		echo "There were more tasks than cores available; some tasks were not given scheduler hints as it would have been ignored by xgrid"
	fi
	echo ""
fi

else

	tasksdone=$tasks
	
fi


# End of code copy/pasted from xgrid.start
##########################################################################################################################
# Remember to have this at the end of xgrid.start:
# Update the local node list unless it was done at the start or within the last day:
#if [ $updatelist == 1 ]; then
#	getnodelist
#fi


# Produce the ranked scores only if $statusonly
if [ $statusonly == 1 ]; then
	
	prettyprofile=${tmpdir}/pprof

	echo "Name|Score|Free cores" > $prettyprofile
	for (( i=1; i<=$lnames; i++ )); do
		current=${order[$i]}
		if [ ${namesfound[$current]} == 1 ]; then
			echo ${names[$current]}"|"${firstscore[$current]}"|"${availcores[$current]} >> $prettyprofile
		elif [ ${namesfound[$current]} == 0 ]; then
			echo ${names[$current]}"|No response|0" >> $prettyprofile
		fi
	done
	
	echo "There are "$totalcores" cores on "$totalmachines" machines available with a score of greater than 0"
	echo "The following information was obtained:"
	echo ""
	cat $prettyprofile | column -t -s "|"
	echo ""
	echo "Tasks would be assigned in the following order:"
	echo ""
	for (( i=1; i<$tasksdone; i++ )); do
		printf "${hints[$i]}, "
	done
	printf ${hints[$tasksdone]}"."
	echo ""
	echo ""
fi


# Produce batch file for actual job:

echo "{" > $batchname 
echo "jobSpecification = {" >> $batchname
echo "submissionIdentifier = \"$subid\";" >> $batchname
echo "applicationIdentifier = \"`whoami`"@"$HOSTNAME\";" >> $batchname

echo "artConditions = {" >> $batchname
if [ $uog == 1 ]; then
	echo "\"basescore\" = {" >> $batchname
	echo "artMin = 1;" >> $batchname
	echo "};" >> $batchname
fi
if [ ! "$artpath" == "" ]; then
	echo "\"customscore\" = {" >> $batchname
	echo "artMin = 1;" >> $batchname
	echo "};" >> $batchname
fi
if [ $queue == 1 ]; then
	echo "\"serverscore\" = {" >> $batchname
	echo "artMin = 1;" >> $batchname
	echo "};" >> $batchname
fi
if [ "$arch" != "" ]; then
	echo "\"archscore\" = {" >> $batchname
	echo "artMin = 1;" >> $batchname
	echo "};" >> $batchname
fi
if [ $force == 1 ]; then
	echo "\"forcescore\" = {" >> $batchname
	echo "artMin = 1;" >> $batchname
	echo "};" >> $batchname
fi
			
echo "};" >> $batchname
echo "artSpecifications = {" >> $batchname
if [ $uog == 1 -a $artrank == 0 ]; then
	echo "\"basescore\" = {" >> $batchname
	echo "artData = <" >> $batchname
	hexdump  -v -e ' "" 4/1 "%02x" " "'  ${tmpdir}/infoscoring.rb >> $batchname
	echo ">;" >> $batchname
	echo "isExecutable = YES;" >> $batchname
	echo "};" >> $batchname
fi
if [ ! "$artpath" == "" ]; then
	echo "\"customscore\" = {" >> $batchname
	echo "artData = <" >> $batchname
	hexdump  -v -e ' "" 4/1 "%02x" " "'  $artpath >> $batchname
	echo ">;" >> $batchname
	echo "isExecutable = YES;" >> $batchname
	echo "};" >> $batchname
fi
if [ $queue == 1 ]; then
	echo "\"serverscore\" = {" >> $batchname
	echo "artData = <" >> $batchname
	hexdump  -v -e ' "" 4/1 "%02x" " "'  ${tmpdir}/server.rb >> $batchname
	echo ">;" >> $batchname
	echo "isExecutable = YES;" >> $batchname
	echo "};" >> $batchname
fi
if [ "$arch" != "" ]; then
	echo "\"archscore\" = {" >> $batchname
	echo "artData = <" >> $batchname
	hexdump  -v -e ' "" 4/1 "%02x" " "'  ${tmpdir}/arch.sh >> $batchname
	echo ">;" >> $batchname
	echo "isExecutable = YES;" >> $batchname
	echo "};" >> $batchname
fi

if [ $force == 1 ]; then
	echo "#!/bin/bash" > ${tmpdir}/force.sh
	echo "comname=\`/usr/sbin/system_profiler SPSoftwareDataType | /usr/bin/grep 'Computer Name' | /usr/bin/awk '{ for (i=3; i<=NF; i++) printf(\"%s \", \$i) }'\`" >> ${tmpdir}/force.sh
	echo 'if [ "$comname" == "'${hints[1]}'" -o "$comname" == "'${hints[1]}' " ]; then' >> ${tmpdir}/force.sh
	echo 'echo 1' >> ${tmpdir}/force.sh
	echo 'else' >> ${tmpdir}/force.sh
	echo 'echo 0' >> ${tmpdir}/force.sh
	echo 'fi' >> ${tmpdir}/force.sh
	echo "exit 0" >> ${tmpdir}/force.sh
	
	cp "${tmpdir}/force.sh" "force.sh"
	
	echo "\"forcescore\" = {" >> $batchname
	echo "artData = <" >> $batchname
	hexdump  -v -e ' "" 4/1 "%02x" " "'  ${tmpdir}/force.sh >> $batchname
	echo ">;" >> $batchname
	echo "isExecutable = YES;" >> $batchname
	echo "};" >> $batchname
fi
echo "};" >> $batchname

echo "name = \"$thejobname\";" >> $batchname
if [ "$email" != "" ];then
	echo "notificationEmail=\"$email\";" >> $batchname 
fi

if [ "$depjobs" != "" ]; then
	echo "\"schedulerParameters\" = {" >> $batchname
	# tasksMustStartSimultaneously and minimumTaskCount also belong in here
	echo "\"dependsOnJobs\" = (" >> $batchname
	echo "$depjobs" >> $batchname
	echo ");" >> $batchname
	echo "};" >> $batchname
fi
	

if [ "$indir" != "" ]; then
	cwd=`pwd`
	cd $indir
	echo "inputFiles = {" >> $batchname

	temp2=${tmpdir}/temp2

	find . ! -name '.*' > $temp2

	# could also use find `ls`  ! -name '.*' > tempfile.$$ here to create a list of files/dirs without the ./ prefix
	# then I would need to change every instance of ${line:2} to just $line in the loop below

	ORIGIFS=$IFS
	#save the current value of ifs

	IFS=$(echo -en "\n\b\r")
	#reset ifs to end of line stuff

	exec 3<&0
	#save current value of stdin

	exec 0< $temp2
	#set stdin to read from the temporary file

	while read line
	do
		if [ -d ${line:2} ]; then
			if [ ! "$(ls ${line:2})" ]; then
				##  EMPTY EXCEPT FOR INVISIBLE FILES
				echo "\"${line:2}/holdingfile.$$.hex\" = {" >> $batchname
				echo "fileData = <686f6c64 0a>;" >> $batchname
				echo "isExecutable = NO;" >> $batchname
				echo "};" >> $batchname
			fi
		else
			if [ ${line:2} != $batchname ]; then
				echo "\"${line:2}\" = {" >> $batchname
				# ignore the './' at the start of the filename
				echo "fileData = <" >> $batchname
				hexdump  -v -e ' "" 4/1 "%02x" " "'  $line >> $batchname
				echo ">;" >> $batchname
				if [ -x ${line:2} ]; then
					echo "isExecutable = YES;" >> $batchname
				else
					echo "isExecutable = NO;" >> $batchname
				fi
				echo "};" >> $batchname
			fi
		fi
	done

	exec 0<&3 3<&-
	# restore stdin and free up fd#6 for other processes to use
 
	IFS=$ORIGIFS
	# restore $IFS which was used to determine what the field separators are

	rm $temp2
	
	if [ "$stdin" != "" ]; then
		if [ ! -f $stdin ]; then
			echo "\"$stdin\" = {" >> $batchname
			echo "fileData = <" >> $batchname
			hexdump  -v -e ' "" 4/1 "%02x" " "'  $stdin >> $batchname
			echo ">;" >> $batchname
			if [ -x $stdin ]; then
				echo "isExecutable = YES;" >> $batchname
			else
				echo "isExecutable = NO;" >> $batchname
			fi
			echo "};" >> $batchname
		fi
	fi
	echo "};" >> $batchname
	cd $cwd
else
# This commandisfile stuff doesn't work as /bin/bash is a file etc....
#	if [ -f ./$cmd ]; then 
#		cmdisfile=1
#	else
		cmdisfile=0
#	fi
	if [ "$stdin" != "" -o $cmdisfile == 1 ]; then
		echo "inputFiles = {" >> $batchname
		if [ "$stdin" != "" ]; then
			echo "\"$stdin\" = {" >> $batchname
			echo "fileData = <" >> $batchname
			hexdump  -v -e ' "" 4/1 "%02x" " "'  $stdin >> $batchname
			echo ">;" >> $batchname
			if [ -x $stdin ]; then
				echo "isExecutable = YES;" >> $batchname
			else
				echo "isExecutable = NO;" >> $batchname
			fi
			echo "};" >> $batchname
		fi
#		if [ $cmdisfile == 1 ]; then
#			echo "\"$cmd\" = {" >> $batchname
#			echo "fileData = <" >> $batchname
#			hexdump  -v -e ' "" 4/1 "%02x" " "'  $cmd >> $batchname
#			echo ">;" >> $batchname
#			echo "isExecutable = YES;" >> $batchname
#			echo "};" >> $batchname
#		fi
		echo "};" >> $batchname	
	fi
fi

if [ $force == 0 ]; then
	echo "schedulerHints = {" >> $batchname

	for (( i = 1; i <= $tasksdone; i++ ))
	do
		if [ ! "${hints[$i]}" == "" ]; then
			echo $i"=\""${hints[$i]}"\";" >> $batchname
		fi
	done
	echo "};" >> $batchname
fi

echo "taskSpecifications = {" >> $batchname

for (( i = 1; i <= $tasks; i++ ))
do
	echo "$i = {" >> $batchname
	#if [ $i != 1 ];then
	#	echo "dependsOnTasks="$[$i-1]";" >> $batchname
	#fi
	echo "command = \"$cmd\";" >> $batchname
	
	if [ $nenv_var -gt 0 ]; then
		echo "environment = {" >> $batchname
		for (( j=1; j<=$nenv_var; j++ )); do
			var=`echo ${env_vars[$j]} | awk -F "=" {'print $1'}`
			val=`echo ${env_vars[$j]} | awk -F "=" {'print $2'}`
			echo $var" = "$val";" >> $batchname
		done
		echo "};" >> $batchname
	fi
		
	echo "arguments = (" >> $batchname
	if [ $nargs -ge 1 ]; then
		for (( j=1; j<=$nargs; j++ )); do
			grepf=`echo ${args[$j]} | grep '$task'`
			if [ "$grepf" == "" ]; then
				echo -n "\"${args[$j]}\"" >> $batchname
			else
				task=$i
				echo -n "\""`eval echo ${args[$j]}`"\"" >> $batchname
			fi
			if [ $j -lt $nargs ]; then
				echo "," >> $batchname
			fi
		done
	fi
	echo ");" >> $batchname
	if [ "$stdin" != "" ]; then
		echo "inputStream = \"$stdin\";" >> $batchname
	fi
	
	echo "};" >> $batchname
done

echo "};" >> $batchname
echo "};" >> $batchname
echo "}" >> $batchname

if [ "`plutil -s $batchname`" != "" ]; then
	echo "An error caused the job submission to be aborted, sorry.  plutil is complaining about the batch file I created - please send the 'batch-failed.txt' file to me at matthewdenwood@mac.com" >&2
	cp $batchname 'batch-failed.txt'
	exit $EX_SOFTWARE
fi
	
if [ "$batch" != "" ]; then
	if [ $statusonly == 0 ]; then
		echo "Creating batch file '$batch' for your job in the local directory..."
		mv $batchname $batch
	fi
else
	echo "Submitting your job to xgrid (this may take some time)..."
	
	xgrid -job batch $batchname >& $tfile1 && success=1 || success=0
	if [ $success == 1 ]; then
		
		jobstring=`tail +2 $tfile1 | head -n 1`
		cutstr=${jobstring:20}
		len=$(( ${#cutstr}-1 ))
		jobnum=${cutstr:0:len}
		
		# This is just for runjags support:
		if [ -f starter.sh -o -f scriptlauncher.sh ]; then
			echo $jobnum > jobid.txt
			echo "Your job ID is "$jobnum
		else
			echo "Job submission successful, your job ID is "$jobnum
		fi
		
		rm $tfile1
		
	else
		echo "An error occured while submitting the job to xgrid.  The following was returned:" >&2
		cat < $tfile1 >&2
		rm $tfile1
		exit $EX_UNAVAILABLE
	fi
	
fi

if [ $statusonly == 0 ]; then
	
	# Update the local node list unless it was done at the start or within the last day:
	if [ $updatelist == 1 ]; then
		getnodelist
	fi
	
	echo "Finished"
	echo ""
fi

exit $EX_OK
