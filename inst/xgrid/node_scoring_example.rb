#!/usr/bin/ruby -w

##############################################################################################################
#                                                                                                            #
#     mgrid node_scoring_example.rb                                                                          #
#     Copyright 2010 Matthew Denwood - matthewdenwood@mac.com - Version 3.00 - June 2010                     #
#                                                                                                            #
#     This file is an example of a scoring script for use with the node ranking features of mgrid, which     #
#     can be used to customise the score assigned to nodes based on their physical characteristics such as   #
#     processor speed and the number of available cores.  These features are only available for users of     #
#     the xgrid setup within the Veterinary Faculty of the University of Glasgow, and this script can safely #
#     be ignored or deleted elsewhere.  See the comments at the start of the 'mgrid.sh' file for more        #
#     details.  This file is written in Ruby, but any scripting language available both locally and on the   #
#     xgrid cluster may be used (ensure that the extension is .rb or .sh, however).  Like any ART script,    #
#     this script must print/echo/put a single value to stdout which represents the score based on the input #
#     values.   Modify and rename this file to 'scoring.rb' (or 'scoring.sh'), and ensure it is either in    #
#     the working directory when calling mgrid, or in the /Library/Application Support/mgrid/" folder for    #
#     a permanent replacement of the default script.                                                         #
#                                                                                                            #
#     The arguments that will be supplied to this script are ordered as follows:                             #
#     xjobs, physical cores, logical cores, server, ram, cpu speed, bits (64 or 32), (ART) cpu type,         #
#     (ART) server, (ART) custom                                                                             #
#                                                                                                            #
#     More details on what exactly these arguments represent follows in the start of the script below.       #
#     The final input value (ART custom) allows a supplied custom ART script (see the -a option to mgrid)    #
#     to harvest information from each node which is not included in the other arguments to this script.     #
#                                                                                                            #
##############################################################################################################

# Current number of jobs running (approximated better as of version 2.1):
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
# Result of the custom art score, which can be used to obtain extra information from a node machine that is not otherwise available here (or 1 if not used):
artcustom = ARGV[9].to_i


###### Code to calculate the node score based on these values is as follows:

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

exit 0

### End of file