
\name{read.winbugs}

\alias{read.winbugs}

\alias{read.WinBUGS}

\title{Extract Any Models, Data, Monitored Variables or Initial Values
As Character Vectors from a Winbugs Type Textfile}

\description{

Read a user specified WinBUGS type textfile or character variable and
extract any models, data, monitored variables or initial values as
character vectors.  Used by (auto)run.jagsfile to interpret the input
file(s) or strings.

}

\usage{

read.winbugs(path)

}

\arguments{

\item{path}{either a relative or absolute path to a textfile (including
the file extension) containing a model in the JAGS language and possibly
data and/or initial values, or a character string of the same.  No
default.  The model must be started with the string 'model\{' and ended
with '\}' on new lines.  Data must be similarly started with 'data\{',
monitored variables with 'monitor\{', and initial values as 'inits\{',
and all ended with '\}'.  Seperate variables in such blocks must be
separated by a line break.  If multiple models are found, all but the
first one are ignored with a warning.  Multiple data blocks and monitor
blocks are combined, multiple inits blocks are used for different
chains.  Monitors may also be given using the phrase '#monitor#
variable' within the model block, in which case 'variable' is added to
the list of monitored variables found in the monitor block(s).  The use
of automatically generated data and initial values is also supported
using similar syntax, with '#data# variable' for automatically
generated data variables or '#inits# variable' for automatically
generated initial value variables in which case 'variable' is used as
data or initial values with a value taken by \code{\link{run.jagsfile}}
from datalist, initlist or R objects as appropriate.  '#inits#',
'#data#' and '#monitor#' statements can appear on the same line as
model code, but no more than one of these statements should be used on
the same line.  Examples of acceptable model syntax is given below.}

}

\value{A named list of 'model' containing the model description, 'data'
containing the data given in the data block(s), 'autodata' containing
data variables specified using '#data#' in the model block, 'inits'
containing the initial values given in the initial value block(s),
'autoinits' containing initial value variables specified using
'#inits#' in the model block, and 'monitor' containing the monitored
variables specified in the monitor blocks and by using '#monitor#'
within the model block.  This function is specified primarily for
WinBugs compatibility, so data blocks would normally contain the data in
a list format rather than the code format that is allowed in JAGS to
perform data transformations (see JAGS manual section 7.0.5).  These
JAGS format data blocks can be specified, and the function will attempt
to differentiate the two types of data from the presence of syntactical
cues such as square brackets, for loops, 'list' and .Dim structural
assignments.  If none of these are found, the data block is assumed to
be a WinBugs type data block and is passed to JAGS as data.  This
behaviour can be over-ridden by inserting '#jagsdata#' or
'#bugsdata#' into the data block as appropriate.  More than one data
block is allowed, and each will be differentiated independently.

}

\seealso{

\code{\link{run.jagsfile}}

}



\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk}}



\examples{



\dontrun{

# ALL SYNTAX GIVEN BELOW IS EQUIVALENT

# Use a modified WinBUGS text file with manual inits and manual data and
# a seperate monitor block (requires least modification from a WinBUGS
# file).  For compatibility with WinBUGS, the use of list() to enclose
# data and initial values is allowed and ignored, however all seperate
# variables in the data and inits blocks must be seperated with a line
# break (commas or semicolons before linebreaks are ignored).  'data\{'
# and 'inits\{' must also be added to WinBUGS textfiles so that the
# function can seperate data from initial values.  Iterative loops are
# allowed in data blocks but not in init blocks.  See also the differences
# in JAGS versus WinBUGS syntax in the JAGS help file.

# Contents of a textfile 'mymodel.bug':

model{

	for(i in 1:N){
		Count[i] ~ dpois(mean)
	}
	mean ~ dgamma(0.01, 100)
}

data{
list(Count <- c(1,2,3,4,5,6,7,8,9,10),
N <- 10)

}
inits{
list(
	mean <- 1)
}

inits{
list(
	mean <- 100)
}

monitor{
	mean
}

# end text file

read.winbugs('pathtofile/mymodel.bug')


# Use internal character variable, define monitors in the model, 
# use autodata and manual initial values:

string <- "
model{

	for(i in 1:N){
		Count[i] ~ dpois(mean) #data# Count, N
	}
	mean ~ dgamma(0.01, 100)
	#monitor# mean
}

inits{
	mean <- 1
}

inits{
	mean <- 100
}
"

read.winbugs(string)


# Use autoinits and a mixture of manual and autodata:
string <- "
model{

	for(i in 1:N){ 
		Count[i] ~ dpois(mean) #data# Count
	}
	mean ~ dgamma(0.01, 100) 
	#monitor# mean
	#inits# mean
}

data{

	N <- 10

}
"

read.winbugs(string)

}



}

\keyword{methods}