
\name{testjags}

\alias{testjags}

\alias{testJAGS}

\title{Analyse the System to Check That Jags Is Installed}

\description{

Test the users system to determine the operating system, version of R
installed, and version of JAGS installed.  Some information is collected
from other functions such as .platform and Sys.info.  Used by the
run.jags function.

}

\usage{

testjags(jags=findjags(), silent=FALSE)}

\arguments{

\item{jags}{the system call or path for activating JAGS.  Default calls
findjags() to attempt to locate JAGS on your system automatically.  In
unix the system call should always be 'jags', in Windows a path to the
JAGS executable or the enclosing /bin or /JAGS folder is required.}

\item{silent}{should on-screen feedback be suppressed?  Default FALSE.}

}

\value{The following self-explanatory information is returned:  os,
JAGS.available (logical), JAGS.path, popen.support (internalisation of
terminal output), JAGS.version, R.version, R.GUI, R.package.type,
username.

}

\seealso{

\code{\link{run.jags}},
\code{\link{findjags}}

}



\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk}}



\examples{
# Run the function to determine if JAGS is installed:
results <- testjags()
}

\keyword{methods}