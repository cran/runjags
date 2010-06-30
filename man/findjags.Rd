\name{findjags}
\alias{findjags}
\alias{findJAGS}
\title{Attempt to Locate a JAGS Install}
\description{
   Search the most likely locations for JAGS to be installed on the
   users system, based on the operating system, and return the most
   likely path to try.  Where multiple installs exist, findjags will
   attempt to return the path to the install with the highest version
   number.  For Unix systems, calling jags using 'jags' requires the
   jags binary to be in the search path, which may be specified in your
   user '.Profile' if necessary (the JAGS executable is also looked for
   in the default install location of /usr/local/bin/jags if popen
   support is enabled).
}
\usage{
findjags(ostype = .Platform$OS.type)}
\arguments{
   \item{ostype}{the operating system type.  There is probably no reason
   to want to change this...}
}
\value{A path or command for the most likely location of JAGS on the
system.  On unix this will always be 'jags', on Windows for example
"C:/Program Files/JAGS/bin/jags-terminal.exe" or "C:/Program
Files/JAGS/JAGS-1.0.0/bin/jags-terminal.exe"
}
\seealso{
   \code{\link{testjags}},
   
   \code{\link{run.jags}}
}

\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk}}

\examples{

findjags()

}
\keyword{methods}