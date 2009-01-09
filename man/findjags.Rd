\name{findjags}
\alias{findjags}
\alias{findJAGS}
\title{Attempt to Locate a JAGS Install}
\description{
   Search the most likely locations for JAGS to be installed on the users system, based on the operating system, and return the most likely path to try.  Where multiple installs exist, findjags will attempt to return the path to the install with the highest version number.
}
\usage{
findjags(ostype = .Platform$OS.type)}
\arguments{
   \item{ostype}{the operating system type.  There is probably no reason to want to change this...}
}
\value{A path or command for the most likely location of JAGS on the system.  On unix this will always be 'jags', on Windows for example "C:/Program Files/JAGS/bin/jags-terminal.exe" or "C:/Program Files/JAGS/JAGS-1.0.0/bin/jags-terminal.exe"
}
\seealso{
   \code{\link{testjags}}
   \code{\link{run.jags}}
}

\author{Matthew Denwood \email{m.denwood@vet.gla.ac.uk} funded as part of the DEFRA VTRI project 0101.}

\examples{

findjags()

}
\keyword{methods}