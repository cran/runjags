
\name{runjags}

\alias{runjags}

\alias{runJAGS}

\alias{runjags-package}

\alias{runJAGS-package}

\title{Run Bayesian MCMC Models in the BUGS syntax from Within R}

\description{

A set of functions to allow any user specified model to be run in JAGS
from within R, returning the MCMC chains as R objects.  Includes
functions to read external WinBUGS type textfiles, and allows several
ways of automatically specifying model data from existing R objects or R
functions.  Also includes functions to automatically calculate model run
length, autocorrealtion and Gelman Rubin statistic diagnostics for all
models to simplify the process of achieving chain convergence.  Designed
for maximum compatibility with WinBUGS syntax, although minor
modification to existing .bug files will be required.  Also provides
functions for submission and retrieval of jobs (both JAGS runs and
user-specified R code execution) to Apple Xgrid distributed computing
clusters (Mac OS X machines only).  Requires Just Another Gibbs Sampler
(JAGS) for most functions, see:
\url{http://www-fis.iarc.fr/~martyn/software/jags/}

}

\details{



JAGS is a program which allows analysis of Bayesian models using Markov
chain Monte Carlo (MCMC) simulation, and was developed by Martyn Plummer
to be an alternative to BUGS that ran on UNIX systems as well as Windows
systems (see: \url{http://www-fis.iarc.fr/~martyn/software/jags/} for
more information).  The R package rjags is a native R interface to the
JAGS library, and allows a far greater level of control and analysis of
compiled models, which is often more useful for model development.  This
package was intended to provide additional functions to help automate
the process of running models, including interpretation of WinBUGS type
text files including data and initial values, automated retrieval of
objects within the working environment to be passed as data or initial
values to the model, automated convergence diagnostics, and automated
collation and plotting of results.  The package also includes functions
for running JAGS models, and any other user specified R code, over Xgrid
distributed computing clusters from within R (requires Mac OS X and
access to an Xgrid system).



}



\seealso{

\code{\link{run.jags}} for basic model runs

\code{\link{autorun.jags}} for automated running of models to
convergence and automated calculation of necessary sample sizes

\code{\link{run.jagsfile}} and \code{\link{autorun.jagsfile}} for
running of local WinBUGS style text files, as well as methods of
automating retrieval of data and initial values from local text files or
character strings

\code{\link{read.winbugs}} for translation of WinBUGS text files into
JAGS compatible model, data and initial values files

\code{\link{combine.mcmc}} and \code{\link{dump.format}} for MCMC
related tools

\code{\link{timestring}}, \code{\link{new_unique}} and \code{\link{ask}}
for more general tools

\code{\link{xgrid.run.jags}} and \code{\link{xgrid.submit.jags}} for use
of Xgrid clusters to run JAGS models remotely

\code{\link{xgrid.run}} and \code{\link{xgrid.submit}} for use of Xgrid
clusters for remote execution of user specified R code

\code{\link[rjags]{jags.model}} in the rjags package for fine control over the JAGS libraries

}



\author{Matthew Denwood \email{matthew.denwood@glasgow.ac.uk}}

\keyword{methods}
