\name{runjags}

\alias{runjags} \alias{runJAGS} \alias{runjags-package}
\alias{runJAGS-package} \alias{runjagspackage} \alias{runJAGSpackage}

\title{Interface utilities for Just Another Gibbs Sampler (JAGS) using
parallel and distributed computing}

\description{

This package provides high-level interface utilities for Just Another
Gibbs Sampler (JAGS).  The primary functions facilitate running parallel
JAGS models for multiple chains, automated control of convergence and
sample length diagnostics, evaluation of the performance of a model
against simulated data, and compatibility with the WinBUGS syntax of
model files with data and initial values lists.  Model fitting can be
done locally (via the rjags package or separate JAGS instances) or via
distributed computing clusters such as those provided by snow (a Simple
Network Of Workstations), Xgrid, or other user specified functions.  A
JAGS extension module implementing additional distributions for JAGS
models (including the Pareto family of distributions) is also provided. 
Requires Just Another Gibbs Sampler (JAGS) for most functions, see:
\url{http://mcmc-jags.sourceforge.net}
}

\details{

JAGS is a program which allows analysis of Bayesian models using Markov
chain Monte Carlo (MCMC) simulation, and was developed by Martyn Plummer
to be an alternative to BUGS that ran on UNIX systems as well as Windows
systems (see: \url{http://mcmc-jags.sourceforge.net} for more
information).  The R package rjags is a native R interface to the JAGS
library, and allows a greater level of control for compiled models,
which may be more useful for model development.  This package was
intended to provide additional functions to help automate the process of
running models, including interpretation of WinBUGS type text files
including data and initial values, automated convergence diagnostics,
automated collation and plotting of results, and convinience wrappers
for running models (either individually or for multiple data sets) over
distributed computing cluster such as those provided by snow and Apple's
(now discontinued) Xgrid.

The package also includes a JAGS extension module providing additional
distributions in the Pareto family - for more details see the vignette
in the 'doc' folder of the installed package.

}

\seealso{

\code{\link{run.jags}} and \code{\link{extend.jags}} for basic model
runs

\code{\link{autorun.jags}} and \code{\link{autoextend.jags}} for
automated running of models to convergence and automated calculation of
necessary sample sizes

\code{\link{runjags-class}} for S3 methods relating to runjags objects,
incluing conversion to/from jags objects (for compatibility with the
rjags package)

\code{\link{runjags.options}} for ways to set default options for
runjags functions

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

\code{\link[rjags]{jags.model}} in the rjags package for fine control
over the JAGS libraries

}

\author{Matthew Denwood \email{matthew.denwood@glasgow.ac.uk}}

\references{ M. J. Denwood, "runjags: An R Package Providing Interface
Utilities, Distributed Computing Methods and Additional Distributions
For MCMC Models in JAGS," Journal of Statistical Software, [Under
review].
}

\keyword{methods}
