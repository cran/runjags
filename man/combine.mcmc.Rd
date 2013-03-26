\name{combine.mcmc}
\alias{combine.mcmc}
\alias{combine.MCMC}

\title{Combine Two or More MCMC Objects Into One Longer MCMC Object}
\description{
   Allows an MCMC object (with 1 or more chains) to be combined with
   another (or several other) MCMC object(s) representing extensions of
   the same simulation, to produce one MCMC object that contains the
   continuous combined Markov chains of the other MCMC objects.  Alternatively,
   a single MCMC list object can be converted into an MCMC object with one chain
   by combining all chains from the MCMC list.
}
\usage{
combine.mcmc(mcmc.objects=list(), thin=1, return.samples=NA, 
collapse.chains=if(length(mcmc.objects)==1) TRUE else FALSE, vars=NA)
}
\arguments{

\item{mcmc.objects}{a list of MCMC or runjags objects, all with the same number of
chains and matching variable names, or a single MCMC object/list or runjags object.  No default.}

\item{thin}{an integer to use to thin the (final) MCMC object by, in
addition to any thinning already applied to the objects before being
passed to combine.mcmc.  Ignored if return.samples is specified (!is.na). 
Default 1 (no additional thinning is performed).}

\item{return.samples}{the number of samples to return after thinning. 
The chains will be thinned to as close to this minimum value as
possible, and any excess iterations discarded.  Supersedes thin if 
both are specified.  Ignored if niter(mcmc.objects) < return.samples.  
Default NA.}

\item{collapse.chains}{option to combine all MCMC chains into a single
MCMC chain with more iterations.  Can be used for combining chains prior
to calculating results in order to reduce the Monte Carlo error of
estimates.  Default TRUE if a single mcmc.object is provided, or FALSE
otherwise.}

\item{vars}{an optional character vector of variable names to extract. 
If supplied, only variable names in the MCMC object/list supplied with a partial
match to anything in 'vars' will be summarised/plotted/extracted.  Note
that regular expressions are not allowed, but the caret (^) token can be
used to specify the match at the start of a variable name, and a quoted
vars will be matched exactly.  Default NA meaning all variables
available are returned.}

}

\value{
An MCMC object if collapse.chains==TRUE or a list of MCMC objects is
supplied, or an mcmc.list object if given a list of (or single) mcmc.list
or runjags objects (and collapse.chains==FALSE)

}

\seealso{
   \code{\link{run.jags}},
   
   \code{\link{runjags-class}}
}

\author{Matthew Denwood \email{matthew.denwood@glasgow.ac.uk}}


\keyword{methods}