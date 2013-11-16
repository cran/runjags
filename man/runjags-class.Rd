\name{runjags-class}

\alias{runjags-class}
\alias{runJAGS-class}
\alias{runjagsclass}
\alias{runJAGSclass}
\alias{runjags.bginfo-class}
\alias{runJAGS.bginfo-class}
\alias{runjags.bginfoclass}
\alias{runJAGS.bginfoclass}
\alias{runjags.model-class}
\alias{runJAGS.model-class}
\alias{runjags.modelclass}
\alias{runJAGS.modelclass}
\alias{runjags.data-class}
\alias{runJAGS.data-class}
\alias{runjags.dataclass}
\alias{runJAGS.dataclass}
\alias{failedjags}

\alias{runjags.plots-class}
\alias{runJAGS.plots-class}
\alias{runjags.plotsclass}
\alias{runJAGS.plotsclass}

\alias{runjags.study-class}
\alias{runJAGS.study-class}
\alias{runjags.studyclass}
\alias{runJAGS.studyclass}

\alias{print.runjags}
\alias{summary.runjags}

\alias{print.runjags.model}
\alias{print.runjags.data}
\alias{print.runjags.inits}
\alias{print.runjags.output}
\alias{print.mcse.stats}
\alias{print.crosscorr.stats}
\alias{print.gelman.with.target}
\alias{print.dic.stats}
\alias{print.runjags.study}
\alias{print.runjags.bginfo}

\alias{as.mcmc.runjags}
\alias{as.mcmc.list.runjags}
\alias{as.jags}
\alias{as.runjags}
\alias{as.jags.default}
\alias{as.runjags.default}
\alias{as.jags.runjags}
\alias{as.runjags.jags}


\alias{plot.runjags}
\alias{plot.runjags.plots}
\alias{plot.runjags.bginfo}
\alias{print.runjags.plots}

\title{The 'runjags' class (and associated classes) and available S3 methods}

\description{

Objects of class 'runjags' are produced by the
run.jags/extend.jags/autorun.jags etc functions, and can be passed to
extend.jags and autoextend.jags to extend the simulation.  They also
have a few specific S3 methods for print, plot and extraction of the
MCMC objects contained within the runjags object.

Objects of class 'runjags.study' are produced by the run.jags.study
function, and class 'runjags.bginfo' represents a JAGS model being run
using a background method and can be passed to the results.jags function
to retrieve (a copy of this object will also have been saved to
'jagsinfo.Rsave' in the working directory of the background JAGS call in
case the returned object is not saved by the user).

These functions provide print and plot methods, and conversion
facilities to/from MCMC objects and objects of class 'jags' for
compatibility with the rjags package.

The 'failedjags' environment is used to store JAGS model/data/initial
value files from failed simulations for inspection by the user.

}

\usage{

	\method{print}{runjags}(x, vars=NA, digits = 5, ...)
	\method{plot}{runjags}(x,vars=NA, layout=NA, 
	newwindows=runjags.getOption("newwindows"), file="", type="all", ...)
	
	\method{print}{runjags.model}(x, linenumbers=runjags.getOption("linenumbers"), ...)
	\method{print}{runjags.data}(x, linenumbers=runjags.getOption("linenumbers"), ...)
	\method{print}{runjags.inits}(x, linenumbers=runjags.getOption("linenumbers"), ...)
	\method{print}{runjags.output}(x, linenumbers=runjags.getOption("linenumbers"), ...)
	
	\method{print}{runjags.study}(x,...)
	\method{print}{runjags.bginfo}(x, ...)
	
	\method{as.mcmc}{runjags}(x)
	\method{as.mcmc.list}{runjags}(x, vars=NA, ...)

	\method{as.jags}{runjags}(x, ...)
	\method{as.runjags}{jags}(x, monitor, modules=runjags.getOption('modules'), 
	factories=runjags.getOption('factories'), check=TRUE, 
	jags = runjags.getOption('jagspath'), ...)
	
}

\arguments{

\item{x}{an object of class 'runjags' (as returned by the
\code{\link{run.jags}} or \code{\link{autorun.jags}} functions), or for
the as.runjags method an object of class 'jags' (as returned by the
\code{\link[rjags]{jags.model}} function in the rjags package).}

\item{vars}{an optional character vector of variable names to extract. 
If supplied, only variable names in the object supplied with a partial
match to anything in 'vars' will be summarised/plotted/extracted.  Note
that regular expressions are not allowed, but the caret (^) token can be
used to specify the match at the start of a variable name, and a quoted
vars will be matched exactly.  Default NA meaning all variables
available are returned.}

\item{digits}{the number of significant digits to display for tabulated
statistics.  Default 5.}

\item{layout}{a numeric vector of length 2 representing the number of
rows and columns to produce plots in.  Default 1 plot per page for trace
and density options, or a single row of 2 plots if plotting both types.}

\item{newwindows}{if there are a greater number of variables than will
fit on one page, should new graphics windows be created for each plot or
the exisiting device used for all plots?  Ignored if writing plots to
file.  Default TRUE on Mac/Windows GUI systems, and FALSE otherwise.}

\item{file}{an optional character string representing a filename to save
plots to (as a PDF) rather than using the default graphics device. 
Default "" (ie don't write to file).}

\item{type}{option to produce 'trace' plots, 'density' plots, a
'crosscorr' plot or 'all'.  Cases are ignored and partial matching is
used; the argument may also be of length >1.  Default 'all'.}

\item{linenumbers}{option to prepend lines with line numbers for runjags
model/data/initial value strings.  This may be helpful for debugging
against the output of (failed) JAGS runs.  Default TRUE.}

\item{...}{other options to be passed down to underlying methods where
available (ignored for plot.runjags - see below).}

\item{monitor}{a character vector of the names of variables to monitor. 
No default.}

\item{modules}{external modules to be loaded into JAGS.  More than 1
module can be used.  Default none.}

\item{factories}{factory modules to be loaded into JAGS.  More than 1
factory can be used.  Factories should be in the format
'<facname>(<factype>)', for example:
factories='mix::TemperedMix(sampler)'.  Default none.}

\item{check}{should the runjags object returned be checked to ensure
that an external call to JAGS is able to run the model?  Default TRUE.}

\item{jags}{the system call or path for activating JAGS (ignored if
check==FALSE).  Default calls findjags() to attempt to locate JAGS on
your system.}


}



\value{

The print method for runjags objects displays a range of summary
statistics for the MCMC chains (similar to that produced by
summary.mcmc, but with additional details).

The plot method produces trace and density plots (note that these are
pre-plotted and stored inside the runjags object, so the usual options
to lattice or plot functions are not available).

The as.mcmc method combines the chains (with a warning) and returns an
mcmc object, and the as.mcmc.list method extracts the mcmc.list from the
runjags object (or possibly a sub-selection of variables given by vars).
 See also \code{\link{combine.mcmc}} which can be used directly on
runjags objects.

The print methods for runjags model, data and initial value strings
simply provide the option for printing line numbers which may be useful
for debugging.  The print methods for runjags.study and runjags.bginfo
objects provide a basic overview of the objects.

}

\details{

The runjags class contains the full model, data, modules, factories etc
required to run JAGS and is designed to encapsulate the model in a
similar vein to the 'lm' or 'mer' class.  Most interaction with a
runjags object should be done using the print/plot/as.mcmc.list methods,
but it may also be helpful to access some elements of the list directly
- the names of the elements can be access using 'names(runjags.object)'.
 For example, this is currently the only method of extracting the full
pd/popt/pd.i information.

}


\seealso{

\code{\link{runjags.options}},

\code{\link{combine.mcmc}},

\code{\link{run.jags}},

\code{\link{autorun.jags}},

\code{\link[coda]{summary.mcmc}}

}



\author{Matthew Denwood \email{matthew.denwood@glasgow.ac.uk}}



\keyword{models}