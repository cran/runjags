% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.module.runjags.R
\name{load.runjagsmodule}
\alias{load.runjagsmodule}
\alias{load.runJAGSmodule}
\alias{unload.runjagsmodule}
\alias{unload.runJAGSmodule}
\title{Load the internal JAGS module provided by runjags}
\usage{
load.runjagsmodule(fail = TRUE, silent = FALSE)

unload.runjagsmodule()

load.runJAGSmodule(fail = TRUE, silent = FALSE)

unload.runJAGSmodule()
}
\arguments{
\item{fail}{should the function fail using stop() if the module cannot be loaded?}

\item{silent}{if !fail, the function will by default print a diagnostic message if the module cannot be loaded - the silent option suppresses this message.}
}
\value{
Invisibly returns TRUE if able to (un)load the module, or FALSE otherwise
}
\description{
The runjags package contains a JAGS extension module that provides several additional distributions for use within JAGS (see details below).  This function is a simple wrapper to load this module.  The version of the module supplied within the runjags package can only be used with the rjags package, or with the rjags or rjparallel methods within runjags.  For a standalone JAGS module for use with any JAGS method (or independent JAGS runs) please see:  https://sourceforge.net/projects/runjags/files/paretoprior/
}
\details{
This module provides the following distributions for JAGS:

PARETO TYPE I:  dpar1(alpha, sigma)
\deqn{
  p(x) = \alpha \sigma^{\alpha} x^{-\left(\alpha+1 \right)}
}{
  p(x) = \alpha \sigma^\alpha x^-(\alpha+1)
}

\deqn{\alpha > 0, \sigma > 0, x > \sigma}{\alpha > 0, \sigma > 0, x > \sigma}

PARETO TYPE II:  dpar2(alpha, sigma, mu)

\deqn{
  p(x) = \frac{\alpha}{\sigma} \left( \frac{\alpha + x - \mu}{\sigma}\right)^{-\left(\alpha+1\right)}
}{
  p(x) = (\alpha / \sigma) ((\alpha + x - \mu) / (\sigma)) ^ -(\alpha+1)
}

\deqn{\alpha > 0, \sigma > 0, x > \mu}{\alpha > 0, \sigma > 0, x > \mu}

PARETO TYPE III:  dpar3(sigma, mu, gamma)

\deqn{
  p(x) = \frac{\frac{x-\mu}{\sigma}^{\frac{1}{\gamma}-1} \left(\frac{x-\mu}{\sigma}^{\frac{1}{\gamma}} +1\right)^{-2}}{\gamma \sigma}
}{
  p(x) = (((x-\mu)/\sigma)^(1/\gamma -1) (((x-\mu)/\sigma)^(1/\gamma) +1)^-2) / (\gamma \sigma)
}

\deqn{\sigma > 0, \gamma > 0, x > \mu}{\sigma > 0, \gamma > 0, x > \mu}

PARETO TYPE IV:  dpar4(alpha, sigma, mu, gamma)

\deqn{
  p(x) = \frac{\alpha \frac{x-\mu}{\sigma}^{\frac{1}{\gamma}-1} \left(\frac{x-\mu}{\sigma}^{\frac{1}{\gamma}} +1\right)^{-\left(\alpha+1\right)}}{\gamma \sigma}
}{
  p(x) = (\alpha((x-\mu)/\sigma)^(1/\gamma -1) (((x-\mu)/\sigma)^(1/\gamma) +1)^-(\alpha+1)) / (\gamma \sigma)
}

\deqn{\alpha > 0, \sigma > 0, \gamma > 0, x > \mu}{\alpha > 0, \sigma > 0, \gamma > 0, x > \mu}

LOMAX:  dlomax(alpha, sigma)

\deqn{
  p(x) = \frac{\alpha}{\sigma} \left(1 + \frac{x}{\sigma}\right)^{-\left(\alpha+1\right)}
}{
  p(x) = (\alpha / \sigma) (1 + (x / \sigma)) ^ -(\alpha+1)
}

\deqn{\alpha > 0, \sigma > 0, x > 0}{\alpha > 0, \sigma > 0, x > 0}

GENERALISED PARETO:  dgenpar(sigma, mu, xi)

\deqn{
  p(x) = \frac{1}{\sigma} \left(1 + \xi \left(\frac{x-\mu}{\sigma}\right)\right)^{-\left(\frac{1}{\xi}+1\right)}
}{
  p(x) = (1 / \sigma) (1 + \xi ((x-\mu) /  sigma))^-(1/\xi + 1)
}

For \eqn{\xi=0}{\xi=0}:

\deqn{
    p(x) = \frac{1}{\sigma} e^{\frac{-\left(x-\mu\right)}{\sigma}}
  }{
    p(x) = (1 / \sigma) e^(-(x-\mu)/\sigma)
  }

\deqn{\sigma > 0, x > \mu}{\sigma > 0, x > \mu}

DUMOUCHEL:  dmouch(sigma)

\deqn{
  p(x) = \frac{\sigma}{\left(x+\sigma\right)^2}
}{
  p(x) = \sigma / ((x + \sigma)^2)
}

\deqn{\sigma > 0, x > 0}{\sigma > 0, x > 0}

HALF CAUCHY:  dhalfcauchy(sigma)

\deqn{
  p(x) = \frac{2 \sigma}{\pi \left(x^2+\sigma^2\right)}
}{
  p(x) = 2 \sigma / (\pi (x^2 + \sigma^2))
}

\deqn{\sigma > 0, x > 0}{\sigma > 0, x > 0}

For an easier to read version of these PDF equations, see the userguide vignette.
}
\examples{
# Load the module for use with any rjags model:
available <- load.runjagsmodule(fail=FALSE)
if(available){
# A simple model to sample from a Lomax distribution.
# (Requires the rjags or rjparallel methods)
m <- "model{
 L ~ dlomax(1,1)
		}"
\dontrun{
results <- run.jags(m, monitor="L", method="rjags", modules="runjags")
}
}
}
\references{
Denwood, M.J. 2016. runjags: An R Package Providing Interface Utilities, Model Templates, Parallel Computing Methods and Additional Distributions for MCMC Models in JAGS. J. Stat. Softw. 71. doi:10.18637/jss.v071.i09.
}
\seealso{
\code{\link{runjags-class}}, \code{\link[rjags]{load.module}}
}
\keyword{methods}
