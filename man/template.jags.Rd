% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/template.jags.R
\name{template.jags}
\alias{template.jags}
\alias{template.JAGS}
\title{Generate a generalised linear mixed model (GLMM) specification in JAGS}
\usage{
template.jags(
  formula,
  data,
  file = "JAGSmodel.txt",
  family = "gaussian",
  write.data = TRUE,
  write.inits = TRUE,
  precision.prior = "dgamma(0.001, 0.001)",
  effect.prior = "dnorm(0, 10^-6)",
  n.chains = 2,
  precision.inits = c(0.01, 10),
  effect.inits = c(-1, 1),
  inits = NULL
)

template.JAGS(
  formula,
  data,
  file = "JAGSmodel.txt",
  family = "gaussian",
  write.data = TRUE,
  write.inits = TRUE,
  precision.prior = "dgamma(0.001, 0.001)",
  effect.prior = "dnorm(0, 10^-6)",
  n.chains = 2,
  precision.inits = c(0.01, 10),
  effect.inits = c(-1, 1),
  inits = NULL
)
}
\arguments{
\item{formula}{a formula representation of the desired model, using lme4 style syntax.  Two-way interactions for all variables are permitted, as are random intercepts.}

\item{data}{a data frame containing the variables specified in formula.  This must be specified.}

\item{file}{the filename of the model to output.  This will be over-written if it exists.}

\item{family}{a character string representing the response distribution - options are:  'gaussian', 'binomial', 'Poisson', 'negative binomial', 'ZIB', 'ZIP', 'ZINB' (the latter denote zero-inflated distributions).}

\item{write.data}{option to write the data to file with the model.  If the data is very large it may be better not to write this to file, but the same data frame must be given to the subsequent run.jags call that runs the model.}

\item{write.inits}{option to write the initial values to file with the model.}

\item{precision.prior}{the prior distribution to be used for precision parameters.}

\item{effect.prior}{the prior distribution to be used for linear and fixed effect terms, as well as interactions and the intercept.}

\item{n.chains}{the number of chains to use.}

\item{precision.inits}{a numeric vector of initial values from which the precision parameters in the model will be randomly chosen.  It is recommended to make these over-dispersed, but if the values are too extreme the model may not compile.}

\item{effect.inits}{a numeric vector of initial values from which the effect parameters in the model will be randomly chosen.  It is recommended to make these over-dispersed, but if the values are too extreme the model may not compile.}

\item{inits}{an optional list of named lists to specify initial values for one or more parameters in each chain.  The number of named lists must match n.chains.}
}
\value{
The filename of the created model template.
}
\description{
Use an lme4 style syntax to create a JAGS model representation of a GLMM, including all data, initial values and monitor specifications required to run the model using \code{\link{run.jags}}.
}
\details{
This function is designed to allow new users to MCMC to create relatively simple GLMM models in JAGS using an lme4-style formula interface.  Examining the template created by this function is a good way to learn about how the BUGS language is structured, as well as the options provided by the runjags package.  After generating the template model, the user is encouraged to examine the model file and make whatever changes are necessary before running the model using `run.jags'.  You can also run the models with no changes and compapre the results to those obtained through more standard model fitting approaches to learn more about how the differently presented sets of inference relate to each other.  Note that the effect of the reference level for factors is explicitly given as 0 in output from runjags.  For more about the BUGS language, see Lunn et al (2012).
}
\examples{
\dontrun{
# Create a simple linear model and compare the results to LM:

# This is based on the example in ?lm:
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
D9 <- data.frame(weight, group)
lm.D9 <- lm(weight ~ group, data=D9)

# The JAGS equivalent:
model <- template.jags(weight ~ group, D9, n.chains=2, 
family='gaussian')
JAGS.D9 <- run.jags(model)
summary(JAGS.D9)
summary(lm.D9)
# Note that lm reports sigma and JAGS the precision - to
# make them more comparable we could use a mutate function:
JAGS.D9 <- run.jags(model, mutate=list(prec2sd, 'precision'))
summary(JAGS.D9)
summary(lm.D9)
# Compare the estimated residuals:
plot(residuals(lm.D9), residuals(JAGS.D9, output='mean'))

# For more examples see:
vignette('quickjags', package='runjags')
}
}
\references{
Lunn D, Jackson C, Best N, Thomas A, Spiegelhalter D (2012). The BUGS book: A practical introduction to Bayesian analysis. CRC press; and Matthew J. Denwood (2016). runjags: An R Package Providing Interface Utilities, Model Templates, Parallel Computing Methods and Additional Distributions for MCMC Models in JAGS. Journal of Statistical Software, 71(9), 1-25. doi:10.18637/jss.v071.i09
}
\seealso{
\code{\link{run.jags}} to run the model, \code{\link{add.summary}} for details of summary statistics available from the fitted model, and \code{\link{runjags-class}} for details of how to extract information such as residuals and the fitted values.
}
\keyword{models}
