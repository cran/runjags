# Checks that the runjags module distributions are correct:
library(runjags)
# Require the rjags library to run these checks - it sets the required environmental variables under windows:
if(!require(rjags)) stop("The rjags library is required to run this check")

# Find and load the runjags shared library (only required for these tests and using the rjags call 'load.modue()' so NOT loaded at runtime):
slibpath <- system.file("libs", paste(.Platform$r_arch, if(.Platform$r_arch!="") "/" else "", if(.Platform$OS.type=="unix") "runjags.so" else "runjags.dll", sep=""), package="runjags")
cat("Loading shared library from:  ", slibpath, "\n", sep="")
dyn.load(slibpath)


checkrunjagsmod <- function(distribution, funtype, parameters, x, uselog=FALSE, lower=TRUE){

	if(class(distribution)!="character") stop("The distribution type must be one of par1, par2, par3, par4, lomax, mouch, genpar or hcauchy")
	disttype <- switch(distribution, par1=1, par2=2, par3=3, par4=4, lomax=5, mouch=6, genpar=7, hcauchy=8, 0)
	if(disttype==0) stop("The distribution type must be one of par1, par2, par3, par4, lomax, mouch, genpar or hcauchy")

	if(class(funtype)!="character") stop("The function type must be one of d, p and q")
	dpqr <- switch(funtype, d=1, p=2, q=3, r=4, 0)
	if(dpqr==0) stop("The function type must be one of d, p and q")
	if(dpqr==4) stop("The function type must be one of d, p and q - r is not available")

	npars <- length(parameters)
	N <- length(x)
	values <- numeric(N)
	
	output <- .C('testrunjags',disttype=as.integer(disttype),dpqr=as.integer(dpqr),uselog=as.logical(uselog),lower=as.logical(lower),N=as.integer(N), x=as.double(x),npars=as.integer(npars),parameters=as.double(parameters),values=as.double(values),status=integer(1))

	if(output$status==1) stop("Incorrect number of parameters provided")
	if(output$status==2) stop("Unrecognised distribution type")
	if(output$status==3) stop("Invalid parameter values provided")
	if(output$status==4) stop("Function type not valid")
	if(output$status==5) stop("Function type not valid")

	return(output$values)
	
}

load("moduletargets.Rsave")

checksok <- sapply(1:N, function(i){
	return(all(signif(checkrunjagsmod(tests[[i]]$distribution, tests[[i]]$funtype, tests[[i]]$parameters, tests[[i]]$x, tests[[i]]$uselog, tests[[i]]$lower),sdigits)==results[[i]]))
})

if(!all(checksok)) stop(paste("The runjags module checks failed for test number(s) ", paste(which(!checksok),collapse=","), sep=""))


# Now check the JAGS implementations just to make sure the functions are found OK:

m <- "model{
	
	r0 ~ dpar(1,1)
	f0 ~ dpar(1,1)
	d0 <- dpar(0.5,1,1)
	p0 <- ppar(0.5,1,1)
	q0 <- qpar(0.5,1,1)
	
	r1 ~ dpar1(1,1)
	f1 ~ dpar1(1,1)
	d1 <- dpar1(0.5,1,1)
	p1 <- ppar1(0.5,1,1)
	q1 <- qpar1(0.5,1,1)
	
	r2 ~ dpar2(1,1,0)
	f2 ~ dpar2(1,1,0)
	d2 <- dpar2(0.5,1,1,0)
	p2 <- ppar2(0.5,1,1,0)
	q2 <- qpar2(0.5,1,1,0)

	r3 ~ dpar3(1,0,1)
	f3 ~ dpar3(1,0,1)
	d3 <- dpar3(0.5,1,0,1)
	p3 <- ppar3(0.5,1,0,1)
	q3 <- qpar3(0.5,1,0,1)

	r4 ~ dpar4(1,1,0,1)
	f4 ~ dpar4(1,1,0,1)
	d4 <- dpar4(0.5,1,1,0,1)
	p4 <- ppar4(0.5,1,1,0,1)
	q4 <- qpar4(0.5,1,1,0,1)

	rl ~ dlomax(1,1)
	fl ~ dlomax(1,1)
	dl <- dlomax(0.5,1,1)
	pl <- plomax(0.5,1,1)
	ql <- qlomax(0.5,1,1)
	
	rm ~ dmouch(1)
	fm ~ dmouch(1)
	dm <- dmouch(0.5,1)
	pm <- pmouch(0.5,1)
	qm <- qmouch(0.5,1)
	
	rg ~ dgenpar(1,1,1)
	fg ~ dgenpar(1,1,1)
	dg <- dgenpar(0.5,1,1,1)
	pg <- pgenpar(0.5,1,1,1)
	qg <- qgenpar(0.5,1,1,1)

#	rh ~ dhcauchy(25)
#	fh ~ dhcauchy(25)
#	dh <- dhcauchy(25, 25) 
#	ph <- phcauchy(25, 25) 
#	qh <- qhcauchy(0.5, 25) 
	
	#monitor# r0,d0,p0,q0,r1,d1,p1,q1,r2,d2,p2,q2,r3,d3,p3,q3,r4,d4,p4,q4,rl,dl,pl,ql,rg,dg,pg,qg,rm,dm,pm,qm, 
	#rh,dh,ph,qh
	#data# f0, f1, f2, f3, f4, fl, fg, fm
	#fh
	#inits# rg
}"
rg <- list(1,2)
f0=f1=f2=f3=f4=fl=fg=fm=fh <- 2

r <- run.jags(m, n.chains=2, method='rjags')
stopifnot(nchain(as.mcmc.list(r))==2)


# Now check that my p and q functions are symmetric:

m <- "model{
	
	qsn <- qnorm(sp, cont1, pos1)
	psn <- pnorm(qsn, cont1, pos1)

	qsg <- qgamma(sp, pos1, pos2)
	psg <- pgamma(qsg, pos1, pos2)
	
	qsl <- qlnorm(sp, cont1, pos2)
	psl <- plnorm(qsl, cont1, pos2)

#  There appears to be a bug in the code for qpar:	
#	qsp <- qpar(sp, pos1, pos2)
#	psp <- ppar(qsp, pos1, pos2)
	
	qsp1 <- qpar1(sp, pos1, pos2)
	psp1 <- ppar1(qsp1, pos1, pos2)
	
	qsp2 <- qpar2(sp, pos1, pos2, cont1)
	psp2 <- ppar2(qsp2, pos1, pos2, cont1)
	
	qsp3 <- qpar3(sp, pos1, cont1, pos3)
	psp3 <- ppar3(qsp3, pos1, cont1, pos3)
	
	qsp4 <- qpar4(sp, pos1, pos2, cont1, pos3)
	psp4 <- ppar4(qsp4, pos1, pos2, cont1, pos3)
	
	qsgp <- qgenpar(sp, pos1, cont2, cont1)
	psgp <- pgenpar(qsgp, pos1, cont2, cont1)
	
	qslm <- qlomax(sp, pos1, pos2)
	pslm <- plomax(qslm, pos1, pos2)
	
	qsm <- qmouch(sp, pos1)
	psm <- pmouch(qsm, pos1)
	
#	qshc <- qhcauchy(sp, pos1)
#	pshc <- phcauchy(qshc, pos1)
	
	dummy ~ dlomax(1,1)
	
	#monitor# psn, psg, psl, psp, psp1, psp2, psp3, psp4, psgp, pslm, psm, 
	#qshc, pshc
	#data# sp, cont1, cont2, pos1, pos2, pos3
	#inits# dummy
}"

dummy <- 2
sp <- 1:9/10
cont1 <- runif(1,-10,10)
cont2 <- runif(1,-10,10)
pos1 <- rgamma(1,1,1)
pos2 <- rgamma(1,1,1)
pos3 <- rgamma(1,1,1)

r <- run.jags(m, sample=10, n.chains=2, method='rjags')

ps <- combine.mcmc(r,vars='ps')[1,]
stopifnot(all(signif(sp)==signif(ps)))


cat("All module checks passed\n")
