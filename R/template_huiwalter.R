#' Create a Hui-Walter model based on paired test data for an arbitrary number of tests and populations
#'
#' @param testdata the input paired test data, where each column name corresponds to a test result - except possibly "ID" which is ignored, and "Population" indicating a population identifier for that row. Each row must represent test results from the same individual either as logical or a factor with two levels (and where the first level indicates a negative test result). Data may be missing at random (except for Population).
#' @param outfile the name of the text file to save the model representation
#' @param covariance a data frame specifying which conditional depdendence terms should be included (either activated or deactivated) with columns Test_A, Test_B, Active_Se and Active_Sp. A single logical FALSE is allowed for back-compatibility, and a single logical TRUE is also currently allowed but is deprecated.
#' @param se_priors the priors to use for sensitivity parameters (can be adjusted in the model once it is generated)
#' @param sp_priors the priors to use for specificity parameters (can be adjusted in the model once it is generated)
#' @param prev_priors the priors to use for prevalence parameters (can be adjusted in the model once it is generated)
#' @param cov_as_cor option for the prior for covariance terms to be put on the correlation rather than covariance directly (deprecated; currently ignored with a warning)
#' @param specify_populations option for the active populations to be retrieved from a PopulationsUsing vector in the R environment - this facilitates sensitivity analysis by excluding subsets of populations without re-generating the model
#' @param outcome_check option to facilitate comparing the observed tallies to the predicted tallies in order to assess model fit
#' @param check_min_obs the minimum number of total observations required before an outcome_check is generated (this prevents e.g. outcome checks being generated for partially missing data)
#'
#' @examples
#' N <- 600
#' status <- rbinom(N, 1, rep(c(0.25,0.5,0.75), each=N/3))
#' testdata <- data.frame(Population = rep(1:3, each=N/3),
#'     FirstTest = rbinom(N, 1, status*0.95 + (1-status)*0.05),
#'     SecondTest = rbinom(N, 1, status*0.75 + (1-status)*0.02),
#'     ThirdTest = rbinom(N, 1, status*0.5 + (1-status)*0.01)
#' )
#' template_huiwalter(testdata, outfile="huiwalter_model.txt",
#'     covariance=data.frame(
#'       Test_A="FirstTest",
#'       Test_B="SecondTest",
#'       Active_Se=TRUE, Active_Sp=FALSE
#'     )
#' )
#'
#' ## Then examine and verify the code manually!
#' cat(readLines("huiwalter_model.txt"), sep="\n")
#'
#' ## Before running the model:
#' \dontrun{
#'   results <- run.jags("huiwalter_model.txt")
#'   results
#' }
#'
#' ## Cleanup:
#' unlink("huiwalter_model.txt")
#'
#' @export


template_huiwalter <- function(testdata, outfile='huiwalter_model.txt', covariance=data.frame(Test_A=character(0), Test_B=character(0), Active=logical(0)), se_priors='dbeta(1,1)', sp_priors='dbeta(1,1)', prev_priors='dbeta(1,1)', cov_as_cor=FALSE, specify_populations=FALSE, outcome_check=TRUE, check_min_obs=20L){

  ppp_values <- FALSE
  single_check <- FALSE
  agreement_check <- FALSE

	stopifnot(is.data.frame(testdata))

	populations_using <- specify_populations
	residual_check <- outcome_check
	residual_min_obs <- check_min_obs

	## R code to generate a Hui-Walter model for N tests and P populations, with potential missing data

	# Function could be used as either just test names so generates code for no missing,
	# or data frame containing the data plus/minus some missing
	# If both given then test names tells us which columns to use, otherwise all columns not ID or Population
	# If a single population then do away with that loop

	# Note it is VERY important that the observations are missing at random

	# TODO:
	# allow 2-test (no covariance) and 1 population models
	# argument for test name (and population column) from data frame
	# option for no data frame, just create model for given N population/tests
	# testing

	if(is.null(testdata$Population)){
		testdata$Population <- factor(1)
	}
	if(!is.factor(testdata$Population)){
		testdata$Population <- factor(testdata$Population)
	}
	stopifnot(all(!is.na(testdata$Population)))
	npop <- length(levels(testdata$Population))

	fulltestdata <- testdata
	fulltestdata[[".order"]] <- seq_len(nrow(fulltestdata))

	## TODO: document
	testdata[["SampleID"]] <- NULL

	## Initialise the file:
	cat('## Auto-generated Hui-Walter model created by runjags version ', runjagsprivate$runjagsversion, ' on ', as.character(Sys.Date()), '\n\nmodel{\n\n\t## Observation layer:', sep='', file=outfile, append=FALSE)
	catapp <- function(...){
	  cat(..., sep='', file=outfile, append=TRUE)
	}

	## Some variables that are needed in a few places:
	testcols <- names(testdata)[!names(testdata) %in% c('ID','Population')]
	stopifnot(length(testcols)>=2)

	if(is.list(se_priors)){
	  stopifnot(all(testcols %in% names(se_priors)) || "default" %in% names(se_priors))
	  se_priors <- sapply(testcols, function(x) if(x %in% names(se_priors)) se_priors[[x]] else se_priors[["default"]])
	}else if(length(se_priors)==1){
		se_priors <- rep(se_priors, length(testcols))
	}
	stopifnot(length(se_priors)==length(testcols))

	if(is.list(sp_priors)){
	  stopifnot(all(testcols %in% names(sp_priors)) || "default" %in% names(sp_priors))
	  sp_priors <- sapply(testcols, function(x) if(x %in% names(sp_priors)) sp_priors[[x]] else sp_priors[["default"]])
	}else if(length(sp_priors)==1){
		sp_priors <- rep(sp_priors, length(testcols))
	}
	stopifnot(length(sp_priors)==length(testcols))

	if(is.list(prev_priors)){
	  stopifnot(all(levels(testdata$Population) %in% names(prev_priors)) || "default" %in% names(prev_priors))
	  prev_priors <- sapply(levels(testdata$Population), function(x) if(x %in% names(prev_priors)) prev_priors[[x]] else prev_priors[["default"]])
	}else if(length(prev_priors)==1){
	  prev_priors <- rep(prev_priors, npop)
	}
	stopifnot(length(prev_priors)==npop)


	# Make sure the tests are interpretable as logical and then convert to factor:
	for(col in testcols){
		if(is.logical(testdata[[col]])){
			testdata[[col]] <- factor(as.numeric(testdata[[col]]), levels=0:1)
		}
		if(is.numeric(testdata[[col]])){
			stopifnot(all(na.omit(testdata[[col]]) %in% c(0,1)))
			testdata[[col]] <- factor(as.numeric(testdata[[col]]), levels=0:1)
		}
		stopifnot(is.factor(testdata[[col]]))
		stopifnot(length(levels(testdata[[col]]))==2)
		levels(testdata[[col]]) <- 0:1
	}

	ntests <- length(testcols)
	ncomb <- 2^ntests
	testarr <- array(1:ncomb, dim=rep(2,length(testcols)), dimnames=lapply(testcols, paste0, c('-','+')))
	teststrings <- expand.grid(lapply(dimnames(testarr), paste0, ' '), stringsAsFactors=FALSE)

	if(is.logical(covariance)){
	  stopifnot(length(covariance)==1L)
	  if(is.na(covariance)){
	    covariance <- data.frame(Test_A = character(0), Test_B = character(0), Active_Se = logical(0), Active_Sp = logical(0))
	  }else if(isFALSE(covariance)){
	    covariance <- expand.grid(Test_A = factor(testcols, levels=testcols), Test_B = factor(testcols, levels=testcols), Active_Se=FALSE, Active_Sp=FALSE)[upper.tri(diag(length(testcols))),,drop=FALSE]
	  }else{
	    covariance <- expand.grid(Test_A = factor(testcols, levels=testcols), Test_B = factor(testcols, levels=testcols), Active_Se=TRUE, Active_Sp=TRUE)[upper.tri(diag(length(testcols))),,drop=FALSE]
	    warning("Setting covariance=TRUE is now deprecated; please specify a data frame of desired covariance terms instead (see ?template_huiwalter)")
	  }
	}else{
	  stopifnot(is.data.frame(covariance))
	  if("Active" %in% names(covariance)){
	    if(!"ActiveSe" %in% names(covariance)) covariance[["Active_Se"]] <- covariance[["Active"]]
	    if(!"ActiveSp" %in% names(covariance)) covariance[["Active_Sp"]] <- covariance[["Active"]]
	  }
	  stopifnot(c("Test_A","Test_B","Active_Se","Active_Sp") %in% names(covariance))
	}

	testpairs <- merge(
	  expand.grid(Test_A = factor(testcols, levels=testcols), Test_B = factor(testcols, levels=testcols))[upper.tri(diag(length(testcols))),,drop=FALSE],
	  covariance[,c("Test_A","Test_B","Active_Se","Active_Sp")],
	  by = c("Test_A", "Test_B"), all = TRUE, sort=TRUE
	)
	ntestpairs <- nrow(testpairs)
	testpairs[["Index"]] <- seq_len(ntestpairs)
	fac_to_in <- function(x) gsub(" ", "0", format(as.numeric(x)))
	testpairs[["Suffix"]] <- paste0("_", fac_to_in(testpairs[["Test_A"]]), "_", fac_to_in(testpairs[["Test_B"]]))
	testpairs[["Test_AI"]] <- as.numeric(testpairs[["Test_A"]])
	testpairs[["Test_BI"]] <- as.numeric(testpairs[["Test_B"]])

	args <- lapply(testcols, function(x) c(0,1))
	names(args) <- testcols
	outcomes <- do.call('expand.grid', args)

	# testcombos <- unique(na.omit(t(apply(expand.grid(1:ntests, 1:ntests), 1, function(x) if(x[1]==x[2]) c(NA,NA) else sort(x)))))
	testcombos <- as.matrix(testpairs[,c("Test_AI","Test_BI")])
	testagree <- apply(testcombos,1,function(x) outcomes[[x[1]]] == outcomes[[x[2]]])
	dimnames(testagree) <- list(NULL, paste0('cc', testpairs[["Suffix"]]))

  datalist <- list(PopulationsUsing = seq_len(npop), AcceptTest=rep(1,ntests), AcceptProb=matrix(1, nrow=ncomb, ncol=npop))
  if(populations_using) datalist[["PopulationsUsing"]] <- NULL
	datablock <- dump.format(datalist)

	## Check how many covariances are active:
	nactivecov <- sum(sapply(testpairs[["Active_Se"]], isTRUE)) + sum(sapply(testpairs[["Active_Sp"]], isTRUE))
	if(nactivecov > (0.34*nrow(testpairs)*2L)) warning("You have specified a lot of active conditional depdendence terms, which is not recommended; if you really need this many conditional depdendence terms then you should probably use an alternative model formulation")

	nsum <- 0

	## Helper function:
	writeobs <- function(tcode, ncomb, nobs=Inf){

	  catapp('for(p in PopulationsUsing){\n\t\tTally_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n')

	  ## For partially missing obs we need to take into account multiple se/sp_probs:
	  if(grepl("M",tcode)){

	    tcm <- strsplit(tcode, "")[[1]] == "R"
	    ardims <- do.call(expand.grid, c(lapply(tcm, function(x) if(x) 0 else 1:2), list(stringsAsFactors=FALSE)))

	    indexes <- vapply(seq_len(nrow(ardims)), function(i){
	      do.call(`[`, c(list(testarr), lapply(ardims[i,], function(x) if(x==0) 1:2 else x)))
	    }, numeric(2^sum(tcm)))
	    stopifnot(length(indexes)==2^length(tcm) && all(testarr %in% indexes))
	    arrin <- paste0('[c(',apply(indexes,2,paste,collapse=','),'),p]')

      lt <- paste0('(prev[p] * se_prob', arrin, ') + ((1-prev[p]) * sp_prob', arrin, ')', collapse=' +\n\t\t\t\t\t\t\t')
	  }else{
	    lt <- paste0('(prev[p] * se_prob[1:', ncomb, ',p]) + ((1-prev[p]) * sp_prob[1:', ncomb, ',p])')
	  }

	  catapp('\t\tprob_', tcode, '[1:', ncomb, ',p] <- ', lt, '\n')
	  if(residual_check && nobs >= residual_min_obs){
	    catapp('\tsim_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n\t')
	    catapp('\t\tcheck_outcome_', tcode, '[1:', ncomb, ',p] <- Tally_', tcode, '[1:', ncomb, ',p] - sim_', tcode, '[1:', ncomb, ',p]\n\t')
	    rv <- paste0(", check_outcome_", tcode)
	  }else{
	    rv <- ""
	  }
	  catapp('\t}')

	  # if(populations_using){
	  #   catapp('for(p in PopulationsUsing){\n\t\tTally_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n\t')
	  #   if(residual_check){
	  #     catapp('\tsim_tally_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n\t')
	  #     catapp('\tresidual_', tcode, '[1:', ncomb, ',p] <- Tally_', tcode, '[1:', ncomb, ',p] - sim_tally_', tcode, '[1:', ncomb, ',p]\n\t')
	  #   }
	  #   catapp('}\n\tfor(p in 1:Populations){\n\t\tprob_', tcode, '[1:', ncomb, ',p] <- (prev[p] * se_prob[1:', ncomb, ',p]) + ((1-prev[p]) * sp_prob[1:', ncomb, ',p])\n\t}')
	  # }else{
	  #   catapp('for(p in 1:Populations){\n\t\tTally_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n\t')
	  #   if(residual_check){
	  #     catapp('\tsim_tally_', tcode, '[1:', ncomb, ',p] ~ dmulti(prob_', tcode, '[1:', ncomb, ',p], N_', tcode, '[p])\n\t')
	  #     catapp('\tresidual_', tcode, '[1:', ncomb, ',p] <- Tally_', tcode, '[1:', ncomb, ',p] - sim_tally_', tcode, '[1:', ncomb, ',p]\n\t')
	  #   }
	  #   catapp('\tprob_', tcode, '[1:', ncomb, ',p] <- (prev[p] * se_prob[1:', ncomb, ',p]) + ((1-prev[p]) * sp_prob[1:', ncomb, ',p])\n\t}')
	  # }

    return(rv)
	}

	resid_monitors <- character(0L)

	## Complete observations (if there are any):
	if(nrow(na.omit(testdata[,testcols])) > 0){
		tcode <- paste0(rep('R',ntests), collapse='')
		tdata <- na.omit(testdata[,c(testcols,'Population')])
		tabdata <- vapply(seq_len(npop), function(x) as.numeric(do.call(`[`, c(list(table(tdata)), lapply(1:length(testcols), function(y) 1:2), list(x)))), numeric(2^length(testcols)))
		stopifnot(all(dim(tabdata)==c(ncomb,npop)))
		dlist <- list(table(tdata$Population), tabdata)
		names(dlist) <- c(paste0('N_', tcode), paste0('Tally_', tcode))
		datablock <- c(datablock, dump.format(dlist))
		nsum <- nsum+sum(tabdata)


		catapp('\n\n\t# Complete observations (N=', sum(tabdata), '):\n\t')
		resid_monitors <- c(resid_monitors, writeobs(tcode, ncomb))
	}

	## Partially missing observations (whatever combinations):
	presencecombos <- unique(!is.na(testdata[,testcols]))
	# Remove the complete set and completely missing (if present):
	presencecombos <- presencecombos[! apply(presencecombos,1,sum) %in% c(0,ntests), , drop=FALSE]

	for(pc in seq_len(nrow(presencecombos))){
		tcode <- paste0(c('M','R')[presencecombos[pc,]+1], collapse='')
		tcomb <- 2^sum(presencecombos[pc,])

		tdata <- testdata[,c(testcols,'Population')]
		tdata <- tdata[apply(!is.na(tdata[,1:length(testcols)]),1,function(x) all(x==presencecombos[pc,])), c(presencecombos[pc,],TRUE)]
		tabdata <- vapply(seq_len(npop), function(x) as.numeric(do.call(`[`, c(list(table(tdata)), lapply(1:sum(presencecombos[pc,]), function(y) 1:2), list(x)))), numeric(2^sum(presencecombos[pc,])))
		stopifnot(all(dim(tabdata)==c((2^sum(presencecombos[pc,])), npop)))
		dlist <- list(table(tdata$Population), tabdata)
		names(dlist) <- c(paste0('N_', tcode), paste0('Tally_', tcode))
		datablock <- c(datablock, dump.format(dlist))
		nsum <- nsum+sum(tabdata)

		ardims <- do.call(expand.grid, c(lapply(presencecombos[pc,], function(x) if(x) 0 else 1:2), list(stringsAsFactors=FALSE)))
		indexes <- vapply(seq_len(nrow(ardims)), function(i){
		  do.call(`[`, c(list(testarr), lapply(ardims[i,], function(x) if(x==0) 1:2 else x)))
		}, numeric(2^sum(presencecombos[pc,])))
		stopifnot(length(indexes)==ncomb && all(testarr %in% indexes))
		arrin <- paste0('[c(',apply(indexes,2,paste,collapse=','),'),p]')

		catapp('\n\n\t# Partial observations (', paste0(testcols, ': ', c('Missing','Recorded')[presencecombos[pc,]+1], collapse=', '), '; N=', sum(tabdata),'):\n\t')
		resid_monitors <- c(resid_monitors, writeobs(tcode, tcomb, sum(tabdata)))
	}

	nsum <- nsum + sum(apply(is.na(testdata[,testcols]),1,all))
	stopifnot(nsum == nrow(testdata))

	if(populations_using){
	  catapp('\n\n\t#data# PopulationsUsing')
	}

	## Main probability calculations based on the total number of tests:

	cat('\n\n\n\t## Observation probabilities:\n\n\tfor(p in PopulationsUsing){\n\n', sep='', file=outfile, append=TRUE)

	pasteargs <- c(list('\t\t# Probability of observing '), as.list(teststrings), 'from a true positive:')
	pasteargs <- c(pasteargs, list('\n\t\tse_prob[', 1:ncomb, ',p] <- '))
	pasteargs <- c(pasteargs, list(apply(outcomes, 1, function(x){
		text <- ifelse(x==1, paste0('se[', 1:ncomb, ']'), paste0('(1-se[', 1:ncomb, '])'))
		return(paste(text, collapse='*'))
	})))
	pasteargs <- c(pasteargs, lapply(seq_len(nrow(testpairs)), function(x){
	  if(is.na(testpairs[["Active_Se"]][x])) return("")
	  ss <- testpairs[["Suffix"]][x]
	  ifelse(testagree[,paste0('cc',ss)], paste0(' +covse', ss), paste0(' -covse', ss))
	 }))

	pasteargs <- c(pasteargs, list('\n\t\t# Probability of observing '), as.list(teststrings), 'from a true negative:')
	pasteargs <- c(pasteargs, list('\n\t\tsp_prob[', 1:ncomb, ',p] <- '))
	pasteargs <- c(pasteargs, list(apply(outcomes, 1, function(x){
		text <- ifelse(x==1, paste0('(1-sp[', 1:ncomb, '])'), paste0('sp[', 1:ncomb, ']'))
		return(paste(text, collapse='*'))
	})))
	pasteargs <- c(pasteargs, lapply(seq_len(nrow(testpairs)), function(x){
	  if(is.na(testpairs[["Active_Sp"]][x])) return("")
	  ss <- testpairs[["Suffix"]][x]
	  ifelse(testagree[,paste0('cc',ss)], paste0(' +covsp', ss), paste0(' -covsp', ss))
	}))

	#pasteargs <- c(pasteargs, list('\n\t\tconstraint_ok[', 1:ncomb, ',p] <- ifelse(se_prob[', 1:ncomb, ',p] >= 0 && se_prob[', 1:ncomb, ',p] <= 1 && sp_prob[', 1:ncomb, ',p] >= 0 && sp_prob[', 1:ncomb, ',p], 1, 0)'))
	#pasteargs <- c(pasteargs, list('\n\t\tAccept[', 1:ncomb, ',p] ~ dbern(constraint_ok[',1:ncomb,',p])'))
	# pasteargs <- c(pasteargs, list('\n\n\t\t# Ensure that the covariance terms do not result in an invalid probability:\n\t\tAcceptProb[', 1:ncomb, ',p] ~ dbern(constraint_ok[ifelse(se_prob[', 1:ncomb, ',p] >= 0 && se_prob[', 1:ncomb, ',p] <= 1 && sp_prob[', 1:ncomb, ',p] >= 0 && sp_prob[', 1:ncomb, ',p], 1, 0))'))

	cat(do.call('paste', c(pasteargs, list(sep=''))), sep='\n\n', file=outfile, append=TRUE)

	catapp('\n\t\t# Ensure that the covariance terms do not result in an invalid probability:\n\t\tfor(c in 1:', ncomb, '){\n\t\t\tAcceptProb[c,p] ~ dbern(ifelse(se_prob[c,p] >= 0 && se_prob[c,p] <= 1 && sp_prob[c,p] >= 0 && sp_prob[c,p] <= 1, 1, 0))\n\t\t}\n')

	## TODO:  incorporate delta terms correctly - I guess the signs should be inverted??
	if(ppp_values){
	  catapp('\n\t\t# Calculate PPP-values for external assessment of additional tests:\n\t\tfor(c in 1:', ncomb, '){\n\t\t\t# The probability of being positive given this combination of test results\n\t\t\t# (simple application of Bayes theorem anagolous to PPV = TP / (TP + FP))\n\t\t\tse_ppp[c,p] <- prev[p] * se_prob[c,p]\n\t\t\tsp_ppp[c,p] <- (1-prev[p]) * sp_prob[c,p]\n\t\t\tppp[c,p] <- se_ppp[c,p] / (se_ppp[c,p] + sp_ppp[c,p])\n\t\t}\n')
	}

	cat('\n\t}', sep='', file=outfile, append=TRUE)


	## Check observed positives for each test:

	## TODO: separate agreement and single test, and implement overall agreement/obsprev
	if(agreement_check || single_check){

	  ## TODO: fix pairns so we don't need a minimum of 1 if there are no pairs in that pop by skipping irrelevant indexes in the loop:

	  catapp('\n\n\n\t## Model fit checks:\n\n\tfor(p in PopulationsUsing){\n\n\t\t# Single test check:\n\t\tfor(t in 1:', ntests, '){\n\t\t\tsim_single[t,p] ~ dbinom(prev[p]*se[t] + (1-prev[p])*(1-sp[t]), N_single[t,p])\n\t\t\tcheck_single[t,p] <- Tally_single[t,p] - sim_single[t,p]\n\t\t}\n\n\t\t# Pairwise crude agreement check:\n\t\tfor(c in 1:', ntestpairs, '){\n\t\t\tsim_pair[1:4,c,p] ~ dmulti(prob_pair[1:4,c,p], N_pair[c,p])\n\t\t\tcheck_pair[c,p] <- Agreement[c,p] - sum(sim_pair[c(1,4),c,p]) / N_pair[c,p]\n\t\t}\n\n\t')

	  ## Overall i.e. across all pops:
	  #tal_sin <- vapply(testcols, function(x){
	  #  c(sum(testdata[[x]]=="1", na.rm=TRUE), sum(!is.na(testdata[[x]])))
	  #}, numeric(2L))
	  #dlist <- list(Tally_single=tal_sin[1,,drop=TRUE], N_single=tal_sin[2,,drop=TRUE])
	  # datablock <- c(datablock, dump.format(dlist))

	  ## By population:
	  tab <- vapply(testcols, function(x){
	    as.matrix(table(testdata[,c(x,'Population')], useNA="no"))
	  }, matrix(0L, nrow=2, ncol=npop))
	  dlist <- list(Tally_single=t(tab[2L,,,drop=TRUE]), N_single=t(apply(tab,c(2,3),sum)))
	  datablock <- c(datablock, dump.format(dlist))

	  # Then use test pair matrix to get covariance names:
	  agreements=pairns <- matrix(NA_integer_, ncol=npop, nrow=nrow(testpairs))
	  for(c in seq_len(nrow(testpairs))){
	    cov_str_se <- if(is.na(testpairs[["Active_Se"]][c])) rep('',4) else paste0(' ', c('+','-','-','+'), 'covse', testpairs[["Suffix"]][c])
	    cov_str_sp <- if(is.na(testpairs[["Active_Sp"]][c])) rep('',4) else paste0(' ', c('+','-','-','+'), 'covsp', testpairs[["Suffix"]][c])
	    catapp(paste('\tprob_pair[', 1:4, ',', c, ',p] <- (prev[p] * ((', c('1-','','1-',''), 'se[', as.numeric(testpairs[["Test_A"]][c]), '])*(', c('1-','1-','',''), 'se[', as.numeric(testpairs[["Test_B"]][c]), '])', cov_str_se, ')) + ((1-prev[p]) * ((', c('','1-','','1-'), 'sp[', as.numeric(testpairs[["Test_A"]][c]), '])*(', c('','','1-','1-'), 'sp[', as.numeric(testpairs[["Test_B"]][c]), '])', cov_str_sp, '))', sep='', collapse='\n\t'), '\n\t')

	    ## By population:
	    tab <- table(testdata[,c(as.character(testpairs[["Test_A"]][c]), as.character(testpairs[["Test_B"]][c]), "Population")], useNA="no")
	    agreements[c,] <- apply(tab, 3, function(x) sum(x[c(1,4)])/sum(x))
	    pairns[c,] <- apply(tab,3,sum)

	  }
	  catapp('}')

	  ## TODO: fix dirty hack!
	  pairns[pairns==0] <- 1

	  dlist <- list(Agreement=agreements, N_pair=pairns)
	  datablock <- c(datablock, dump.format(dlist))
	}


	## Priors:

	cat('\n\n\n\t## Priors:\n', sep='', file=outfile, append=TRUE)

	for(p in seq_len(npop)){
		cat('\n\t# Prevalence in population ', levels(testdata$Population)[p], ':\n\tprev[', p, '] ~ ', prev_priors[p], '\n', sep='', file=outfile, append=TRUE)
	}

	cat('\n',file=outfile, append=TRUE)

	for(t in 1:length(testcols)){
		cat('\n\t# Sensitivity of ', testcols[t], ' test:\n\tse[', t, '] ~ ', se_priors[t], '\n', sep='', file=outfile, append=TRUE)
		cat('\t# Specificity of ', testcols[t], ' test:\n\tsp[', t, '] ~ ', sp_priors[t], '\n', sep='', file=outfile, append=TRUE)
	}
	catapp('\n\t# Ensure that label switching does not occur for any test:\n\tfor(t in 1:', length(testcols), '){\n\t\tAcceptTest[t] ~ dbern(ifelse((se[t]+sp[t]) >= 1.0, 1, 0))\n\t}')

	cat('\n',file=outfile, append=TRUE)

	for(t in seq_len(nrow(testpairs))){

	  if(!is.na(testpairs[["Active_Se"]][t])){

	    i1 <- testpairs[["Test_AI"]][t]
	    i2 <- testpairs[["Test_BI"]][t]

	    catapp('\n\t# Conditional dependence between ', testpairs[["Test_A"]][t], ' and ', testpairs[["Test_B"]][t], ' (sensitivity):\n\t')
	    if(!testpairs[["Active_Se"]][t]) catapp('# ')
	    ## Note: the middle of the prior is NOT a covariance of zero, so inits are needed!
	    catapp('covse', testpairs[["Suffix"]][t], ' ~ dunif(-1, 1)  ## if the sensitivity of these tests may be correlated\n\t')
	    catapp('# covse', testpairs[["Suffix"]][t], ' ~ dunif( (se[',i1,']-1)*(1-se[',i2,']) , min(se[',i1,'],se[',i2,']) - se[',i1,']*se[',i2,'])  ## alternative prior (may require setting initial values)\n\t')
	    if(testpairs[["Active_Se"]][t]) catapp('# ')
	    catapp('covse', testpairs[["Suffix"]][t], ' <- 0  ## if the sensitivity of these tests can be assumed to be independent\n\t')
	    catapp('# Calculated relative to the pairwise min/max:\n\t', 'corse', testpairs[["Suffix"]][t], ' <- ifelse(covse', testpairs[["Suffix"]][t], ' < 0, -covse', testpairs[["Suffix"]][t], ' / ((se[',i1,']-1)*(1-se[',i2,'])), covse', testpairs[["Suffix"]][t], ' / (min(se[',i1,'],se[',i2,']) - se[',i1,']*se[',i2,']))\n')

	    catapp('\n\t# Conditional dependence between ', testpairs[["Test_A"]][t], ' and ', testpairs[["Test_B"]][t], ' (specificity):\n\t')
	    if(!testpairs[["Active_Sp"]][t]) catapp('# ')
	    catapp('covsp', testpairs[["Suffix"]][t], ' ~ dunif(-1, 1)  ## if the specificity of these tests may be correlated\n\t')
	    ## Note: the middle of the prior is NOT a covariance of zero, so inits are needed!
	    catapp('# covsp', testpairs[["Suffix"]][t], ' ~ dunif( (sp[',i1,']-1)*(1-sp[',i2,']) , min(sp[',i1,'],sp[',i2,']) - sp[',i1,']*sp[',i2,'])  ## alternative prior (may require setting initial values)\n\t')
	    if(testpairs[["Active_Sp"]][t]) catapp('# ')
	    catapp('covsp', testpairs[["Suffix"]][t], ' <- 0  ## if the specificity of these tests can be assumed to be independent\n\t')
	    catapp('# Calculated relative to the pairwise min/max:\n\t', 'corsp', testpairs[["Suffix"]][t], ' <- ifelse(covsp', testpairs[["Suffix"]][t], ' < 0, -covsp', testpairs[["Suffix"]][t], ' / ((sp[',i1,']-1)*(1-sp[',i2,'])), covsp', testpairs[["Suffix"]][t], ' / (min(sp[',i1,'],sp[',i2,']) - sp[',i1,']*sp[',i2,']))\n')

	  }

	  if(cov_as_cor){
	    warning("The cov_as_cor option is now deprecated and ignored (i.e. always interpreted as FALSE)")
	    cov_as_cor <- FALSE
	  }

	  # if(cov_as_cor){
	  #   i1 <- testcombos[t,1]
	  #   i2 <- testcombos[t,2]
	  #   cat('\n\t# Correlation in sensitivity between ', testcols[i1], ' and ', testcols[i2], ' tests:\n\t', if(!covon) '# ', 'corse', paste(c(i1,i2), collapse=''), ' ~ dunif(-1, 1)  ## if the sensitivity of these tests may be correlated\n\t', if(covon) '# ', 'corse', paste(c(i1,i2), collapse=''), ' <- 0 ## if the sensitivity of these tests can be assumed to be independent\n\t', '# Corresponding covariance calculation:\n\t', 'covse', paste(c(i1,i2), collapse=''), ' <- ifelse(corse', paste(c(i1,i2), collapse=''), ' < 0, -corse', paste(c(i1,i2), collapse=''), ' * ((se[',i1,']-1)*(1-se[',i2,'])), corse', paste(c(i1,i2), collapse=''), ' * (min(se[',i1,'],se[',i2,']) - se[',i1,']*se[',i2,']))\n\t', sep='', file=outfile, append=TRUE)
	  #   cat('\n\t# Correlation in specificity between ', testcols[i1], ' and ', testcols[i2], ' tests:\n\t', if(!covon) '# ', 'corsp', paste(c(i1,i2), collapse=''), ' ~ dunif(-1, 1)  ## if the specificity of these tests may be correlated\n\t', if(covon) '# ', 'corsp', paste(c(i1,i2), collapse=''), ' <- 0  ## if the specificity of these tests can be assumed to be independent\n\t', '# Corresponding covariance calculation:\n\t', 'covsp', paste(c(i1,i2), collapse=''), ' <- ifelse(corsp', paste(c(i1,i2), collapse=''), ' < 0, -corsp', paste(c(i1,i2), collapse=''), ' * ((sp[',i1,']-1)*(1-sp[',i2,'])), corsp', paste(c(i1,i2), collapse=''), ' * (min(sp[',i1,'],sp[',i2,']) - sp[',i1,']*sp[',i2,']))\n\t', sep='', file=outfile, append=TRUE)
	  # }else{
	  #   i1 <- testcombos[t,1]
	  #   i2 <- testcombos[t,2]
	  #
	  #   cat('\n\t# Covariance in sensitivity between ', testcols[i1], ' and ', testcols[i2], ' tests:\n\t', if(!covon) '# ', 'covse', paste(c(i1,i2), collapse=''), ' ~ dunif( (se[',i1,']-1)*(1-se[',i2,']) , min(se[',i1,'],se[',i2,']) - se[',i1,']*se[',i2,'] )  ## if the sensitivity of these tests may be correlated\n\t', if(covon) '# ', 'covse', paste(c(i1,i2), collapse=''), ' <- 0  ## if the sensitivity of these tests can be assumed to be independent\n', sep='', file=outfile, append=TRUE)
	  #   cat('\t# Calculated relative to the min/max for ease of interpretation:\n\t', 'corse', paste(c(i1,i2), collapse=''), ' <- ifelse(covse', paste(c(i1,i2), collapse=''), ' < 0, -covse', paste(c(i1,i2), collapse=''), ' / ((se[',i1,']-1)*(1-se[',i2,'])), covse', paste(c(i1,i2), collapse=''), ' / (min(se[',i1,'],se[',i2,']) - se[',i1,']*se[',i2,']))\n', sep='', file=outfile, append=TRUE)
	  #
	  #   cat('\n\t# Covariance in specificity between ', testcols[i1], ' and ', testcols[i2], ' tests:\n\t', if(!covon) '# ', 'covsp', paste(c(i1,i2), collapse=''), ' ~ dunif( (sp[',i1,']-1)*(1-sp[',i2,']) , min(sp[',i1,'],sp[',i2,']) - sp[',i1,']*sp[',i2,'] )  ## if the specificity of these tests may be correlated\n\t', if(covon) '# ', 'covsp', paste(c(i1,i2), collapse=''), ' <- 0  ## if the specificity of these tests can be assumed to be independent\n', sep='', file=outfile, append=TRUE)
	  #   cat('\t# Calculated relative to the min/max for ease of interpretation:\n\t', 'corsp', paste(c(i1,i2), collapse=''), ' <- ifelse(covsp', paste(c(i1,i2), collapse=''), ' < 0, -covsp', paste(c(i1,i2), collapse=''), ' / ((sp[',i1,']-1)*(1-sp[',i2,'])), covsp', paste(c(i1,i2), collapse=''), ' / (min(sp[',i1,'],sp[',i2,']) - sp[',i1,']*sp[',i2,']))\n', sep='', file=outfile, append=TRUE)
	  #
	  # }
	}

	cat('\n}\n', sep='', file=outfile, append=TRUE)


	## Monitors:
	#cat('\n#monitor# se, sp, prev', apply(expand.grid(c('covse','corse','covsp','corsp'), apply(testcombos,1,paste,collapse='')),1,paste,collapse=''), sep=', ', file=outfile, append=TRUE)
	catapp('\n#monitor# se, sp, prev')
	if(any(!is.na(testpairs[["Active_Se"]]))){
	  catapp(paste0(", covse",testpairs[["Suffix"]][!is.na(testpairs[["Active_Se"]])]))
	  catapp(paste0(", corse",testpairs[["Suffix"]][!is.na(testpairs[["Active_Se"]])]))
	}
	if(any(!is.na(testpairs[["Active_Sp"]]))){
	  catapp(paste0(", covsp",testpairs[["Suffix"]][!is.na(testpairs[["Active_Sp"]])]))
	  catapp(paste0(", corsp",testpairs[["Suffix"]][!is.na(testpairs[["Active_Sp"]])]))
	}
	if(residual_check){
    catapp(resid_monitors)
	}
	if(agreement_check){
	  catapp(", check_single, check_pair")
	}
	if(ppp_values){
	  catapp(", ppp")
	}

	## Initial values:
	alternate <- function(x,len){
		x <- rep(x,times=ceiling(len/length(x)))
		return(x[1:len])
	}
	if(cov_as_cor){
	  cvn <- apply(expand.grid(apply(testcombos,1,paste,collapse=''), c('corse','corsp'))[,2:1],1,paste,collapse='')
	}else{
	  cvn <- apply(expand.grid(apply(testcombos,1,paste,collapse=''), c('covse','covsp'))[,2:1],1,paste,collapse='')
	}
	# Fails to initialise with anything other than 0:
	covinitvals <- as.list(alternate(c(0,0), length(cvn)))
	names(covinitvals) <- cvn
	covinits <- c(dump.format(covinitvals), dump.format(lapply(covinitvals, function(x) -x)))
	if(FALSE){
	  covinits <- gsub('\"cov', '# \"cov', covinits, fixed=TRUE)
	  covinits <- gsub('\"cor', '# \"cor', covinits, fixed=TRUE)
	}

	initblock <- c(dump.format(list(se=alternate(c(0.5,0.99), length(testcols)), sp=alternate(c(0.99,0.75), length(testcols)), prev=alternate(c(0.05,0.95), length(levels(testdata$Population)))))) #, covinits[1])
	cat('\n\n## Inits:\ninits{\n', initblock, '}', sep='', file=outfile, append=TRUE)
	initblock <- c(dump.format(list(se=alternate(c(0.5,0.99)[2:1], length(testcols)), sp=alternate(c(0.99,0.75)[2:1], length(testcols)), prev=alternate(c(0.05,0.95)[2:1], length(levels(testdata$Population)))))) #, covinits[2])
	cat('\ninits{\n', initblock, '}', sep='', file=outfile, append=TRUE)

	## Data:

	cat('\n\n## Data:\ndata{\n', datablock, '}\n\n', sep='', file=outfile, append=TRUE)

	cat('The model and data have been written to', outfile, 'in the current working directory\n')
	if(populations_using) cat('You will need to create a numeric vector of populations to include in the model within your R working environment, e.g.:\n\tPopulationsUsing <- seq_len(', length(levels(testdata$Population)), ')\n', sep='')
	cat('*** NOTE: The template provided should be examined and modified (including checking ***\n***   priors, covariance terms, and verifying the code) before running the model!   ***\n')

}


