pairwiseComparison.fn <-
function(y, INDEX, na.rm=TRUE, conf.level=0.95)
{
################################################################################################
# function to calculate mean difference and its confidence interval, SSMD, p-value of t.test for 
#   pairwise comparison 
#***********************************************************************************************
# Input
#   y:     response value
#   INDEX: vector for group names 
#   na.rm: whether to remove value for calculation
#   conf.level: confidence level for two-sided t-test
# Output
#   a vector for calculated mean difference, its upper and lower bounds of CI, SSMD and pvalue 
#   in each pairs of group comparison, along with mean, standard deviation, and sample size 
#   in each group
# Author: Xiaohua Douglas Zhang
# Date: 2017-06-30
#***********************************************************************************************

	Types <- sort( unique(INDEX) )
	nType <- length(Types)
	nPair <- nType*(nType-1)/2
	Means <- tapply( y, INDEX=INDEX, mean, na.rm=na.rm )
	SDs <- tapply( y, INDEX=INDEX, sd, na.rm=na.rm )
	Ns <- tapply( y, INDEX=INDEX, function(x) {sum(!is.na(x))} )
	meanD.vec <- CIlower.vec <- CIupper.vec <- SSMD.vec <- pval.vec <- rep(NA, nPair)
	idx <- 0
	coreNames <- rep(NA, nPair)
	for(j in 1:(nType-1) ) {
		for(k in (j+1):nType ) {
			idx <- idx+1
			coreNames[idx] <- paste(Types[j], "vs", Types[k], sep=".")
			meanD.vec[idx] <- Means[j] - Means[k]
			condt1 <- INDEX==Types[j]; condt2 <- INDEX==Types[k]
			if( sum(condt1, na.rm=TRUE)>1 & sum(condt2, na.rm=TRUE)>1 ) {
				SSMD.vec[idx] <- meanD.vec[idx]/sqrt(SDs[j]^2 + SDs[k]^2)
				Ttest.lst <- t.test(y[condt1], y[condt2], conf.level=conf.level)
				pval.vec[idx] <- Ttest.lst$p.value 
				CIlower.vec[idx] <- Ttest.lst$conf.int[1]
				CIupper.vec[idx] <- Ttest.lst$conf.int[2]
			}
		}
	}		
	results <- c(meanD.vec, CIlower.vec, CIupper.vec, SSMD.vec, pval.vec, Means, SDs, Ns)
	outNames <- 
		c( paste0("mDiff_",coreNames), paste0("CIlower_",coreNames), paste0("CIupper_",coreNames),
		   paste0("SSMD_", coreNames), paste0("p_", coreNames), paste0("mean_", Types), 
		   paste0("SD_", Types), paste0("N_", Types) )
	names(results) <- outNames  
	return(results)
}
