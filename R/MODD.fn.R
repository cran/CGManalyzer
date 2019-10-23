
MODD.fn <- function(y, Interval = 5) {
#***************************************************************************
# Function to calculate the mean of daily differences (MODD). 
# MODD:
#   The absolute value of the difference between glucose values taken
#   on two consecutive days at the same time was calculated; the MODD is 
#   the mean of these differences.
# Input 
#   y: measured response, must be evenly spaced in measured time
#   Interval: number of minutes between two consecutive time points
# Output
#   a value of MODD
# Author: Xiaohua Douglas Zhang
# Date:   2017-12-12
#****************************************************************************

	Nsegment <- floor(length(y)*Interval/24)
	Npoint <- 24/Interval
	if( Nsegment < 2) {
		MODD <- NA
	    warning("To be able to calculate MODD, the number of full days must be at least 2!")
	} else {
		diff.mat <- matrix( NA, nrow=Nsegment-1, ncol= Npoint)
		for( i in 1:(Nsegment-1) ) {
				diff.mat[i, ] <- y[i*Npoint+1:Npoint] - y[(i-1)*Npoint+1:Npoint] 
		}
		MODD <- mean( abs(as.vector(diff.mat)), na.rm=TRUE )	
	}
	return( MODD )
}
