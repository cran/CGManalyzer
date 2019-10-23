
CONGA.fn <- function(y, Interval = 5, n=2) {
#***************************************************************************
# Function to calculate the continuous overlapping net glycemic action (CONGA). 
# CONGA:  
#  For each observation after the first n hours of observations, the difference 
#  between the current observation and the observation n hours previous was calculated. 
#  CONGAn is defined as the standard deviation of the differences.
# Input 
#   y: measured response, must be evenly spaced in measured time
#   Interval: number of minutes between two consecutive time points
#   n: the length of a segment in CONGA 
# Output
#   a value of CONGA
# Author: Xiaohua Douglas Zhang, Dandan Wang
# Date:   2017-12-12
#****************************************************************************

	Npoint.segment <- 60/Interval*n
	N <- length(y)-Npoint.segment
	diff.vec <- rep(NA, N)
	for( i in 1:N ) {
		diff.vec[i] <- y[i+Npoint.segment] - y[i]
	}
	CONGA <- sd( diff.vec, na.rm=TRUE)
	return(CONGA)
}
