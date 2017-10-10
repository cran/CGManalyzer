equalInterval.fn <-
function(x, y, Interval = NA, minGap = 4*Interval) {
#***************************************************************************
# Function to derive the data with equal interval
# Input 
#   x: time sequence
#   y: measured response
#   Interval: interval indicating equal space between two consecutive points
#   minGap: the length of a chain of continuous missing values in which the 
#           missing values will not be derived from the neighbor points
# Output
#   a matrix with equally spaced time sequence and corresponding signal value
# Author: Xiaohua Douglas Zhang
# Date:   2017-05-26
#****************************************************************************
   if( length(table( diff(x) )) == 1 ) {
       warning("The data have already had equal intervals between any two consecutive points. No adjustment!")
	   xNew <- x; yNew <- y
   }
   x <- x[ !is.na(y) ]
   y <- y[ !is.na(y) ]  
   xNew <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), by = Interval)
   yNew <- rep( NA, length(xNew) )
   for( j in 1:length(xNew) ) {
     theX <- xNew[j]
	 condt1 <- x <= theX; condt2 <- x >= theX
	 if( sum(condt1, na.rm=TRUE)>0 & sum(condt2, na.rm=TRUE)>0 ) { #exclude edge points
		i1 <- max(which(x <= theX), na.rm=TRUE)
		i2 <- min(which(x >= theX), na.rm=TRUE)
		if( i2 - i1 <= minGap) {                            #exclude long chain of missing value
			x1 <- x[i1]; x2 <- x[i2]
			y1 <- y[i1]; y2 <- y[i2]
		    if( x1 == x2 ) { yNew[j] <- (y1+y2)/2 } else {
				yNew[j] <- y1 + (y2-y1)/(x2-x1)*(theX-x1)
			}
		}
	 }
   }
   return(cbind("timeSeries"=xNew, "signal"=yNew))
}
