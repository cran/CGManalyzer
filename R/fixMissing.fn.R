fixMissing.fn <-
function( y, x, Method=c("skip","linearInterpolation", "loess", "dayCycle"), OBScycle = 24*60*60/10 ) {
#*****************************************************************************************
# Function to fix missing values in a vector 
# Input
#   y: a vector of data with missing values 
#   x: a vector for a series of consecutive time indices
#   Method: method options for fixing missing value
#    "skip": skip all missing values
#    "loess": use local fitting by loess()
#    "dayCycle":a missing value is replaced by the mean of the two values one day ahead 
#       and one day behind plus the mean of the differences between the two edge points  
#       and their corresponding means of the two values one day head and one day behind
#       in a segment with missing values in which the missing value belongs to.  
#    OBScycle: number of observations in a full cycle
# Output: 
#    newY: a vector of data from 'y'  but with missing values fixed
# Author: 
#    Xiaohua Douglas Zhang, Ph.D., ASA Fellow
# Date:
#    December 22, 2015
#    updated, February 2017
#*****************************************************************************************
    newY <- y; newX <- x
	names(y) <- 1:length(y)
	a <- rle( is.na(y) ) 
	nNA <- a$lengths[ a$values == 1 ]
	if( sum(nNA) ==0 ) { 
		print( "Warning: no missing value") 
	} else if (sum(nNA) == length(y) ) {
		print( "Warning: all missing values") 	
	} else {
		idxNA <- as.numeric( names(nNA) )
		if( Method == "skip" | Method == "Skip" | Method == "SKIP" ) {
			newY <- y[ !is.na(y) ]
			newX <- x[ !is.na(y) ]
		} else if ( Method == "linearInterpolation" | Method == "loess" ) { 
		    idxStart <- ifelse( idxNA[1]-nNA[1]==1, idxNA[1], 1 )
			idxEnd <-  ifelse( sum(is.na(idxNA))==1, length(y) - nNA[length(nNA)], length(y)) 
		    yy <- y[idxStart:idxEnd]
		    xx <- x[idxStart:idxEnd]	
####  using neighborhoood linear interpolation
			if( Method == "linearInterpolation") { 
				newY[idxStart:idxEnd] <- approx(xx, yy, xout=xx, method = "linear")$y
			} else {
####  using "loess"  # hard to control the smoothness 			
				a.df <- data.frame("xx"=xx, "yy"=yy)
				yPredict <- predict(loess(yy~xx), data=a.df, data.frame("xx"=xx)  )
				yy[is.na(yy)] <- yPredict[is.na(yy)]
				newY[idxStart:idxEnd] <- yy 			
			}
		} else if( Method == "dayCycle") {  			 
####  using day cycle
		    # treat the case where the last segment of missing values is in the end of y
			if( sum(is.na(idxNA)) == 1 ) idxNA[is.na(idxNA)] <- length(y)+1
            # idxNA = idxNA[ !is.na[idxNA] ]
			for( k in 1:length(idxNA) ) {
				theIdxNAs <- idxNA[k]-nNA[k]:1
				segment1 <- theIdxNAs-OBScycle
				segment1 <- c(segment1[1]-1, segment1, segment1[length(segment1)]+1)
				segment2 <- theIdxNAs+OBScycle
				segment2 <- c(segment2[1]-1, segment2, segment2[length(segment2)]+1)
				if( segment1[1] >= 1 & segment2[length(segment2)] <= length(y) ) {
                #   fix the missing value in the middle days
					base.mat <- rbind( y[segment1], y[segment2] )
					theValue <- apply(base.mat, 2, mean, na.rm=TRUE)
				} else if( segment1[1] < 1 & segment2[length(segment2)] <= length(y)) {
                #   fix the missing value in the first days			
					theValue <- y[segment2]
				} else if( segment2[length(segment2)] > length(y) ) {
                #   fix the missing value in the last days			
					theValue <- y[segment1]
				} else { stop("Warning: need at least data for 3 days") }
				adjValue <- mean(c(y[theIdxNAs[1]-1] - theValue[1], 
								y[theIdxNAs[length(theIdxNAs)]+1] - theValue[length(theIdxNAs)]),
								na.rm=TRUE )
				theValue <- theValue + adjValue
				newY[theIdxNAs] <- theValue[ 2:(length(theValue)-1) ]
			}
		} else { stop( "'Method' has to be 'skip', 'linearInterpolation', 'loess' or 'dayCycle'") }
	} 
	return( cbind("x"=newX, "signal"=newY) )
}
