MSEplot.fn <-
function(Scale, MSE, Name, responseName=NA, timeUnit="", byGroup=TRUE, MSEsd=NA, N=NA, stdError=TRUE,  
		 xRange=NA, yRange=NA, las=2, col=NA, pch=NA, Position="topleft", cex.legend=0.75, main="")
{
################################################################################################
# function to plot the mean and standard error or standard deviation of multiscale entropy by group 
#***********************************************************************************************
# Input
#   Scale: a vector for scale
#   MSE:   matrix for entropy if byGroup=FALSE, and otherwise for average entropy value in a group 
#          at a scale. In the matrix, the row is for scale and column for individuals or groups.
#   Name:  vector of names for groups
#   responseName: name to represent the response to be analyzed, such as 'glucose'
#   timeUnit: the time unit for scale. 
#   byGroup: If byGroup = TRUE, multiscale entropy is plotted by groups; otherwise, by individuals
#   MSEsd: matrix for standard deviation of entropy value in a group at a scale
#   N: matrix for number of subjects in a group at a scale
#   stdError: if it is true, the length of a vertical bar represent 2*standard error; 
#             otherwise, the length of a vertical bar represent 2*standard deviation
#   xRange: range for the x-axis
#   yRange: range for the y-axis
#   las:    las for the y-axis
#   col:    vector for the colors to indicate groups or individuals
#   pch:    vector for the point types to indicate groups or individuals
#   Position: position for the legend
#   cex.legend: cex for the legend
#   main:     main title for title()
# Output
#   a figure for plotting entropy vs. scale 
# Author: Xiaohua Douglas Zhang
# Date: 2017-06-30
#***********************************************************************************************
	nScale <- length(Scale)
	K <- dim(MSE)[2]	
	if( is.na(xRange[1]) ) xRange <- range( Scale )
	if( is.na(col[1]) ) col <- 1:K
	if( is.na(pch[1]) ) pch <- 1:K
	if( is.na(yRange[1]) ) {
		yRange <- switch(byGroup+1, range( MSE, na.rm=TRUE ),
						switch(stdError+1, 
						        range( c(range(MSE+MSEsd, na.rm=TRUE), range(MSE-MSEsd, na.rm=TRUE)) ),
								range( c(range(MSE+MSEsd/sqrt(N), na.rm=TRUE), 
								         range(MSE-MSEsd/sqrt(N), na.rm=TRUE)) )
							  )
					   )
    }
	plot( xRange, yRange, type="n", axes=FALSE, xlab=paste0("Scale factor in ", timeUnit), 
			ylab= paste0("Sample entropy of ", responseName) )
	axis(1, at=Scale, labels=Scale); axis(2, las=las); box()
	for( i in 1:K ) {
		points( Scale, MSE[,i], col=col[i], pch=pch[i]  )
		lines( Scale, MSE[,i], col=col[i]  )
		if( byGroup ) {   #draw error bar if byGroup = TRUE
			x.vec <- Scale
			if( stdError ) {
				y1.vec <- MSE[,i]-MSEsd[,i]/sqrt(N[,i])
				y2.vec <- MSE[,i]+MSEsd[,i]/sqrt(N[,i])			
			} else {
				y1.vec <- MSE[,i]-MSEsd[,i]
				y2.vec <- MSE[,i]+MSEsd[,i]		
			}
			segments(x.vec, y1.vec, x.vec, y2.vec, col=col[i])
		}
	}
	legend(Position, legend=Name, col=col, cex=cex.legend, lty=1, pch=pch)
	title(main=main)	
}
