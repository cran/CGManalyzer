antennaPlot.fn <-
function(Mean, SSMD, Name, CIlower, CIupper, xRange=NA, yRange=NA, col=1:length(Mean), 
		 pch=1:length(Mean), cex=1, Position="topleft", main="") 
{
#########################################################################################
# function to draw an antenna plot 
#****************************************************************************************
# Input: 
#   Mean: vector for mean difference in a comparison
#   SSMD: vector for ssmd in a comparison
#   Name: vector for name of pairs in a comparison
#   CIlower: vector for the lower bound of confidence interval
#   CIupper: vector for the upper bound of confidence interval
#   col: vector of colors for pairs in a comparison
#   pch: vector of point types for pairs in a comparison
#   main: main for title()
#   Position: position for the legend
# Output:
#    an antenna plot
# Author: Xiaohua Douglas Zhang
# Date: 2017-06-29
#****************************************************************************************

	if( is.na(xRange[1]) ) xRange <- range( c( range( CIlower, na.rm=TRUE), 0, 
											   range( CIupper, na.rm=TRUE) )  )
	if( is.na(yRange[1]) ) yRange <- range( c(0, SSMD), na.rm=TRUE )
	plot( xRange, yRange, type="n", axes=FALSE, 
		  xlab="Mean of difference and its confidence interval", ylab= "SSMD" )
	axis(1); axis(2, las=2); box()
	lines( rep(0, 2), yRange, col="grey", lty=2 )
	lines( xRange, rep(0, 2), col="grey", lty=2 ) 
	N <- length(Mean) 	
	for(i in 1:N) {	
		points( Mean[i], SSMD[i], col=col[i], pch=pch[i] ) 
		lines(  c(CIlower[i], CIupper[i]), rep(SSMD[i], 2), col=col[i]  )
	}
	COEF <- lm( SSMD ~ Mean - 1, na.action="na.exclude")$coef
	YY <- range(SSMD, na.rm=T)
	lines( c(0,YY/COEF), c(0, YY) )
	legend(Position, legend=Name, col=col, pch=pch, cex=cex, lty=1)
	title(main=main)
}
