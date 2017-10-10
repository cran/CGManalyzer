plotTseries.fn <-
function(x, y, xAt=NA, xLab=NA, yRange=NA, Frame=TRUE, xlab="", ylab="",
						  pch=1, lty=1, col.point=1, col.line=1, cex.point=1, lwd=1) 
{
################################################################################################
# function to plot time series data
#***********************************************************************************************
# Input
#   x: time in continuous value such as in seconds or minutes, (e.g. the return from timeSeqConversion.fn)
#   y: measured response value
#   Frame: whether the plot frame should be drawn
# Author: Xiaohua Douglas Zhang
# Date: 2017-05-24
#***********************************************************************************************
	if( is.na(yRange[1]) ) yRange <- range(y, na.rm=TRUE)
	if( Frame == TRUE ) {
		plot(range(x, na.rm=TRUE), yRange, type="n", axes=F,  xlab=xlab, ylab=ylab)
		axis(2, las=2); box()
		if( sum(is.na(xAt))!=0 | sum(is.na(xLab))!=0 ) {
			warning("Time is in the smallest unit since 'xAt' or 'xLab' is not provided.")
			axis(1)
		} else { axis(1, at = xAt, labels = xLab) }
	}
	lines(x, y, lty=lty, col=col.line, lwd=lwd) 
	points(x, y, pch=pch, col=col.point, cex=cex.point)
}
