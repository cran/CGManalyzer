boxplotCGM.fn <-
function(dataFolder, dataFiles, idxNA=NA, responseName, sensorIDs, columnNames=NULL,
		 yRange, skip=0, header=TRUE, comment.char="", sep=",", cex.axis1=0.75)
{
#########################################################################################
# function to draw an antenna plot
#****************************************************************************************
# Input:
#   dataFolder:  name for the folder for holding data
#   dataFiles:   name of the data file to be read in R
#   idxNA:       symbol to represent a missing value, such as NA
#   responseName: name to represent the response to be analyzed, such as 'glucose'
#   sensorIDs:   names of sensors or subjects
#   columnNames: names of columns of the data after reading in R
#   rRange:      range of y-axis to be drawn in the boxplot
#   skip:        number of lines to be skipped in each data file when the data is read in R
#   header, comment.char, sep:    the same meaning as in read.table()
#   cex.axis1:   cex for the x-axis
# Output:
#    a box plot for the data by each sensor or subject
# Author: Xiaohua Douglas Zhang
# Date: 2017-06-29
#****************************************************************************************

	nFile <- length(dataFiles)
	plot( c(1,nFile), yRange, xlab="", ylab=responseName, type="n", axes=FALSE)
	axis(2); box()
	axis(1, at=1:nFile, labels=sensorIDs, las=2, cex.axis=cex.axis1)
	for( iFile in 1:nFile ) {
		    print(paste(iFile, "th file") )
		data.df0 <- read.table(paste(dataFolder, dataFiles[iFile], sep="/"),
							  skip=skip, header=header, comment.char=comment.char, sep=sep)
		if( !header ) {
			data.df0 <- data.df0[, 1:length(columnNames)]
			dimnames(data.df0)[[2]] <- columnNames
		}
		if( !is.na(idxNA) ) data.df0[ data.df0[, responseName]==idxNA, responseName] <- NA
		Y <- data.df0[, responseName]
		boxplot( Y, at=iFile, add=TRUE )
	}
}
