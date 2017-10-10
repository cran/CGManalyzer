summaryCGM.fn <-
function(dataFolder, dataFiles, responseNames, sensorIDs, columnNames=NULL,
					  skip=0, header=TRUE, comment.char="", sep=",")
{
#***************************************************************************
# Function to calculate the summary statistics for each subject or sensor:
#      number of subjects or sensors, minimum, 1st quartile, median, mean,
#      2nd quartile, maximum, standard deviation, MAD
# skip: number of lines to be skip in data file when using read.table
# Author: Xiaohua Douglas Zhang
# Date:   2017-06-06
#****************************************************************************
	nFile = length(dataFiles)
	Nresponse = length(responseNames)
	Nsensor = length(sensorIDs)
	summary.arr = array( NA, dim=c(Nsensor, 10, Nresponse) )

	for( iFile in 1:nFile ) {
		print(paste(iFile, "th file") )
		data.df0 = read.table(paste(dataFolder, dataFiles[iFile], sep="/"),
							  skip=skip, header=header,comment.char=comment.char, sep=sep)
		if( !header ) {
			data.df0=data.df0[, 1:length(columnNames)]
			dimnames(data.df0)[[2]] = columnNames
		}
		for( k in 1:length(responseNames) ) {
			Y = data.df0[, responseNames[k]]
			if( sum(is.na(Y)) ==0) s.vec=c(summary(Y), 0) else s.vec=summary(Y)
			summary.arr[iFile,,k] = c( length(Y), s.vec, sd(Y, na.rm=TRUE), mad(Y, na.rm=TRUE) )
		}
	}
	dimnames(summary.arr)=
	  list( "SensorID" = sensorIDs,
            "SummaryStat"=c("N", "Min", "Q1", "Median", "Mean", "Q3", "Max", "nNA", "SD", "MAD"),
			"Response"=responseNames)
	return(summary.arr)
}
