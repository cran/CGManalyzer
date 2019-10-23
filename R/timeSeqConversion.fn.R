timeSeqConversion.fn <-
function(time.stamp, time.format = "yyyy:mm:dd:hh:nn", timeUnit = "minute") 
{
#***************************************************************************************************
# function to convert a matrix (with columns for year, month, day, minute and/or second)
#    to a time sequence in a unit of minute or second
# Input
#   time.stamp: a vector for the time stamp. It can have any format such as "2016:08:11:09:14:00", 
#      "11/08/2016 09:14:00" and others. The requirement is simply that the positions for year, month, 
#      day, hour, minute, second are fixed and consistent in all data files and "0" before a non-zero 
#      number cannot be skipped. 
#   time.format: a string to specify the format in time.stamp in a study, such as "yyyy:mm:dd:hh:nn:ss:ii",
#      "dd/mm/yyyy hh:nn:ss:ii" and others which must have 'y' for year, 'm' for month, 'd' for day,  
#      'h' for hour, 'n' for minute, 's' for second, 'i' for millisecond(one thousandth of a second), 
#      each uniquely
#   timeUnit: minimal time unit in time.stamp. can be 'minute' or 'second'
# Output: a matrix with 6 columns for timeseries, year, month, day, hour and minute, respectively
# Author: Xiaohua Douglas Zhang
# Date: 2017-05-25
#****************************************************************************************

    yearPos <- gregexpr(pattern ='y', time.format)[[1]]	
	monthPos <- gregexpr(pattern ='m', time.format)[[1]]	
    dayPos <- gregexpr(pattern ='d', time.format)[[1]]	
    hourPos <- gregexpr(pattern ='h', time.format)[[1]]	
    minutePos <- gregexpr(pattern ='n', time.format)[[1]]	
    secondPos <- gregexpr(pattern ='s', time.format)[[1]]	
    millisecondPos <- gregexpr(pattern ='i', time.format)[[1]]	
	pos.lst <- list( "Year"=yearPos, "Month"=monthPos, "Day"=dayPos, "Hour"=hourPos, 
	                "Minute"=minutePos, "Second"=secondPos, "Millisecond"=millisecondPos)
					
	pos.name <- names(pos.lst)
	Time.mat <- NULL
	for( i in 1:length(pos.lst) ) {
	  thePos <- pos.lst[[i]]
      Time <- as.matrix( as.numeric(substr( time.stamp, min(thePos), max(thePos) )))
	  dimnames(Time)[[2]] <- list(pos.name[i])
	  if(thePos[1] != -1) Time.mat <- cbind(Time.mat, Time)
	}
	
    minYear <- min(Time.mat[, "Year"], na.rm=TRUE)
	if( timeUnit == "minute" | timeUnit == "Minute" | timeUnit == "MINUTE" ) {
		date.vec <- as.Date( paste(Time.mat[, "Year"], substring(Time.mat[, "Month"]+1000, 3,4), 
						   substring(Time.mat[, "Day"]+1000, 3,4), sep="/") )
		days.vec <- as.numeric( date.vec - as.Date(paste( minYear, "01", "01", sep="/")) )
		Tsequence.vec <- days.vec*24*60 + Time.mat[, "Hour"]*60 + Time.mat[, "Minute"] 
		Tsequence.vec <- Tsequence.vec - min(Tsequence.vec, na.rm=TRUE)
	} else if( timeUnit == "second" | timeUnit == "Second" | timeUnit == "SECOND" ) {
		date.vec <- as.Date( paste(Time.mat[, "Year"], substring(Time.mat[, "Month"]+1000, 3,4), 
						   substring(Time.mat[, "Day"]+1000, 3,4), sep="/") )
		days.vec <- as.numeric( date.vec - as.Date(paste( minYear, "01", "01", sep="/")) )
		Tsequence.vec <- days.vec*24*60*60 + Time.mat[,"Hour"]*60*60 + Time.mat[,"Minute"]*60 + Time.mat[,"Second"] 
		Tsequence.vec <- Tsequence.vec - min(Tsequence.vec, na.rm=TRUE)	
	} else {
	    warning("'timeUnit' must be in minute or second." )
		Tsequence.vec <- rep(NA, dim(Time.mat)[2])
	}
	return( cbind("timeSequence"=Tsequence.vec, Time.mat) )
}
