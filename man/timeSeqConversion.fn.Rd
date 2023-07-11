\name{timeSeqConversion.fn}
\alias{timeSeqConversion.fn}

\title{
function to convert a matrix (with columns for year, month, day, minute and/or second)
to a time sequence in a unit of minute or second
}
\description{
function to convert a matrix (with columns for year, month, day, minute and/or second)
to a time sequence in a unit of minute or second
}
\usage{
timeSeqConversion.fn(time.stamp, time.format = "yyyy:mm:dd:hh:nn", timeUnit = "minute")
}

\arguments{
  \item{time.stamp}{
a vector for the time stamp. It can have any format such as "2016:08:11:09:14:00",  "11/08/2016 09:14:00" and others. The requirement is simply that the positions for year, month, day, hour, minute, second are fixed and consistent in all data files and "0" before a non-zero number cannot be skipped.
}
  \item{time.format}{
a string to specify the format in time.stamp in a study, such as "yyyy:mm:dd:hh:nn:ss:ii", "dd/mm/yyyy hh:nn:ss:ii" and others which must have 'y' for year, 'm' for month, 'd' for day, 'h' for hour, 'n' for minute, 's' for second, 'i' for millisecond(one thousandth of a second), each uniquely
}
  \item{timeUnit}{
minimal time unit in time.stamp. can be 'minute' or 'second'
}
}
\details{
function to convert a matrix (with columns for year, month, day, minute and/or second)
to a time sequence in a unit of minute or second
}
\value{
 a matrix with 6 columns for timeseries, year, month, day, hour and minute, respectively
}
\references{
Zhang XD, Zhang Z, Wang D. 2018. CGManalyzer: an R package for analyzing continuous glucose monitoring studies. Bioinformatics 34(9): 1609-1611 (DOI: 10.1093/bioinformatics/btx826).
}
\author{
Xiaohua Douglas Zhang
}

\examples{
library(CGManalyzer)
package.name <- "CGManalyzer"
source( system.file("SPEC", "SPECexample.R", package = package.name) )
data.df0 <- read.table(paste(dataFolder, dataFiles[1], sep="/"),
            skip=Skip, header=Header, comment.char=Comment.char, sep=Sep)
if( !Header ) {
	data.df0 <- data.df0[, 1:length(columnNames)]
    dimnames(data.df0)[[2]] <-  columnNames
}
if( !is.na(idxNA) ) data.df0[ data.df0[, responseName] == idxNA, responseName] <- NA
for( i in 1:length(timeStamp.column) ) {
	if(i==1) { timeStamp.vec <- data.df0[, timeStamp.column[i] ] } else {
		 timeStamp.vec <- paste0(timeStamp.vec, " ", data.df0[, timeStamp.column[i] ])
	}
}
Time.mat <- timeSeqConversion.fn(time.stamp=timeStamp.vec, time.format=time.format,
            timeUnit=timeUnit)
Time.mat[1:6,]
}

