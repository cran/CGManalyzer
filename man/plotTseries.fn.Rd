\name{plotTseries.fn}
\alias{plotTseries.fn}

\title{
 function to plot time series data
}
\description{
 function to plot time series data
}
\usage{
plotTseries.fn(x, y, xAt = NA, xLab = NA, yRange = NA, Frame = TRUE, xlab = "",
ylab = "", pch = 1, lty = 1, col.point = 1, col.line = 1, cex.point = 1, lwd = 1)
}

\arguments{
  \item{x}{
time in continuous value such as in seconds or minutes, (e.g. the return from timeSeqConversion.fn)
}
  \item{y}{
measured response value
}
  \item{xAt}{
a vector to indicate where the labels in the x-axis are
}
  \item{xLab}{
a vector to indicate what the labels in the x-axis are
}
  \item{yRange}{
range for y in the plot
}
  \item{Frame}{
whether the plot frame should be drawn
}
  \item{xlab}{
as in plot()
}
  \item{ylab}{
as in plot()
}
  \item{pch}{
as in plot()
}
  \item{lty}{
as in plot()
}
  \item{col.point}{
the color for the points
}
  \item{col.line}{
the color for the line
}
  \item{cex.point}{
cex for the points
}
  \item{lwd}{
as in plot()
}
}
\details{
function to plot time series data
}

\references{
Zhang XD, Zhang Z. CGManalyzer: a R package for analyzing continuous glucose monitoring studies
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
data.df <- data.frame( timeStamp.vec, Time.mat[,1], data.df0[,responseName] )
dimnames(data.df)[[2]] <- c("timeStamp", "timeSeries", responseName)
data.df <- data.df[ order(data.df[, "timeSeries"]), ]
plotTseries.fn(	x=data.df[, "timeSeries"], y=data.df[, responseName],
				xAt=0:14*720, xLab=0:14/2, yRange=NA, Frame=TRUE,
				xlab="Time in Days", ylab=responseName, pch=1, lty=1,
				col.point=1, col.line=1, cex.point=0.5, lwd=1 )
}

