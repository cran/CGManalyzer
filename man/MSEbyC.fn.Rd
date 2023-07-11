\name{MSEbyC.fn}
\alias{MSEbyC.fn}

\title{
call a C function to calculate multiscale entropy
}
\description{
Function to call a C function to calculate multiscale entropy (MSE)
of an equally spaced time series.
}
\usage{
MSEbyC.fn(x, scaleMax = 10, scaleStep = 1, mMin = 2, mMax = 2, mStep = 1, rMin = 0.15,
rMax = 0.15, I = 400000)
}

\arguments{
  \item{x}{
A numeric vector, with data for a regularly spaced time series. No missing value is allowed because the C program is not set up to handle missing value.
}
  \item{scaleMax}{
maximal value of scale factors for coarse graining in the MSE algorithm. The scale factors are a sequence from 1 to a value no more than 'scaleMax' with equal space 'scaleStep'.  Scale factors are positive integers that specify bin size for coarse graining: the number of consecutive observations in 'x' that form a bin and are averaged in the first step of the algorithm.
}
  \item{scaleStep}{
see 'scaleMax'
}
  \item{mMin}{
A sequence from 'mMin' to 'mMax' with equal space of 'mStep' that defines the vector of positive integers that give the window size for the entropy calculations in the second step of the algorithm:  the number of consecutive _bins_ over which similarity between subsequences is of interest.  Typical values in the sequence are 1, 2, or 3.
}
  \item{mMax}{
See 'Min'
}
  \item{mStep}{
See 'Min'
}
  \item{rMin}{
  A sequence from 'rMin' to 'rMax' with equal space of 0.05 that defines  coefficients for similarity thresholds. Typical values in the sequence are 0.15, 0.2.  r*sd(x) must be in the same units as 'x'. Averages in two bins are defined to be similar if they differ by 'r*sd(x)' or less.
}
  \item{rMax}{
See 'rMin'
}
  \item{I}{
the maximal number of points to be used for calculating MSE cFolder: The directory in which .c is held as well as in which temporary files associated with running C are created/removed.
}
}
\details{
Function to call a C function to calculate multiscale entropy (MSE)
of an equally spaced time series.
}
\value{
A data frame with with one row for each combination of 'Scale', 'm' and 'rSD'.  Columns are "Scale", "m", "rSD", and "SampEn" (the calculated sample entropy). The data frame will also have an attribute "SD", the standard deviation of 'x'. rSD = r*sd(x)
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
data.df <- data.frame( timeStamp.vec, Time.mat[,1], data.df0[,responseName] )
dimnames(data.df)[[2]] <- c("timeStamp", "timeSeries", responseName)
data.df <- data.df[ order(data.df[, "timeSeries"]), ]
data.mat <- data.df[, c("timeSeries", responseName)]
data.mat <- data.mat[!is.na(data.mat[,2]), ]
MSE.mat <- MSEbyC.fn(data.mat[,2], scaleMax, scaleStep, mMin=m, mMax=m, mStep=1,
          rMin=r, rMax=r, I=I)
MSE.mat
}
